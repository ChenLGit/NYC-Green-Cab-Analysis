install.packages("ggplot2")
install.packages("ggExtra")
install.packages("ggproto")
install.packages("ggmap")
install.packages("maptools")
install.packages("RColorBrewer")
install.packages("grid")
install.packages("gridExtra")
install.packages("astsa")
install.packages("sp")
install.packages("rgdal")
install.packages("classInt")
install.packages("XML")
install.packages("sqldf")
install.packages("reshape")
#install.packages("hexbin")
#install.packages("dplyr")


library("ggplot2")
library("ggExtra")
library("ggproto")
library("ggmap")
library("maptools")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("astsa")
library("sp")
library("rgdal")
library("classInt")
library("XML")
library("sqldf")
library("reshape")
#library("hexbin")
#library("dplyr")


###################### Load data ######################

# Load data - NYC Green Taxi Data
# http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml
NYTaxi <- read.csv(
  file = "green_tripdata_2016-02.csv",
  header = TRUE, 
  col.names = c("VendorID","PickupTime","DropoffTime", "InMemory", "RateCodeID", 
                "PickupLongitude", "PickupLatitude", "DropoffLongitude", "DropoffLatitude",
                "PassengerCount", "Trip_distance", "FareAmount", "Rush_Overnight", "MTA_tax", 
                "TipAmount", "TollsAmount", "Ehail_fee", "ImprovementSurcharge", 
                "TotalAmount", "PaymentType", "TripType"))
head(NYTaxi)

# Load data - Weather Data
# https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND
weather<- read.csv(
  file = "WeatherData_V2.csv",
  header = TRUE, 
  col.names = c("STATION",	"STATION_NAME",	"ELEVATION",	"LATITUDE",	"LONGITUDE",	"DATE",	"NEW_DATE","PRCP",	
                "SNWD",	"SNOW",	"TAVG",	"TMAX",	"TMIN",	"WESD",	"WESF",	"AWND",	"WDF2",	"WDF5",	"WSF2",	
                "WSF5",	"PGTM",	"WT09",	"WT01",	"WT02",	"WT06",	"WT11",	"WT04",	"WT08"))
head(weather)


############### Data Cleaning and Combination #################
# subset: http://www.statmethods.net/management/subset.html 
# subset: http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
# http://stackoverflow.com/questions/21341089/datepart-in-sqldf

# Clean dataset
NYTaxi_Sub_Stg1 = sqldf('select *, (TipAmount/FareAmount) *100 as TipPercentage, (FareAmount/Trip_distance) as FareperMile
                      from NYTaxi 
                      where  RateCodeID < 10
                      and PickupLongitude >= -76
                      and PickupLongitude <= -72 
                      and DropoffLongitude >= -76 
                      and DropoffLongitude <= -72
                      and Trip_distance < 200
                      and FareAmount > 0 
                      and FareAmount < 50 
                      and TollsAmount > 0 
                      and TollsAmount < 100 
                      and TipAmount >0 
                      and TipPercentage < 100 
                      and PassengerCount > 0 
                      and ImprovementSurcharge > 0 
                      and TotalAmount > 0 
                      and TotalAmount < 200 
                      and Rush_Overnight <= 1') 

# Check data
sqldf('select Date(PickupTime) from NYTaxi_Sub_Stg1')
sqldf('select Date(NEW_DATE) from weather')

# Calculate Mean of weatehr indexes
weather_avg <-aggregate(weather[, 8:28], list(weather$NEW_DATE), mean, na.rm = TRUE)

# Rename Column 
colnames(weather_avg)[1] <- "Weather_Date"
head(weather_avg)

# Combine Green Taxi Dataset with Weather Dataset
#data.frame(time=format(as.POSIXct(NYTaxi_Sub$PickupTime, format="%Y-%m-%d %H:%M"), format="%H"))
NYTaxi_Sub=sqldf("select a.*, Date(PickupTime) as PickupDate, 
                              round((strftime('%H',PickupTime)),2) as PickupHour,
                              strftime('%Y-%m-%d %H',PickupTime) as PickupDateHour, b.*  
                  from NYTaxi_Sub_Stg1 a 
                  join weather_avg b
                  where Date(PickupTime) = Weather_Date")

sqldf("select distinct PickupTime, PickupDate, PickupHour, PickupDateHour from NYTaxi_Sub")
head(NYTaxi_Sub) 


################### Check Data Status ################### 
# melt: http://www.statmethods.net/management/reshape.html
# melt: http://stackoverflow.com/questions/11346880/r-plot-multiple-box-plots-using-columns-from-data-frame
# http://www.computerworld.com/article/2486425/business-intelligence/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html
# las: http://www.statmethods.net/advgraphs/axes.html

# Open a plot object

par( mar=c(8,5,5,5), mfrow=c(1,1))

# melt data
meltOriginalData <- melt(NYTaxi)
# boxplot data to check variation
boxplot(data=meltOriginalData, value~variable, las=2, cex.axis = 0.7, ylab="Data Range")

# melt data
meltSubsetData <- melt(NYTaxi_Sub_Stg1)
# boxplot data to check variation
boxplot(data=meltSubsetData, value~variable, las=2, cex.axis = 0.7, ylim = c(-100,150), ylab="Data Range")

dev.off()


#################### Trip Distance vs Tip Amount/Tip Percentage/Tip per Mile ####################
#https://www3.nd.edu/~steve/computing_with_data/11_geom_examples/ggplot_examples.html
# http://docs.ggplot2.org/0.9.3.1/stat_smooth.html

# Tip Amount vs Trip_Distance
TipAmount <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$TipAmount)) + 
  geom_jitter(size = 0.1, aes(color = Rush_Overnight)) + 
  ylab("Tip Amount") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red") 

# Tip Percentage vs Trip Distance
TipPercentage <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$TipPercentage)) + 
  geom_jitter(size = 1, aes(color = Rush_Overnight)) +
  ylab("Tip Percentage") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red", formula = y ~ poly(x, 2)) 

# Fare per Mile vs Trip Distance
FarePerMile <- ggplot(NYTaxi_Sub, aes(x = NYTaxi_Sub$Trip_distance, y = NYTaxi_Sub$FareperMile)) + 
  geom_point(size = 1, aes(color = Rush_Overnight)) +
  coord_cartesian(ylim = c(3, 10)) +
  ylab("Fare per Mile") +
  xlab("Trip Distance") +
  scale_colour_gradient(name = "Extra Charge", low = "#9BE0F9", high = "blue") +
  theme(legend.position=c(0.7,0.85)) +
  stat_smooth(method = "lm",  col = "red", formula = y ~ poly(x, 2)) 

# https://rpubs.com/MarkusLoew/13295
#grid.draw(rbind(ggplotGrob(TipAmount), ggplotGrob(TipPercentage), ggplotGrob(FarePerMile), size="first"))
# http://stackoverflow.com/questions/13712574/how-to-change-position-of-grid-draw

grid.arrange(TipAmount , TipPercentage, FarePerMile, ncol=3,
             heights=c(10, 1),widths =c(2,2,2) ,as.table =TRUE)


################### Pickup Trip Frequency based on Hour/Day #####################
# http://docs.ggplot2.org/current/geom_bar.html

# Creat Frequency Table for PickupTime
PickupTimeFreq = sqldf('select PickupDate, PickupHour, count(PickupTime) as PickupFreq
                        from NYTaxi_Sub 
                        group by 1,2')
PickupTimeDailyFreq = sqldf('select PickupDate, count(PickupTime) as PickupDailyFreq
                        from NYTaxi_Sub 
                            group by 1')

# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# color blind friendly palette:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
               "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Hourly Pickup Frequency 
HourlyPickupbyDay <- ggplot(NYTaxi_Sub, aes(PickupDateHour, fill = PickupDate)) + geom_bar(stat="count" ) + 
  scale_fill_manual(values = cbPalette)+
  #xlab("Pickup Date") +
  ylab("Pickup Frequency")+
  #coord_cartesian(ylim = c(17, 42)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        #axis.text = element_text(size = 0, angle = 45, hjust = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Daily Pickup Frequency
WeeklyTrend <- ggplot(PickupTimeDailyFreq, aes(x=PickupDate, y=PickupDailyFreq, fill = PickupDate))+ geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette)+
  xlab("Pickup Date") +
  ylab("Pickup Frequency")+
  coord_cartesian(ylim = c(200, 610)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0.5),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Hourly Pickup Frequency Trend *********
HourlyPickupTrend <- ggplot (PickupTimeFreq, aes(x=PickupHour, y=PickupFreq, colour= PickupDate, alpha = 0.8))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette)+
  xlab("Pickup Hour") +
  ylab("Pickup Frequency")+
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0),
        text = element_text(size=10),
        legend.position='bottom', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10, colour = "black", angle = 0))


# Plot on Grid 
grid.arrange(HourlyPickupbyDay , WeeklyTrend, HourlyPickupTrend, nrow = 3,
             heights=c(5,6.5, 20),widths =c(10) ,as.table =TRUE)


######################### Pickup Time vs Revenue #########################

# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Creat Hourly Daily Revenue Table 
# Hourly Revenue
PickupDailyHourlyReveue = sqldf('select PickupDateHour, PickupDate, avg(FareAmount) as PickupDHRev
                                from NYTaxi_Sub 
                                group by 1,2')

# Hourly Daily Revenue
PickupDailyHourlyRev <- ggplot(PickupDailyHourlyReveue, aes(x=PickupDateHour, y=PickupDHRev, fill = PickupDate)) + geom_bar(stat="identity") + 
  scale_fill_manual(values = cbPalette)+
  #xlab("Pickup Date") +
  ylab("Hourly Revenue")+
  coord_cartesian(ylim = c(17, 42)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        #axis.text = element_text(size = 0, angle = 45, hjust = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Creat Daily Revenue Table
PickupDailyReveue = sqldf('select PickupDate, avg(FareAmount) as PickupDRev
                          from NYTaxi_Sub 
                          group by 1')

# Daily Revenue
PickupDailyRev <- ggplot(PickupDailyReveue, aes(x=PickupDate, y=PickupDRev, fill = PickupDate))+ geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette)+
  xlab("Pickup Date") +
  ylab("Daily Revenue")+
  coord_cartesian(ylim = c(20, 25.5)) +
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0.5),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Creat Hourly Revenue Table 
PickupHourlyRevenue = sqldf('select PickupDate, PickupHour, avg(FareAmount) as PickupHRev
                            from NYTaxi_Sub 
                            group by 1,2')
# Hourly Revenue
PickupHourlyRev <- ggplot (PickupHourlyRevenue, aes(x=PickupHour, y=PickupHRev, colour= PickupDate, alpha = 0.8))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette) +
  xlab("Pickup Hour") +
  ylab("Hourly Revenue")+
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        #axis.text = element_text(size = 8, angle = 0, hjust = 0),
        text = element_text(size=10),
        legend.position='none', 
        legend.direction = "vertical",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5, colour = "black", angle = 0))

# Creat Revenue per Mile Table
PickupMileRevenue = sqldf('select PickupDate, PickupHour, avg(FareAmount)/avg(Trip_distance) as FareperMile
                            from NYTaxi_Sub 
                            group by 1,2')

# Revenue per Mile
PickupMileRev <- ggplot (PickupMileRevenue, aes(x=PickupHour, y=FareperMile, colour= PickupDate, alpha = 0.8 ))+ 
  geom_line(aes(colour = PickupDate, linetype = PickupDate, size = PickupDate)) + 
  geom_point() +
  stat_smooth(method = "lm",  formula = y ~ poly(x, 7), col = "red", level = 0.9995)+
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid",
                                 "dotdash", "dotdash", "dotdash", "dotdash", "dotdash", "solid","solid")) +
  scale_size_manual(values=c(1, 1, 1, 1, 1, 2, 2,
                             1, 1, 1, 1, 1, 2, 2)) +
  scale_colour_manual(values=cbPalette) +
  xlab("Pickup Hour") +
  ylab("Fare Amount per Mile")+
  theme(panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, angle = 0, hjust = 0),
        text = element_text(size=10),
        legend.position='bottom', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10, colour = "black", angle = 0))

grid.arrange(PickupDailyHourlyRev , PickupDailyRev, PickupHourlyRev, PickupMileRev,nrow = 4,
             heights=c(7.5,10, 13.5, 24),widths =c(10) ,as.table =TRUE)


################### Weather vs Frequency #####################

# Select Weather Data 
Weather_Frequency = sqldf('select PickupDate, TAVG, SNOW, AWND, count(PickupTime) as WeatherPickupFreq
                            from NYTaxi_Sub 
                            group by 1,2,3,4')
# Normalized Data
Normalized_Weather_Frequency = sqldf('select PickupDate,  (TAVG-TAVG_MIN)/(TAVG_MAX-TAVG_MIN) as NorTAVG,  (SNOW-SNOW_MIN)/(SNOW_MAX-SNOW_MIN) as NorSNOW, (AWND-AWND_MIN)/(AWND_MAX-AWND_MIN) as NorAWND, 
 (cast(b.WeatherPickupFreq as float)-WeatherPickupFreq_MIN)/(WeatherPickupFreq_MAX-WeatherPickupFreq_MIN) as NorWeatherPickupFreq
                                     from (select min(TAVG) as TAVG_MIN,  max(TAVG) as TAVG_MAX, min(SNOW) as SNOW_MIN,	max(SNOW) as SNOW_MAX,	min(AWND) as AWND_MIN,	max(AWND) as AWND_MAX,	min(WeatherPickupFreq) as WeatherPickupFreq_MIN,	max(WeatherPickupFreq) as WeatherPickupFreq_MAX	from Weather_Frequency )a,
                                     Weather_Frequency b 
                                     group by 1')
# Melt Data
Normalized_Weather_Frequency_melt <- melt(Normalized_Weather_Frequency, id="PickupDate")
# Creat Palette
weatherPalette = c("#0072B2", "#56B4E9", "#009E73", "#D55E00")

#http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
# http://docs.ggplot2.org/0.9.2.1/theme.html

# Correlation among Different Weather Indexes and Pickup Frequency
ggplot(data=Normalized_Weather_Frequency_melt, aes(x=PickupDate, y=value, colour=variable, group = variable)) +
  geom_line(aes(colour = variable, linetype = variable, size = variable)) + 
  geom_point(size = 3) +
  scale_colour_manual(values=weatherPalette) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash", "solid")) +
  scale_size_manual(values=c(1, 1, 1, 2)) +
  #xlab("Date") +
  ylab("Normalized Value")+
  labs(aesthetic="Normalized Value")+     
  #labs(title = "Weather Indexes vs Pickup Frequency (Normalized)") +
  theme(plot.title = element_text(size = 25, colour = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "grey92", colour = "white"),
        panel.grid.minor = element_line(colour = "white", linetype = "solid"),
        panel.grid.major = element_line(colour = "white", linetype = "solid"),
        axis.ticks = element_line(colour = "white"),
        #axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14, angle = 45, hjust = 1),
        legend.position='top', 
        legend.direction = "horizontal",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14, colour = "black", angle = 0)
       )
  
# https://onlinecourses.science.psu.edu/stat510/?q=book/export/html/74
# https://www.r-bloggers.com/setting-graph-margins-in-r-using-the-par-function-and-lots-of-cow-milk/

# Cross Correlation Functions between Average Temperature and Daily Pickup Frequency
#plot.background = element_rect()
par(ps = 30, cex = 1, cex.main = 1, mar=c(5,5,5,5), mfrow=c(3,1))
TAVG_PFrequency <- ccf(Weather_Frequency$TAVG, Weather_Frequency$WeatherPickupFreq, 
                       xlab= "Lag (Day)", ylab="ACF", main="Average Temperature vs Pickup Frequency", prob=TRUE, 
                       cex.lab=1, cex.axis=1, cex.main=1.5)
#lag2.plot (Weather_Frequency$TAVG, Weather_Frequency$WeatherPickupFreq, max.lag = 5, corr = TRUE, smooth = TRUE)

# Cross Correlation Functions between Average Snow Depth and Daily Pickup Frequency 
SNOW_PFrequency <-ccf(Weather_Frequency$SNOW, Weather_Frequency$WeatherPickupFreq,
                      xlab= "Lag (Day)", ylab="ACF", main="Average Snow Depth vs Pickup Frequency", prob=TRUE, 
                      cex.lab=1, cex.axis=1, cex.main=1.5)
#lag2.plot (Weather_Frequency$SNOW, Weather_Frequency$WeatherPickupFreq, max.lag = 5, corr = TRUE, smooth = TRUE)

# Cross Correlation Functions between Average Wind Speed and Daily Pickup Frequency 
AWND_PFrequency <-ccf(Weather_Frequency$AWND, Weather_Frequency$WeatherPickupFreq,
                      xlab= "Lag (Day)", ylab="ACF", main="Average Wind Speed vs Pickup Frequency", prob=TRUE, 
                      cex.lab=1, cex.axis=1, cex.main=1.5)
#lag2.plot (Weather_Frequency$AWND, Weather_Frequency$WeatherPickupFreq, max.lag = 5, corr = TRUE, smooth = TRUE)
# dev.off


######################## Geographical Plot of Pickup Density #######################
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html
# http://stat405.had.co.nz/ggmap.pdf
# http://docs.ggplot2.org/0.9.3.1/stat_density2d.html
# http://docs.ggplot2.org/dev/geom_density2d.html
# http://docs.ggplot2.org/0.9.2.1/scale_gradient.html
# http://docs.ggplot2.org/0.9.2.1/theme.html
# http://stackoverflow.com/questions/25409981/change-plot-title-sizes-in-a-facet-wrap-multiplot
# http://docs.ggplot2.org/0.9.3.1/facet_grid.html

# Geo Heat Map
#If runwith Error: ScalesList was built with an incompatible version of ggproto. Please reinstall ggplot2 and ggmap.
#devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")

# Change Map Color Style
theme_set(theme_bw(16))
# Get Map through Web
newyorkcity <- get_map(location = c(-73.9485,40.7447), zoom = 11, color = "bw")

# Set up map
NewYorkCity <- ggmap(newyorkcity, extent = "device", 
                     base_layer = ggplot(data = NYTaxi_Sub, aes(x = PickupLongitude, y = PickupLatitude))) 
# Plot Geo Heat Map
NewYorkCity + 
  geom_point(aes(x = PickupLongitude, y = PickupLatitude), 
             colour = "black", alpha = 0.5, size = 0.1, data = NYTaxi_Sub) +
  stat_density2d(data = NYTaxi_Sub,
                 aes(x = PickupLongitude, y = PickupLatitude, fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 180, geom = "polygon", n = 50, h=0.05) +
                 scale_fill_gradient(low = "blue", high = "red") + 
                 scale_alpha(range = c(0.05,0.08), guide = FALSE)+
  geom_density2d(data = NYTaxi_Sub, 
                 aes(x = PickupLongitude, y = PickupLatitude), 
                 colour = "yellow", size = 0.1, alpha = 0.8) + 
  theme(legend.position='top', 
        legend.direction = "horizontal",
        legend.text = element_text(size = 12, colour = "black", angle = 0),
        #legend.key.size = unit(1, "cm"),
        legend.key.width = unit(2, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_colour_brewer(name = "My Legend") +
  xlab("Day") +
  ylab("Week") + 
  facet_wrap(~PickupDate, ncol=7)

# Reference
# Reports
# http://egr.uri.edu/wp-uploads/asee2016/42-150-1-DR.pdf
# https://gist.github.com/ikovsky/c371b5822f5ecdb6add173ad1c328c97#file-green_taxi_2d_heatmap-r
# https://github.com/toddwschneider/nyc-taxi-data
# http://blog.nycdatascience.com/student-works/analysis-of-nyc-yellow-taxi-data/
# http://blog.nycdatascience.com/r/nyc-taxi-riders-tipping-behavior-analysis/
# http://toddwschneider.com/posts/analyzing-1-1-billion-nyc-taxi-and-uber-trips-with-a-vengeance/


# Combine Plots/ Facet
# https://rpubs.com/MarkusLoew/13295
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
# http://docs.ggplot2.org/0.9.3.1/facet_wrap.html
# http://docs.ggplot2.org/0.9.3.1/facet_grid.html
# http://stackoverflow.com/questions/25409981/change-plot-title-sizes-in-a-facet-wrap-multiplot


# Color
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# http://stackoverflow.com/questions/30352412/how-do-you-create-a-gradient-of-colors-for-a-discrete-variable-in-ggplot2
# 


# Intro to ggplot2
# http://seananderson.ca/ggplot2-FISH554/

# Legend 
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
# https://github.com/tidyverse/ggplot2/wiki/Legend-Attributes
# http://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot-density
# http://docs.ggplot2.org/0.9.3.1/guide_legend.html


# Background 
# http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

# Control Line Shape Color Individually 
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# http://stackoverflow.com/questions/27350243/ggplot-line-graph-with-different-line-styles-and-markers
# http://stackoverflow.com/questions/11344561/controlling-line-color-and-line-type-in-ggplot-legend



# Theme
# http://docs.ggplot2.org/0.9.2.1/theme.html

# Multiple Plot in Same Chart
# http://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
# http://stackoverflow.com/questions/21337110/ggplot-each-group-consists-of-only-one-observation
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# http://stackoverflow.com/questions/13487625/overlaying-two-graphs-using-ggplot2-in-r

# Bar Graph
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
# http://stackoverflow.com/questions/11703947/r-stacked-barchart-with-aggregate-data
# http://stackoverflow.com/questions/26833375/how-to-group-data-and-then-draw-bar-chart-in-ggplot2

# Aggregate
# https://www.r-bloggers.com/aggregate-a-powerful-tool-for-data-frame-in-r/

# Partial Graph
# http://stackoverflow.com/questions/31571413/y-starts-at-1-in-ggplot-bar-graph-in-ggplot2
# http://stackoverflow.com/questions/30631051/r-ggplot2-bar-chart-facet-wrap-with-scales-free-and-y-axis-starting-at-nonzero-v
# http://stackoverflow.com/questions/11695502/different-starting-point-not-0-in-barplot-y-axis

# Geo map
# https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
# http://gis.stackexchange.com/questions/19064/how-to-open-a-shapefile-in-r
# https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/
# http://gis.stackexchange.com/questions/19064/how-to-open-a-shapefile-in-r
# https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/
# https://www.nceas.ucsb.edu/scicomp/usecases/ReadWriteESRIShapeFiles
# https://community.tableau.com/message/483472
# https://community.tableau.com/docs/DOC-5831
# http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
# http://stat405.had.co.nz/ggmap.pdf
# https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
# http://gis.stackexchange.com/questions/48828/making-beautiful-maps-in-r
# http://archive.is/4cOS7
# http://www.molecularecologist.com/2012/09/making-maps-with-r/
# https://www.r-bloggers.com/create-maps-with-maptools-r-package/
# https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
# http://spatialanalysis.co.uk/2010/09/rmaps/
# http://spatialanalysis.co.uk/wp-content/uploads/2010/09/Maps_R_Final.pdf
# http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html
# https://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/
# http://gis.stackexchange.com/questions/179527/creating-spatial-population-heat-map
# http://stackoverflow.com/questions/25847188/geographical-heat-map-of-a-custom-property-in-r-with-ggmap
# https://pakillo.github.io/R-GIS-tutorial/
# http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
# https://www.mapbox.com/blog/binning-alternative-point-maps/
# http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/
# http://www.rpubs.com/cengel248/97543
# http://docs.ggplot2.org/0.9.3.1/stat_density2d.html

