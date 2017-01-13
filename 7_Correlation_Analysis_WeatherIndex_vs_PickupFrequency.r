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
       
# open plot object
par(ps = 30, cex = 1, cex.main = 1, mar=c(5,5,5,5), mfrow=c(3,1))

# Cross Correlation Functions between Average Temperature and Daily Pickup Frequency
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
