############### Data Cleaning and Combination #################
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
