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
