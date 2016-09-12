# data loading script

library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")


#initial download from WeatherData
#Heathrow <- getWeatherForDate("EGLL", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Heathrow2 <- Heathrow[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Heathrow2, file = "Processed/rowDataH.tsv", sep = "\t", row.names = F)

HeathrowRead <- read.table( "Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F) #heathrow weather data
HeathrowRead$Time <- as.POSIXct( HeathrowRead$Time)


#initial download from WeatherData
#Dundee <- getWeatherForDate("EGPN", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Dundee2 <- Dundee[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Dundee2, file = "Processed/rowDataD.tsv", sep = "\t", row.names = F)

DundeeRead <- read.table( "Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F) #dundee weather data
DundeeRead$Time <- as.POSIXct( DundeeRead$Time)
