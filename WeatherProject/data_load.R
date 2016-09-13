# data loading script

library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")


#initial download from WeatherData
#Heathrow1 <- getWeatherForDate("EGLL", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Heathrow2 <- Heathrow1[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Heathrow2, file = "Processed/rowDataH.tsv", sep = "\t", row.names = F)

#Dundee1 <- getWeatherForDate("EGPN", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Dundee2 <- Dundee1[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Dundee2, file = "Processed/rowDataD.tsv", sep = "\t", row.names = F)


#Read data from file
Heathrow <- read.table( "Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F) #heathrow weather data
Heathrow$Time <- as.POSIXct( Heathrow$Time)

Dundee <- read.table( "Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F) #dundee weather data
Dundee$Time <- as.POSIXct( Dundee$Time)


#cleaning data
CleanData <- function( Data, WeatherArg) {
  # Remove NAs and uneccessary values
  # Args: Data = a data frame or data table to be cleaned
  #       WeatherArg = Vector indicating the columns of the data to be cleaned
  MyCleanData <- Data[ , WeatherArg]
  if ( WeatherArg[ 2] == "TemperatureC") {
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Temperature), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Temperature == -9999, ]
  }
  if ( WeatherArg[ 2] == "Humidity") {
    MyCleanData$Humidity <- as.numeric( MyCleanData$Humidity)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Humidity), ]
  }
  if ( WeatherArg[ 2] == "Sea_Level_PressurehPa") {
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Sea_Level_PressurehPa), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Sea_Level_PressurehPa == -9999, ]
    MyCleanData <- MyCleanData[ !MyCleanData$Sea_Level_PressurehPa == 0, ]
  }
  if ( WeatherArg[ 2] == "Wind_SpeedKm_h") {
    MyCleanData$Wind_SpeedKm_h <- as.numeric( MyCleanData$Wind_SpeedKm_h)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Wind_SpeedKm_h), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Wind_SpeedKm_h == -9999, ]
  }
  if ( WeatherArg[ 2] == "Events") {
    MyCleanData$Events <- factor( MyCleanData$Events)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Events), ]
  }
  if ( WeatherArg [ 2] == "WindDirDegrees") {
    MyCleanData$WindDirDegrees <- as.numeric( MyCleanData$WindDirDegrees)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$WindDirDegrees), ]
    MyCleanData <- MyCleanData[ MyCleanData$WindDirDegrees <= 360, ]
  }
  return(MyCleanData)
}



