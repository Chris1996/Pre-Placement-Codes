# Data loading script

library(weatherData)
library(ggplot2)
library(lubridate)
library(plyr)


# Initial download from WeatherData
if (0) {
  heathrow.download <- getWeatherForDate("EGLL", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
  heathrow.download2 <- heathrow.download[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
  write.table(heathrow.download2, file = "Processed/rowDataH.tsv", sep = "\t", row.names = F)
  
  dundee.download <- getWeatherForDate("EGPN", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
  dundee.download2 <- dundee.download[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
  write.table(dundee.download2, file = "Processed/rowDataD.tsv", sep = "\t", row.names = F)
}


# Read data from files
heathrow <- read.table( "Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F) #heathrow weather data
heathrow$Time <- as.POSIXct( heathrow$Time)
heathrow <- rename(heathrow, c("TemperatureC" = "Temperature", 
                               "Sea_Level_PressurehPa" = "Pressure", 
                               "Wind_SpeedKm_h" = "Wind.Speed", 
                               "WindDirDegrees" = "Wind.Direction"))

dundee <- read.table( "Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F) #dundee weather data
dundee$Time <- as.POSIXct( dundee$Time)
dundee <- rename(dundee, c("TemperatureC" = "Temperature", 
                           "Sea_Level_PressurehPa" = "Pressure", 
                           "Wind_SpeedKm_h" = "Wind.Speed", 
                           "WindDirDegrees" = "Wind.Direction"))

