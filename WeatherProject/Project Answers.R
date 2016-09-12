library(weatherData)
library(ggplot2)
library(data.table)
library(lubridate)

#Heathrow <- getWeatherForDate("EGLL", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Heathrow2 <- Heathrow[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Heathrow2, file = "Processed/rowDataH.tsv", sep = "\t", row.names = F)

#Dundee <- getWeatherForDate("EGPN", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
#Dundee2 <- Dundee[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
#write.table(Dundee2, file = "Processed/rowDataD.tsv", sep = "\t", row.names = F)

HeathrowRead <- read.table("Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F)
HeathrowRead$Time <- as.POSIXct(HeathrowRead$Time)


DundeeRead <- read.table("Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F)
DundeeRead$Time <- as.POSIXct(DundeeRead$Time)


HeathrowTemp <- HeathrowRead[,c("Time", "TemperatureC")] #isolate time and temperature columns
HeathrowTemp <- HeathrowTemp[!HeathrowTemp$TemperatureC == -9999,] #remove temperature values at -9999
HeathrowTemp$Date <- date(HeathrowTemp$Time)
HeathrowTemp$Week <- week(HeathrowTemp$Time) #need to differentiate between years
HeathrowTemp$Month <- month(HeathrowTemp$Time) #need to differentiate between years
HeathrowDailyTempMeans <- aggregate(cbind("DailyTempMean" = TemperatureC) ~ Date, data = HeathrowTemp, FUN = mean)
HeathrowDailyTempMedians <- aggregate(cbind("DailyTempMedian" = TemperatureC) ~ Date, data = HeathrowTemp, FUN = median)
HeathrowWeeklyTempMeans <- aggregate(cbind("WeeklyTempMean" = TemperatureC) ~ Week, data = HeathrowTemp, FUN = mean)
HeathrowWeeklyTempMedians <- aggregate(cbind("WeeklyTempMedian" = TemperatureC) ~ Week, data = HeathrowTemp, FUN = median)
HeathrowMonthlyTempMeans <- aggregate(cbind("MonthlyTempMean" = TemperatureC) ~ Month, data = HeathrowTemp, FUN = mean)
HeathrowMonthlyTempMedians <- aggregate(cbind("MonthlyTempMedian" = TemperatureC) ~ Month, data = HeathrowTemp, FUN = median)

DundeeTemp <- DundeeRead[,c("Time", "TemperatureC")] #isolate time and temperature columns
DundeeTemp <- DundeeTemp[!DundeeTemp$TemperatureC == -9999,] #remove temperature values at -9999
DundeeTemp$Date <- date(DundeeTemp$Time)
DundeeTemp$Week <- week(DundeeTemp$Time)
DundeeTemp$Month <- month(DundeeTemp$Time)
DundeeDailyTempMeans <- aggregate(cbind("DailyTempMean" = TemperatureC) ~ Date, data = DundeeTemp, FUN = mean)
DundeeDailyTempMedians <- aggregate(cbind("DailyTempMedian" = TemperatureC) ~ Date, data = DundeeTemp, FUN = median)
DundeeWeeklyTempMeans <- aggregate(cbind("WeeklyTempMean" = TemperatureC) ~ Week, data = DundeeTemp, FUN = mean)
DundeeWeeklyTempMedians <- aggregate(cbind("WeeklyTempMedian" = TemperatureC) ~ Week, data = DundeeTemp, FUN = median)
DundeeMonthlyTempMeans <- aggregate(cbind("MonthlyTempMean" = TemperatureC) ~ Month, data = DundeeTemp, FUN = mean)
DundeeMonthlyTempMedians <- aggregate(cbind("MonthlyTempMedian" = TemperatureC) ~ Month, data = DundeeTemp, FUN = median)

ggplot() +  #ggplot for heathrow/dundee daily temperature means
  geom_point(data = HeathrowDailyTempMeans, color = "blue",aes(Date,DailyTempMean)) +
  geom_point(data = DundeeDailyTempMeans, color = "red",aes(Date,DailyTempMean))

ggplot() +  #ggplot for heathrow/dundee daily temperature medians
  geom_point(data = HeathrowDailyTempMedians, color = "blue",aes(Date,DailyTempMedian)) +
  geom_point(data = DundeeDailyTempMedians, color = "red",aes(Date,DailyTempMedian))

ggplot() +  #ggplot for heathrow/dundee weekly temperature means
  geom_point(data = HeathrowWeeklyTempMeans, color = "blue",aes(Week,WeeklyTempMean)) +
  geom_point(data = DundeeWeeklyTempMeans, color = "red",aes(Week,WeeklyTempMean))

ggplot() +  #ggplot for heathrow/dundee weekly temperature medians
  geom_point(data = HeathrowWeeklyTempMedians, color = "blue",aes(Week,WeeklyTempMedian)) +
  geom_point(data = DundeeWeeklyTempMedians, color = "red",aes(Week,WeeklyTempMedian))

ggplot() +  #ggplot for heathrow/dundee monthly temperature means
  geom_point(data = HeathrowMonthlyTempMeans, color = "blue",aes(Month,MonthlyTempMean)) +
  geom_point(data = DundeeMonthlyTempMeans, color = "red",aes(Month,MonthlyTempMean))

ggplot() +  #ggplot for heathrow/dundee monthly temperature medians
  geom_point(data = HeathrowMonthlyTempMedians, color = "blue",aes(Month,MonthlyTempMedian)) +
  geom_point(data = DundeeMonthlyTempMedians, color = "red",aes(Month,MonthlyTempMedian))



table(DundeeDailyTempMeans$Date %in% HeathrowDailyTempMeans$Date)
#returns all true
table(HeathrowDailyTempMeans$Date %in% DundeeDailyTempMeans$Date)
#returns 7 false, the rest true

HeathrowDailyTempMeans2 <- HeathrowDailyTempMeans[HeathrowDailyTempMeans$Date %in% DundeeDailyTempMeans$Date,]
#remove dates that only have temp values in Heathrow

table(DundeeDailyTempMeans$Date == HeathrowDailyTempMeans2$Date) #check all dates are same and in the same position

cor(x = DundeeDailyTempMeans$DailyTempMean, y = HeathrowDailyTempMeans2$DailyTempMean) #calculate correlation between daily temperature means
#returns [1] 0.8977393


HeathrowPressure <- HeathrowRead[,c("Time", "Sea_Level_PressurehPa")] #isolate time and pressure columns
table(is.na(HeathrowPressure$Sea_Level_PressurehPa)) #check for Nas in the pressure column
HeathrowPressure <- HeathrowPressure[!is.na(HeathrowPressure$Sea_Level_PressurehPa),]
HeathrowPressure <- HeathrowPressure[!HeathrowPressure$Sea_Level_PressurehPa == -9999,]
HeathrowPressure <- HeathrowPressure[!HeathrowPressure$Sea_Level_PressurehPa == 0,]
HeathrowPressure$Date <- date(HeathrowPressure$Time)
HeathrowDailyPressureMeans <- aggregate(cbind("DailyPressureMean" = Sea_Level_PressurehPa) ~ Date, data = HeathrowPressure, FUN = mean)

DundeePressure <- DundeeRead[,c("Time", "Sea_Level_PressurehPa")] #isolate time and pressure columns
table(is.na(DundeePressure$Sea_Level_PressurehPa)) #check for Nas in the pressure column
DundeePressure <- DundeePressure[!is.na(DundeePressure$Sea_Level_PressurehPa),]
DundeePressure <- DundeePressure[!DundeePressure$Sea_Level_PressurehPa == -9999,]
DundeePressure$Date <- date(DundeePressure$Time)
DundeeDailyPressureMeans <- aggregate(cbind("DailyPressureMean" = Sea_Level_PressurehPa) ~ Date, data = DundeePressure, FUN = mean)


table(DundeeDailyPressureMeans$Date %in% HeathrowDailyPressureMeans$Date)
#returns all true
table(HeathrowDailyPressureMeans$Date %in% DundeeDailyPressureMeans$Date)
#returns 7 false, the rest true

HeathrowDailyPressureMeans2 <- HeathrowDailyPressureMeans[HeathrowDailyPressureMeans$Date %in% DundeeDailyPressureMeans$Date,]
#remove dates that only have pressure values in Heathrow

table(DundeeDailyPressureMeans$Date == HeathrowDailyPressureMeans2$Date) #check all dates are same and in the same position

cor(x = DundeeDailyPressureMeans$DailyPressureMean,y = HeathrowDailyPressureMeans2$DailyPressureMean) #calculate correlation between daily humidity means
#returns [1] 0.8330252





HeathrowHumidity <- HeathrowRead[,c("Time","Humidity")] #isolate time and humidity columns
table(is.na(HeathrowHumidity$Humidity)) #check for Nas in the humidity column
HeathrowHumidity <- HeathrowHumidity[!is.na(HeathrowHumidity$Humidity),] #remove nas
HeathrowHumidity$Date <- date(HeathrowHumidity$Time)
HeathrowHumidity$Humidity <- as.numeric(HeathrowHumidity$Humidity) #coerce to numeric ie "80" becomes 80
HeathrowHumidity <- HeathrowHumidity[!is.na(HeathrowHumidity$Humidity),]
HeathrowDailyHumidityMeans <- aggregate(cbind("DailyHumidityMean" = Humidity) ~ Date, data = HeathrowHumidity, FUN = mean)


DundeeHumidity <- DundeeRead[,c("Time","Humidity")] #isolate time and humidity columns
table(is.na(DundeeHumidity$Humidity)) #check for Nas in the humidity column
DundeeHumidity <- DundeeHumidity[!is.na(DundeeHumidity$Humidity),] #remove nas
DundeeHumidity$Date <- date(DundeeHumidity$Time)
DundeeHumidity$Humidity <- as.numeric(DundeeHumidity$Humidity) #coerce to numeric ie "80" becomes 80
DundeeHumidity <- DundeeHumidity[!is.na(DundeeHumidity$Humidity),]
DundeeDailyHumidityMeans <- aggregate(cbind("DailyHumidityMean" = Humidity) ~ Date, data = DundeeHumidity, FUN = mean)


table(DundeeDailyHumidityMeans$Date %in% HeathrowDailyHumidityMeans$Date)
#returns all true
table(HeathrowDailyHumidityMeans$Date %in% DundeeDailyHumidityMeans$Date)
#returns 7 false, the rest true

HeathrowDailyHumidityMeans2 <- HeathrowDailyHumidityMeans[HeathrowDailyHumidityMeans$Date %in% DundeeDailyHumidityMeans$Date,]
#remove dates that only have humidity values in Heathrow

table(DundeeDailyHumidityMeans$Date == HeathrowDailyHumidityMeans2$Date) #check all dates are same and in the same position

cor(x = DundeeDailyHumidityMeans$DailyHumidityMean,y = HeathrowDailyHumidityMeans2$DailyHumidityMean) #calculate correlation between daily hummidity means
#returns [1] 0.4937412




HeathrowWindSpeed <- HeathrowRead[,c("Time", "Wind_SpeedKm_h")] #isolate time and wind speed columns
table(is.na(HeathrowWindSpeed$Wind_SpeedKm_h)) #check for Nas in the wind speed column
HeathrowWindSpeed <- HeathrowWindSpeed[!is.na(HeathrowWindSpeed$Wind_SpeedKm_h),] #remove nas
HeathrowWindSpeed$Date <- date(HeathrowWindSpeed$Time)
HeathrowWindSpeed$Wind_SpeedKm_h <- as.numeric(HeathrowWindSpeed$Wind_SpeedKm_h) #coerce to numeric ie "80" becomes 80
HeathrowWindSpeed <- HeathrowWindSpeed[!is.na(HeathrowWindSpeed$Wind_SpeedKm_h),]
HeathrowDailyWindSpeedMeans <- aggregate(cbind("DailyWindSpeedMean" = Wind_SpeedKm_h) ~ Date, data = HeathrowWindSpeed, FUN = mean)


DundeeWindSpeed <- DundeeRead[,c("Time", "Wind_SpeedKm_h")] #isolate time and wind speed columns
DundeeWindSpeed$Date <- date(DundeeWindSpeed$Time)
DundeeWindSpeed$Wind_SpeedKm_h <- as.numeric(DundeeWindSpeed$Wind_SpeedKm_h) #coerce to numeric ie "80" becomes 80
table(is.na(DundeeWindSpeed$Wind_SpeedKm_h)) #check for Nas in the wind speed column
DundeeWindSpeed <- DundeeWindSpeed[!is.na(DundeeWindSpeed$Wind_SpeedKm_h),] #remove nas
DundeeWindSpeed <- DundeeWindSpeed[!DundeeWindSpeed$Wind_SpeedKm_h == -9999,]
DundeeDailyWindSpeedMeans <- aggregate(cbind("DailyWindSpeedMean" = Wind_SpeedKm_h) ~ Date, data = DundeeWindSpeed, FUN = mean)



table(DundeeDailyWindSpeedMeans$Date %in% HeathrowDailyWindSpeedMeans$Date)
#returns all true
table(HeathrowDailyWindSpeedMeans$Date %in% DundeeDailyWindSpeedMeans$Date)
#returns 7 false, the rest true

HeathrowDailyWindSpeedMeans2 <- HeathrowDailyWindSpeedMeans[HeathrowDailyWindSpeedMeans$Date %in% DundeeDailyWindSpeedMeans$Date,]
#remove dates that only have Wind speed values in Heathrow

table(DundeeDailyWindSpeedMeans$Date == HeathrowDailyWindSpeedMeans2$Date) #check all dates are same and in the same position

cor(x = DundeeDailyWindSpeedMeans$DailyWindSpeedMean, y = HeathrowDailyWindSpeedMeans2$DailyWindSpeedMean) #calculate correlation between daily windspeed means
#returns [1] 0.4488157



HeathrowPressureRain <- HeathrowRead[,c("Time", "Events")]
#HeathrowPressureRain$Events <- factor(HeathrowPressureRain$Events)
#HeathrowPressureRain$Date <- date(HeathrowPressureRain$Time)
#HeathrowDailyRainMeans <- aggregate(cbind("DailyRainMean" = Events) ~ Date, data = HeathrowPressureRain, FUN = mean)

HeathrowDailyPressureMeans3 <- HeathrowDailyPressureMeans[HeathrowDailyPressureMeans$Date %in% HeathrowDailyRainMeans$Date, ]
cor(x = HeathrowDailyRainMeans$DailyRainMean, y = HeathrowDailyPressureMeans3$DailyPressureMean)
#returns [1] -0.3847564


DundeePressureRain <- DundeeRead[,c("Time", "Events")]
DundeePressureRain$Events <- factor(DundeePressureRain$Events)
DundeePressureRain$Date <- date(DundeePressureRain$Time)
DundeeDailyRainMeans <- aggregate(cbind("DailyRainMean" = Events) ~ Date, data = DundeePressureRain, FUN = mean)

DundeeDailyPressureMeans2 <- DundeeDailyPressureMeans[DundeeDailyPressureMeans$Date %in% DundeeDailyRainMeans$Date, ]
cor(x = DundeeDailyRainMeans$DailyRainMean, y = DundeeDailyPressureMeans2$DailyPressureMean)
#returns [1] -0.283742



HeathrowWindDir <- HeathrowRead[c("Time", "WindDirDegrees")] #isolate time and Wind Direction columns
HeathrowWindDir$Date <- date(HeathrowWindDir$Time)
HeathrowDailyWindDirMeans <- aggregate(cbind("DailyWindDirMean" = WindDirDegrees) ~ Date, data = HeathrowWindDir, FUN = mean)

HeathrowDailyTempMeans3 <- HeathrowDailyTempMeans[HeathrowDailyTempMeans$Date %in% HeathrowDailyWindDirMeans$Date, ]
HeathrowDailyWindDirMeans2 <- HeathrowDailyWindDirMeans[HeathrowDailyWindDirMeans$Date %in% HeathrowDailyTempMeans$Date, ]
cor(x = HeathrowDailyWindDirMeans2$DailyWindDirMean, y = HeathrowDailyTempMeans3$DailyTempMean)
#returns [1] -0.03342881


DundeeWindDir <- DundeeRead[c("Time", "WindDirDegrees")] #isolate time and Wind Direction columns
DundeeWindDir$Date <- date(DundeeWindDir$Time)
DundeeDailyWindDirMeans <- aggregate(cbind("DailyWindDirMean" = WindDirDegrees) ~ Date, data = DundeeWindDir, FUN = mean)

DundeeDailyTempMeans2 <- DundeeDailyTempMeans[DundeeDailyTempMeans$Date %in% DundeeDailyWindDirMeans$Date, ]
cor(x = DundeeDailyWindDirMeans$DailyWindDirMean, y = DundeeDailyTempMeans2$DailyTempMean)
#returns [1] -0.1339723



ggplot() +  #ggplot for heathrow/dundee daily pressure means
  geom_point(data = HeathrowDailyPressureMeans, color = "blue",aes(Date,DailyPressureMean)) +
  geom_point(data = DundeeDailyPressureMeans, color = "red",aes(Date,DailyPressureMean))


ggplot() +  #ggplot for heathrow/dundee daily humidity means
  geom_point(data = HeathrowDailyHumidityMeans, color = "blue",aes(Date,DailyHumidityMean)) +
  geom_point(data = DundeeDailyHumidityMeans, color = "red",aes(Date,DailyHumidityMean))


ggplot() +  #ggplot for heathrow/dundee daily wind speed means
  geom_point(data = HeathrowDailyWindSpeedMeans, color = "blue",aes(Date,DailyWindSpeedMean)) +
  geom_point(data = DundeeDailyWindSpeedMeans, color = "red",aes(Date,DailyWindSpeedMean))






HeathrowPressureRain2 <- CleanData( ReadData( "Heathrow"), c("Time", "Sea_Level_PressurehPa"))
HeathrowPressureRain2$Events <- factor( HeathrowPressureRain2$Events)
levels( HeathrowPressureRain2$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
boxplot( HeathrowPressureRain2$Sea_Level_PressurehPa ~ HeathrowPressureRain2$Events, data  = HeathrowPressureRain2)


ggplot( data = HeathrowPressureRain2[ , c( "Sea_Level_PressurehPa", "Events")], na.rm = T) +
  geom_boxplot( aes( Events, Sea_Level_PressurehPa))


ggplot( data = HeathrowPressureRain2[ , c( "Sea_Level_PressurehPa", "Events")], na.rm = T) +
  geom_density( aes( Sea_Level_PressurehPa, colour = Events))







#Improvements:
#   - use a function to save copy and pasting
#   - na.rm is an arguement to some functions. Saves subsetting with !is.na
#   - add key to ggplots to attach meaning to colours
