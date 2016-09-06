library(weatherData)
library(ggplot2)
library(data.table)
library(lubridate)

Heathrow <- getWeatherForDate("EGLL", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
Heathrow2 <- Heathrow[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
write.table(Heathrow2, file = "Processed/rowDataH.tsv", sep = "\t", row.names = F)

Dundee <- getWeatherForDate("EGPN", "2014-01-01", end_date="2016-08-30",opt_all_columns = T, opt_detailed = T)
Dundee2 <- Dundee[c("Time", "TemperatureC", "Humidity", "Sea_Level_PressurehPa", "Wind_SpeedKm_h", "Events", "WindDirDegrees")]
write.table(Dundee2, file = "Processed/rowDataD.tsv", sep = "\t", row.names = F)

HeathrowRead <- read.table("Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F)
HeathrowRead$Time <- as.POSIXct(HeathrowRead$Time)


DundeeRead <- read.table("Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F)
DundeeRead$Time <- as.POSIXct(DundeeRead$Time)


HeathrowTemp <- HeathrowRead[c("Time", "TemperatureC")] #isolate time and temperature columns
HeathrowTemp <- HeathrowTemp[!HeathrowTemp[,2]==-9999,] #remove temperature values at -9999
HeathrowTemp$Date <- date(HeathrowTemp$Time)
HeathrowTemp$Week <- week(HeathrowTemp$Time)
HeathrowTemp$Month <- month(HeathrowTemp$Time)
HeathrowDailyTempMeans <- aggregate(cbind("DailyTempMean" = TemperatureC) ~ Date, data = HeathrowTemp, FUN = mean)
HeathrowDailyTempMedians <- aggregate(cbind("DailyTempMedian" = TemperatureC) ~ Date, data = HeathrowTemp, FUN = median)
HeathrowWeeklyTempMeans <- aggregate(cbind("WeeklyTempMean" = TemperatureC) ~ Week, data = HeathrowTemp, FUN = mean)
HeathrowWeeklyTempMedians <- aggregate(cbind("WeeklyTempMedian" = TemperatureC) ~ Week, data = HeathrowTemp, FUN = median)
HeathrowMonthlyTempMeans <- aggregate(cbind("MonthlyTempMean" = TemperatureC) ~ Month, data = HeathrowTemp, FUN = mean)
HeathrowMonthlyTempMedians <- aggregate(cbind("MonthlyTempMedian" = TemperatureC) ~ Month, data = HeathrowTemp, FUN = median)

DundeeTemp <- DundeeRead[,1:2] #isolate time and temperature columns
DundeeTemp <- DundeeTemp[!DundeeTemp[,2]==-9999,] #remove temperature values at -9999
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



table(DundeeDailyTempMeans[,1] %in% HeathrowDailyTempMeans[,1])
#returns all true
table(HeathrowDailyTempMeans[,1] %in% DundeeDailyTempMeans[,1])
#returns 7 false, the rest true

HeathrowDailyTempMeans2 <- HeathrowDailyTempMeans[HeathrowDailyTempMeans[,1] %in% DundeeDailyTempMeans[,1],]
#remove dates that only have temp values in Heathrow

table(DundeeDailyTempMeans[,1] == HeathrowDailyTempMeans2[,1]) #check all dates are same and in the same position

cor(x = DundeeDailyTempMeans[,2],y = HeathrowDailyTempMeans2[,2]) #calculate correlation between daily temperature means
#returns [1] 0.8977393


HeathrowPressure <- HeathrowRead[,c(1,4)] #isolate time and pressure columns
table(is.na(HeathrowPressure[,2])) #check for Nas in the pressure column
HeathrowPressure <- HeathrowPressure[!is.na(HeathrowPressure[,2]),]
HeathrowPressure$Date <- format(HeathrowPressure$Time,"%y-%m-%d")
HeathrowDailyPressureMeans <- aggregate(cbind("DailyPressureMean" = Sea_Level_PressurehPa) ~ Date, data = HeathrowPressure, FUN = mean)

DundeePressure <- DundeeRead[,c(1,4)] #isolate time and pressure columns
table(is.na(DundeePressure[,2])) #check for Nas in the pressure column
DundeePressure <- DundeePressure[!is.na(DundeePressure[,2]),]
DundeePressure$Date <- format(DundeePressure$Time,"%y-%m-%d")
DundeeDailyPressureMeans <- aggregate(cbind("DailyPressureMean" = Sea_Level_PressurehPa) ~ Date, data = DundeePressure, FUN = mean)


table(DundeeDailyPressureMeans[,1] %in% HeathrowDailyPressureMeans[,1])
#returns all true
table(HeathrowDailyPressureMeans[,1] %in% DundeeDailyPressureMeans[,1])
#returns 7 false, the rest true

HeathrowDailyPressureMeans2 <- HeathrowDailyPressureMeans[HeathrowDailyPressureMeans[,1] %in% DundeeDailyPressureMeans[,1],]
#remove dates that only have pressure values in Heathrow

table(DundeeDailyPressureMeans[,1] == HeathrowDailyPressureMeans2[,1]) #check all dates are same and in the same position

cor(x = DundeeDailyPressureMeans[,2],y = HeathrowDailyPressureMeans2[,2]) #calculate correlation between daily humidity means
#returns [1] 0.7196912





HeathrowHumidity <- HeathrowRead[,c(1,3)] #isolate time and humidity columns
table(is.na(HeathrowHumidity[,2])) #check for Nas in the humidity column
HeathrowHumidity <- HeathrowHumidity[!is.na(HeathrowHumidity[,2]),] #remove nas
HeathrowHumidity$Date <- format(HeathrowHumidity$Time,"%y-%m-%d")
HeathrowHumidity$Humidity <- as.numeric(HeathrowHumidity[,2]) #coerce to numeric ie "80" becomes 80
HeathrowHumidity <- HeathrowHumidity[!is.na(HeathrowHumidity[,2]),]
HeathrowDailyHumidityMeans <- aggregate(cbind("DailyHumidityMean" = Humidity) ~ Date, data = HeathrowHumidity, FUN = mean)


DundeeHumidity <- DundeeRead[,c(1,3)] #isolate time and humidity columns
table(is.na(DundeeHumidity[,2])) #check for Nas in the humidity column
DundeeHumidity <- DundeeHumidity[!is.na(DundeeHumidity[,2]),] #remove nas
DundeeHumidity$Date <- format(DundeeHumidity$Time,"%y-%m-%d")
DundeeHumidity$Humidity <- as.numeric(DundeeHumidity[,2]) #coerce to numeric ie "80" becomes 80
DundeeHumidity <- DundeeHumidity[!is.na(DundeeHumidity[,2]),]
DundeeDailyHumidityMeans <- aggregate(cbind("DailyHumidityMean" = Humidity) ~ Date, data = DundeeHumidity, FUN = mean)


table(DundeeDailyHumidityMeans[,1] %in% HeathrowDailyHumidityMeans[,1])
#returns all true
table(HeathrowDailyHumidityMeans[,1] %in% DundeeDailyHumidityMeans[,1])
#returns 7 false, the rest true

HeathrowDailyHumidityMeans2 <- HeathrowDailyHumidityMeans[HeathrowDailyHumidityMeans[,1] %in% DundeeDailyHumidityMeans[,1],]
#remove dates that only have humidity values in Heathrow

table(DundeeDailyHumidityMeans[,1] == HeathrowDailyHumidityMeans2[,1]) #check all dates are same and in the same position

cor(x = DundeeDailyHumidityMeans[,2],y = HeathrowDailyHumidityMeans2[,2]) #calculate correlation between daily hummidity means
#returns [1] 0.4937412




HeathrowWindSpeed <- HeathrowRead[,c(1,5)] #isolate time and wind speed columns
table(is.na(HeathrowWindSpeed[,2])) #check for Nas in the wind speed column
HeathrowWindSpeed <- HeathrowWindSpeed[!is.na(HeathrowWindSpeed[,2]),] #remove nas
HeathrowWindSpeed$Date <- format(HeathrowWindSpeed$Time,"%y-%m-%d")
HeathrowWindSpeed$Wind_SpeedKm_h <- as.numeric(HeathrowWindSpeed[,2]) #coerce to numeric ie "80" becomes 80
HeathrowWindSpeed <- HeathrowWindSpeed[!is.na(HeathrowWindSpeed[,2]),]
HeathrowDailyWindSpeedMeans <- aggregate(cbind("DailyWindSpeedMean" = Wind_SpeedKm_h) ~ Date, data = HeathrowWindSpeed, FUN = mean)


DundeeWindSpeed <- DundeeRead[,c(1,5)] #isolate time and wind speed columns
table(is.na(DundeeWindSpeed[,2])) #check for Nas in the wind speed column
DundeeWindSpeed <- DundeeWindSpeed[!is.na(DundeeWindSpeed[,2]),] #remove nas
DundeeWindSpeed$Date <- format(DundeeWindSpeed$Time,"%y-%m-%d")
DundeeWindSpeed$Wind_SpeedKm_h <- as.numeric(DundeeWindSpeed[,2]) #coerce to numeric ie "80" becomes 80
DundeeWindSpeed <- DundeeWindSpeed[!is.na(DundeeWindSpeed[,2]),]
DundeeDailyWindSpeedMeans <- aggregate(cbind("DailyWindSpeedMean" = Wind_SpeedKm_h) ~ Date, data = DundeeWindSpeed, FUN = mean)



table(DundeeDailyWindSpeedMeans[,1] %in% HeathrowDailyWindSpeedMeans[,1])
#returns all true
table(HeathrowDailyWindSpeedMeans[,1] %in% DundeeDailyWindSpeedMeans[,1])
#returns 7 false, the rest true

HeathrowDailyWindSpeedMeans2 <- HeathrowDailyWindSpeedMeans[HeathrowDailyWindSpeedMeans[,1] %in% DundeeDailyWindSpeedMeans[,1],]
#remove dates that only have Wind speed values in Heathrow

table(DundeeDailyWindSpeedMeans[,1] == HeathrowDailyWindSpeedMeans2[,1]) #check all dates are same and in the same position

cor(x = DundeeDailyWindSpeedMeans[,2],y = HeathrowDailyWindSpeedMeans2[,2]) #calculate correlation between daily windspeed means
#returns [1] 0.1037135



HeathrowPressureRain <- HeathrowRead[,c(1,6)]
HeathrowPressureRain$Events <- as.factor(HeathrowPressureRain$Events)
HeathrowPressureRain$Date <- format(HeathrowPressureRain$Time,"%y-%m-%d")
HeathrowDailyRainMeans <- aggregate(cbind("DailyRainMean" = Events) ~ Date, data = HeathrowPressureRain, FUN = mean)

HeathrowDailyPressureMeans3 <- HeathrowDailyPressureMeans[HeathrowDailyPressureMeans[,1] %in% HeathrowDailyRainMeans[,1],]
cor(x = HeathrowDailyRainMeans[,2],y = HeathrowDailyPressureMeans3[,2])
#returns [1] -0.2857008


DundeePressureRain <- DundeeRead[,c(1,6)]
DundeePressureRain$Events <- as.factor(DundeePressureRain$Events)
DundeePressureRain$Date <- format(DundeePressureRain$Time,"%y-%m-%d")
DundeeDailyRainMeans <- aggregate(cbind("DailyRainMean" = Events) ~ Date, data = DundeePressureRain, FUN = mean)

DundeeDailyPressureMeans2 <- DundeeDailyPressureMeans[DundeeDailyPressureMeans[,1] %in% DundeeDailyRainMeans[,1],]
cor(x = DundeeDailyRainMeans[,2],y = DundeeDailyPressureMeans2[,2])
#returns [1] -0.1522531



HeathrowWindDir <- HeathrowRead[c("Time", "WindDirDegrees")] #isolate time and Wind Direction columns
HeathrowWindDir$Date <- date(HeathrowWindDir$Time)
HeathrowDailyWindDirMeans <- aggregate(cbind("DailyWindDirMean" = WindDirDegrees) ~ Date, data = HeathrowWindDir, FUN = mean)

HeathrowDailyTempMeans3 <- HeathrowDailyTempMeans[HeathrowDailyTempMeans[,1] %in% HeathrowDailyWindDirMeans[,1],]
cor(x = HeathrowDailyWindDirMeans[,2],y = HeathrowDailyTempMeans3[,2])
#returns [1] -0.03342881


DundeeWindDir <- DundeeRead[c("Time", "WindDirDegrees")] #isolate time and Wind Direction columns
DundeeWindDir$Date <- date(DundeeWindDir$Time)
DundeeDailyWindDirMeans <- aggregate(cbind("DailyWindDirMean" = WindDirDegrees) ~ Date, data = DundeeWindDir, FUN = mean)

DundeeDailyTempMeans2 <- DundeeDailyTempMeans[DundeeDailyTempMeans[,1] %in% DundeeDailyWindDirMeans[,1],]
cor(x = DundeeDailyWindDirMeans[,2],y = DundeeDailyTempMeans2[,2])
#returns [1] -0.1339723

