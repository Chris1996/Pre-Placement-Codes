#test script

#show the different errors for invalid inputs
CleanData( Heathrow, c("Time", "InvalidArg"))

CleanData("NotDataFrame" , c("Time", "TemperatureC"))

a <- 1:2
b <- 3:4
df <- data.frame(a,b)
CleanData(df , c("Time", "TemperatureC"))

WeatherAverages(Heathrow, c("Time", "TemperatureC"), TimeDif = "Invalid")

WeatherAverages(Heathrow, c("Time", "TemperatureC"), Average = "Invalid")

WeatherAverages(df, c("Time", "TemperatureC"))

#############################################################################

set.seed(50)
heathrow.test <- heathrow[sample(1:69517, 100), ]
dundee.test <- dundee[sample(1:25387, 100), ]
test1 <- CleanData( heathrow.test, c("Time", "TemperatureC"))
test2 <- CleanData( heathrow.test, c("Events", "Humidity"))
test3 <- CleanData( heathrow.test, rev(c("Events", "Humidity")))
test4 <- WeatherAverages( heathrow.test, c("Time", "TemperatureC"))
test5 <- WeatherAverages( heathrow.test, c("Time", "Humidity"))
test6 <- CleanData( dundee.test, c("TemperatureC", "Sea_Level_PressurehPa"))
test7 <- WeatherAverages( dundee.test, c("Time", "Wind_SpeedKm_h"))
test8 <- VisualizeAverages( heathrow.test, dundee.test, c("Time", "TemperatureC"))


