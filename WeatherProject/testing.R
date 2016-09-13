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
HeathrowTest <- Heathrow[sample(1:69517, 100), ]
DundeeTest <- Dundee[sample(1:25387, 100), ]
test1 <- CleanData( HeathrowTest, c("Time", "TemperatureC"))
test2 <- CleanData( HeathrowTest, c("Events", "Humidity"))
test3 <- CleanData( HeathrowTest, rev(c("Events", "Humidity")))
test4 <- WeatherAverages( HeathrowTest, c("Time", "TemperatureC"))
test5 <- WeatherAverages( HeathrowTest, c("Time", "Humidity"))
test6 <- CleanData( DundeeTest, c("TemperatureC", "Sea_Level_PressurehPa"))
test7 <- WeatherAverages( DundeeTest, c("Time", "Wind_SpeedKm_h"))
test8 <- VisualizeAverages( HeathrowTest, DundeeTest, c("Time", "TemperatureC"))


