# Data analysis script

library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")


# Creates plots and calculates chosen values

# Visualize day/week/month mean and median temperature in Heathrow and Dundee
lhr.dun.daily.temp.mean <- VisualizeAverages( heathrow, 
                                              dundee, 
                                              c("Time", "TemperatureC"))

lhr.dun.weekly.temp.mean <- VisualizeAverages( heathrow, 
                                               dundee, 
                                               c("Time", "TemperatureC"), 
                                               TimeDif = week)

lhr.dun.monthly.temp.mean <- VisualizeAverages( heathrow, 
                                                dundee, 
                                                c("Time", "TemperatureC"), 
                                                TimeDif = month)

lhr.dun.daily.temp.median <- VisualizeAverages( heathrow, 
                                                dundee, 
                                                c("Time", "TemperatureC"), 
                                                Average = median)

lhr.dun.weekly.temp.median <- VisualizeAverages( heathrow, 
                                                 dundee, 
                                                 c("Time", "TemperatureC"), 
                                                 TimeDif = week, 
                                                 Average = median)

lhr.dun.monthly.temp.median <- VisualizeAverages( heathrow, 
                                                  dundee, 
                                                  c("Time", "TemperatureC"), 
                                                  TimeDif = month, 
                                                  Average = median)


#Calculate and visualize whether the daily means of each weather varible are
#correlated in Heathrow and Dundee
temp.correlation <- CorrelatedValues( heathrow, dundee, c("Time", "TemperatureC"))

pressure.correlation <- CorrelatedValues( heathrow, 
                                          dundee, 
                                          c("Time", "Sea_Level_PressurehPa"))
visual.pressure.cor <- VisualizeAverages( heathrow, 
                                          dundee, 
                                          c("Time", "Sea_Level_PressurehPa"))

humidity.correlation <- CorrelatedValues( heathrow, 
                                          dundee, 
                                          c("Time", "Humidity"))
visual.humidity.cor <- VisualizeAverages( heathrow, 
                                          dundee, 
                                          c("Time", "Humidity"))

wind.speed.correlation <- CorrelatedValues( heathrow, 
                                            dundee, 
                                            c("Time", "Wind_SpeedKm_h"))
visual.ws.cor <- VisualizeAverages( heathrow, dundee, c("Time", "Wind_SpeedKm_h"))


#Visualize weather pressure is correlated with weather events in Heathrow
#and Dundee
lhr.events.pressure <- VisualizeRelationshipsBoxplot( heathrow, 
                                                      c("Events", "Sea_Level_PressurehPa"))
lhr.rain.pressure <- VisualizeRelationshipsBoxplot( heathrow, 
                                                    c("Events", "Sea_Level_PressurehPa"), 
                                                    Rain = T)

dun.events.pressure <- VisualizeRelationshipsBoxplot( dundee, 
                                                      c("Events", "Sea_Level_PressurehPa"))
dun.rain.pressure <- VisualizeRelationshipsBoxplot( dundee, 
                                                    c("Events", "Sea_Level_PressurehPa"), 
                                                    Rain = T)

#Visualize how Wind Direction is related to Temperature change in Heathrow
#and Dundee
lhr.wind.dir.temp <- VisualizePolar( heathrow, TempRange = c(9, 14))

dun.wind.dir.temp <- VisualizePolar( dundee, TempRange = c(6, 13))



name <- c("temp.correlation", 
          "pressure.correlation", 
          "humidity.correlation",
          "wind.speed.correlation")
value <- c(temp.correlation, 
           pressure.correlation, 
           humidity.correlation,
           wind.speed.correlation)
correlated.results <- data.frame(name,value)
#already written the file, don't want to keep overwriting
#write.table(correlated.results, file = "Results/correlated_results.tsv", sep = "\t", row.names = F)
                                 



