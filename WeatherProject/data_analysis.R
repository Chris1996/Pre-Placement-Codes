#data analysis script

library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")



#Creates plots and calculates chosen values

#Visualize day/week/month mean and median temperature in Heathrow and Dundee
lhr.dun.daily.temp.mean <- VisualizeAverages( Heathrow, 
                                              Dundee, 
                                              c("Time", "TemperatureC"))

lhr.dun.weekly.temp.mean <- VisualizeAverages( Heathrow, 
                                               Dundee, 
                                               c("Time", "TemperatureC"), 
                                               TimeDif = week)

lhr.dun.monthly.temp.mean <- VisualizeAverages( Heathrow, 
                                                Dundee, 
                                                c("Time", "TemperatureC"), 
                                                TimeDif = month)

lhr.dun.daily.temp.median <- VisualizeAverages( Heathrow, 
                                                Dundee, 
                                                c("Time", "TemperatureC"), 
                                                Average = median)

lhr.dun.weekly.temp.median <- VisualizeAverages( Heathrow, 
                                                 Dundee, 
                                                 c("Time", "TemperatureC"), 
                                                 TimeDif = week, 
                                                 Average = median)

lhr.dun.monthly.temp.median <- VisualizeAverages( Heathrow, 
                                                  Dundee, 
                                                  c("Time", "TemperatureC"), 
                                                  TimeDif = month, 
                                                  Average = median)


#Calculate and visualize whether the daily means of each weather varible are
#correlated in Heathrow and Dundee
temp.correlation <- CorrelatedValues( Heathrow, Dundee, c("Time", "TemperatureC"))

pressure.correlation <- CorrelatedValues( Heathrow, 
                                          Dundee, 
                                          c("Time", "Sea_Level_PressurehPa"))
visual.pressure.cor <- VisualizeAverages( Heathrow, 
                                          Dundee, 
                                          c("Time", "Sea_Level_PressurehPa"))

humidity.correlation <- CorrelatedValues( Heathrow, 
                                          Dundee, 
                                          c("Time", "Humidity"))
visual.humidity.cor <- VisualizeAverages( Heathrow, 
                                          Dundee, 
                                          c("Time", "Humidity"))

wind.speed.correlation <- CorrelatedValues( Heathrow, 
                                            Dundee, 
                                            c("Time", "Wind_SpeedKm_h"))
visual.ws.cor <- VisualizeAverages( Heathrow, Dundee, c("Time", "Wind_SpeedKm_h"))


#Visualize weather pressure is correlated with weather events in Heathrow
#and Dundee
lhr.events.pressure <- VisualizeRelationshipsBoxplot( Heathrow, 
                                                      c("Events", "Sea_Level_PressurehPa"))
lhr.rain.pressure <- VisualizeRelationshipsBoxplot( Heathrow, 
                                                    c("Events", "Sea_Level_PressurehPa"), 
                                                    Rain = T)

dun.events.pressure <- VisualizeRelationshipsBoxplot( Dundee, 
                                                      c("Events", "Sea_Level_PressurehPa"))
dun.rain.pressure <- VisualizeRelationshipsBoxplot( Dundee, 
                                                    c("Events", "Sea_Level_PressurehPa"), 
                                                    Rain = T)

#Visualize how Wind Direction is related to Temperature change in Heathrow
#and Dundee
lhr.wind.dir.temp <- VisualizePolar( Heathrow, TempRange = c(9, 14))

dun.wind.dir.temp <- VisualizePolar( Dundee, TempRange = c(6, 13))



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
                                 




