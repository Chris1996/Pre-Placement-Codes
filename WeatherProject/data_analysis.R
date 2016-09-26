# Data analysis script

library(weatherData)
library(ggplot2)
library(lubridate)
library(plyr)

source("functions.R")
source("data_load.R")

# Creates plots and calculates chosen values

# Visualize day/week/month mean and median temperature in Heathrow and Dundee
lhr.dun.daily.temp.mean <- VisualizeAverages( heathrow, 
                                              dundee, 
                                              c("Time", "Temperature"))

lhr.dun.weekly.temp.mean <- VisualizeAverages2( heathrow, 
                                                dundee, 
                                                c("Time", "Temperature"), 
                                                TimeDif = week)

lhr.dun.monthly.temp.mean <- VisualizeAverages2( heathrow, 
                                                 dundee, 
                                                 c("Time", "Temperature"), 
                                                 TimeDif = month)

lhr.dun.daily.temp.median <- VisualizeAverages( heathrow, 
                                                dundee, 
                                                c("Time", "Temperature"), 
                                                Average = median)

lhr.dun.weekly.temp.median <- VisualizeAverages2( heathrow, 
                                                  dundee, 
                                                  c("Time", "Temperature"), 
                                                  TimeDif = week, 
                                                  Average = median)

lhr.dun.monthly.temp.median <- VisualizeAverages2( heathrow, 
                                                   dundee, 
                                                   c("Time", "Temperature"), 
                                                   TimeDif = month, 
                                                   Average = median)

# Calculate and visualize whether the daily means of each weather varible are
# correlated in Heathrow and Dundee
temp.correlation <- CorrelatedValues( heathrow, dundee, c("Time", "Temperature"))

visual.temperature.cor2 <- VisualizeCorrelations(heathrow, dundee, 
                                                 c("Time", "Temperature"))

pressure.correlation <- CorrelatedValues( heathrow, dundee, 
                                          c("Time", "Pressure"))

visual.pressure.cor <- VisualizeAverages( heathrow, dundee, 
                                          c("Time", "Pressure"))

visual.pressure.cor2 <- VisualizeCorrelations(heathrow, dundee, 
                                              c("Time", "Pressure"))

humidity.correlation <- CorrelatedValues( heathrow, dundee, 
                                          c("Time", "Humidity"))

visual.humidity.cor <- VisualizeAverages( heathrow, dundee, 
                                          c("Time", "Humidity"))

visual.humidity.cor2 <- VisualizeCorrelations(heathrow, dundee, 
                                              c("Time", "Humidity"))

wind.speed.correlation <- CorrelatedValues( heathrow, dundee, 
                                            c("Time", "Wind.Speed"))

visual.ws.cor <- VisualizeAverages( heathrow, dundee, c("Time", "Wind.Speed"))

visual.ws.cor2 <- VisualizeCorrelations(heathrow, dundee, c("Time", "Wind.Speed"))

# Visualize weather pressure is correlated with weather events in Heathrow
# and Dundee
lhr.events.pressure <- VisualizeRelationshipsBoxplot( heathrow, 
                                                      c("Events", "Pressure"))
lhr.rain.pressure <- VisualizeRelationshipsBoxplot( heathrow, 
                                                    c("Events", "Pressure"), 
                                                    Rain = T)

dun.events.pressure <- VisualizeRelationshipsBoxplot( dundee, 
                                                      c("Events", "Pressure"))
dun.rain.pressure <- VisualizeRelationshipsBoxplot( dundee, 
                                                    c("Events", "Pressure"), 
                                                    Rain = T)

# Visualize how Wind Direction is related to Temperature change in Heathrow
# and Dundee
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

write.table(correlated.results, file = "Results/correlated_results.tsv", sep = "\t", row.names = F)


b <- CleanData(heathrow, c("Events", "Pressure"))
b$Events <- as.factor(b$Events)
levels( b$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
cor(as.numeric(b$Events), b$Pressure, use = "complete.obs")
# [1] -0.2156727

d <- CleanData(dundee, c("Events", "Pressure"))
d$Events <- as.factor(d$Events)
levels( d$Events) <- c( "No Rain", "Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
cor(as.numeric(d$Events), d$Pressure, use = "complete.obs")
# [1] -0.1279982


# Plot histograms to aid boxplots in showing correlation between pressure and weather events
MyData <- CleanData( heathrow, c("Events", "Pressure"))
MyData <- CleanData( MyData, c("Pressure", "Events"))
MyData$Occurrences <- MyData$Events
levels(MyData$Occurrences) <- as.character(aggregate(MyData, MyData["Events"], FUN = length)[[2]])
levels(MyData$Events) <- paste(levels(MyData$Events), levels(MyData$Occurrences), sep = " = ")

ggplot( data = MyData, na.rm = T, aes(x = Pressure)) +
  geom_histogram( aes(fill = Events), binwidth = 1) +
  theme( panel.background = element_rect( fill = "white"),
         panel.grid.major = element_line( colour = "grey"),
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.position = "none") +
  facet_wrap("Events", scales = "free_y")




MyData2 <- CleanData( dundee, c("Events", "Pressure"))
MyData2 <- CleanData( MyData2, c("Pressure", "Events"))
MyData2$Occurrences <- MyData2$Events
levels(MyData2$Occurrences) <- as.character(aggregate(MyData2, MyData2["Events"], FUN = length)[[2]])
levels(MyData2$Events) <- paste(levels(MyData2$Events), levels(MyData2$Occurrences), sep = " = ")

ggplot( data = MyData2, na.rm = T, aes(x = Pressure)) +
  geom_histogram( aes(fill = Events), binwidth = 1) +
  theme( panel.background = element_rect( fill = "white"),
         panel.grid.major = element_line( colour = "grey"),
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.position = "none") +
  facet_wrap("Events", scales = "free_y")






                                 