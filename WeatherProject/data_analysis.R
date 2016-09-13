#data analysis script

library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")


WeatherAverages <- function( City, WeatherArg, TimeDif = date, Average = mean) {
  # Calculates the daily, weekly or monthly mean of the chosen weather data
  # Args: City = data frame either Heathrow or Dundee
  #       WeatherArg = vector length 2 with first arguement "Time"
  #       TimeDif = function: either date, week or month
  #       Average = chosen function to calculate averages
  MyData <- CleanData( City, WeatherArg)
  MyData2 <- MyData[ , WeatherArg]
  if ( identical( TimeDif, date)) {
    MyData2$Time <- date( MyData2$Time)
  } else if ( identical( TimeDif, week)) {
    YearNum <- year( MyData2$Time)
    WeekNum <- week( MyData2$Time)
    MyData2$Time <- YearNum * 100 + WeekNum
  } else if ( identical( TimeDif, month)) {
    YearNum <- year( MyData2$Time)
    MonthNum <- month( MyData2$Time)
    MyData2$Time <- YearNum * 100 + MonthNum
  }
  MyData2[ , WeatherArg[ 2]] <- as.numeric( MyData2[ , WeatherArg[ 2]])
  MyData2 <- MyData2[ !is.na( MyData2[ , WeatherArg[ 2]]), ]
  MyMeanData <- aggregate( MyData2, by = MyData2[ "Time"], FUN = Average)
  MyMeanData <- MyMeanData[ , c( "Time", WeatherArg[ 2])]
}

VisualizeAverages <- function( City1, City2, WeatherArg, TimeDif = date, Average = mean) {
  # Creates a scatter graph of time vs chosen weather arguement  
  data1 <- WeatherAverages( City1, WeatherArg, TimeDif, Average)
  data2 <- WeatherAverages( City2, WeatherArg, TimeDif, Average)
  ggplot() +
    geom_point( data = data1, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'City1'")) +
    geom_point( data = data2, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'City2'")) +
    scale_colour_manual( labels = c("Heathrow", "Dundee"), values = c( "blue", "red")) +
    labs( colour = "Cities") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"))
} 

CorrelatedValues <- function( City1, City2, WeatherArg) {
  # Calculates correlation of a weather pattern between two cities
  # Args: City1/City2 = data frames either Heathrow or Dundee
  # WeatherArg = vector length 2 with first arguement "Time"
  AverageCity1 <- WeatherAverages( City1, WeatherArg)
  AverageCity2 <- WeatherAverages( City2, WeatherArg)
  AverageCity1b <- AverageCity1[ AverageCity1$Time %in% AverageCity2$Time, ]
  AverageCity2b <- AverageCity2[ AverageCity2$Time %in% AverageCity1$Time, ]
  cor( AverageCity1b[[ 2]], AverageCity2b[[ 2]])
} 

VisualizeRelationshipsBoxplot <- function( City, WeatherArg, Rain = F, Colour = "Blues") {
  # Gives a graphical representation of the relationship between 2 weather variables...
  # ...in the given City
  MyData <- CleanData( City, WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  if ( Rain == T) {
    if ( identical( City, Heathrow)) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    } else if ( identical( City, Dundee)) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    }
  }
  ggplot( data = MyData, na.rm = T) +
    geom_boxplot( aes_string( WeatherArg[ 1], WeatherArg[ 2], fill = WeatherArg[ 1])) +
    ggtitle( City) +
    scale_fill_brewer( palette = Colour) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"))
}

VisualizeRelationshipsScatter <- function( City, WeatherArg) {
  # Gives a graphical representation of the relationship between 2 weather variables...
  # ...in the given City
  MyData <- CleanData( ReadData( City), WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  ggplot( data = MyData, na.rm = T) +
    geom_point( aes_string( WeatherArg[ 1], WeatherArg[ 2])) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"))
} 

VisualizePolar <- function( City, TempRange = c( 0, 15)) {
  MyData <- CleanData( City, c( "WindDirDegrees", "TemperatureC"))
  MyData <- CleanData( MyData, rev(c( "WindDirDegrees", "TemperatureC")))
  MyMeanData <- aggregate( MyData, by = MyData[ "WindDirDegrees"], FUN = mean)
  ggplot( data = MyMeanData, na.rm = T) +
    geom_line( aes( x = MyMeanData$WindDirDegrees, y = MyMeanData$TemperatureC)) +
    ylim( TempRange) +
    coord_polar( theta = "x") +
    ggtitle( City) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           panel.grid.minor = element_line( colour = "grey"))
}



##############################################################

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
                                 




