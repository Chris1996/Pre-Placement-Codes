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


#load functions
CleanData <- function( Data, WeatherArg) {
  # Remove NAs and uneccessary values
  # Args: Data = a data frame or data table to be cleaned
  #       WeatherArg = Vector indicating the columns of the data to be cleaned
  
  #Error checks
  if (!all(WeatherArg %in% c("Time", 
                           "TemperatureC", 
                           "Humidity", 
                           "Sea_Level_PressurehPa",
                           "Wind_SpeedKm_h",
                           "Events",
                           "WindDirDegrees"))) {
    stop("Error: invalid argument WeatherArg")
  }
  if (length(WeatherArg) != 2) {
    stop("Error: invalid length of WeatherArg")
  }
  if (!is.data.frame(Data)) {
    stop("Error: Data is not data frame")
  }
  if (!all(colnames(Data) %in% c("Time", 
                             "TemperatureC", 
                             "Humidity", 
                             "Sea_Level_PressurehPa",
                             "Wind_SpeedKm_h",
                             "Events",
                             "WindDirDegrees")) && 
      !all(c("Time", 
             "TemperatureC", 
             "Humidity", 
             "Sea_Level_PressurehPa",
             "Wind_SpeedKm_h",
             "Events",
             "WindDirDegrees") %in% colnames(Data))) {
    stop("Error: invalid Data")
  }
  
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

WeatherAverages <- function( City, WeatherArg, TimeDif = date, Average = mean) {
  # Calculates the daily, weekly or monthly mean of the chosen weather data
  # Args: City = data frame either Heathrow or Dundee
  #       WeatherArg = vector length 2 with first arguement "Time"
  #       TimeDif = function: either date, week or month
  #       Average = chosen function to calculate averages
  
  #Error checks
  #We can use error checks in CleanData to check City and WeatherArg are valid
  if (!(c(TimeDif) %in% c(date, week, month))) {
    stop("Error: invalid argument TimeDif")
  }
  if (!(c(Average) %in% c(mean, median))) {
    stop("Error: invalid argument Average")
  }
  
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



