library(weatherData)
library(ggplot2)
library(lubridate)
setwd("~/R/WeatherProject/WeatherProject/")

CleanData <- function( Data, WeatherArg) {
  # Remove NAs and uneccessary values
  # Args: Data = a data frame or data table to be cleaned
  #       WeatherArg = Vector indicating the columns of the data to be cleaned
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

ReadData <- function( City) {
# Take argurement City, either "Heathrow" or "Dundee and loads the correspponding data file
#
  if ( City == "Heathrow") {
    MyData <- read.table( "Processed/rowDataH.tsv", sep="\t", header = T, stringsAsFactors = F) #heathrow weather data
    MyData$Time <- as.POSIXct( MyData$Time)
    return( MyData)
  } else if ( City == "Dundee") {
    MyData <- read.table( "Processed/rowDataD.tsv", sep="\t", header = T, stringsAsFactors = F) #dundee weather data
    MyData$Time <- as.POSIXct( MyData$Time)
    return( MyData)
  } else
    print( "Check input args, ReadData only accepts Heathrow or Dundee")
}

WeatherAverages <- function( City, WeatherArg, TimeDif = date, Average = mean) {
# Calculates the daily, weekly or monthly mean of the chosen weather data
# Args: City = either "Heathrow" or "Dundee"
#       WeatherArg = vector length 2 with first arguement "Time"
#       TimeDif = function: either date, week or month
#       Average = chosen function to calculate averages
  MyData <- CleanData( ReadData( City), WeatherArg)
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
    geom_point( data = data1, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'blue'")) +
    geom_point( data = data2, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'red'")) +
    scale_colour_manual( labels = c("Heathrow", "Dundee"), values = c( "blue", "red")) +
    labs( colour = "Cities") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"))
} 

CorrelatedValues <- function( City1, City2, WeatherArg) {
# Calculates correlation of a weather pattern between two cities
# Args: City1/City2 = either Heathrow or Dundee
# WeatherArg = vector length 2 with first arguement "Time"
  AverageCity1 <- WeatherAverages( City1, WeatherArg)
  AverageCity2 <- WeatherAverages( City2, WeatherArg)
  AverageCity1b <- AverageCity1[ AverageCity1$Date %in% AverageCity2$Date, ]
  AverageCity2b <- AverageCity2[ AverageCity2$Date %in% AverageCity1$Date, ]
  cor( AverageCity1b[[ 2]], AverageCity2b[[ 2]])
} 

RainOrNoRain <- function( City) {
  # Reads, cleans and converts the events coloumn to factors "Rain" or "No Rain"
  MyData <- ReadData( City)[ , c("Time", "Events")]
  MyData$Events <- factor( MyData$Events)
  levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
  MyData <- MyData[ !is.na( MyData$Events), ]
}

VisualizeRelationshipsBoxplot <- function( City, WeatherArg, Rain = F, Colour = "Blues") {
# Gives a graphical representation of the relationship between 2 weather variables...
# ...in the given City
  MyData <- CleanData( ReadData( City), WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  if ( Rain == T) {
    if ( identical( City, "Heathrow")) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    } else if (identical( City, "Dundee")) {
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
  MyData <- CleanData( ReadData( City), c( "WindDirDegrees", "TemperatureC"))
  MyData <- CleanData( MyData, rev(c( "WindDirDegrees", "TemperatureC")))
  MyMeanData <- aggregate( MyData, by = MyData[ "WindDirDegrees"], FUN = mean)
  ggplot( data = MyMeanData, na.rm = T) +
    geom_line( aes( x = MyMeanData$WindDirDegrees, y = MyMeanData$TemperatureC, colour = "red")) +
    ylim( TempRange) +
    coord_polar( theta = "x") +
    ggtitle( City) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           panel.grid.minor = element_line( colour = "grey"))
}


######################################################################
#Heathrow <- ReadData("Heathrow")
#Dundee <- ReadData("Dundee")

#VisualizeAverages("Heathrow", "Dundee", c("Time", "TemperatureC"))
