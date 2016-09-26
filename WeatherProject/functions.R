# Load functions
CleanData <- function( Data, WeatherArg) {
  # Remove NAs and uneccessary values
  # Args: Data = a data frame or data table to be cleaned
  #       WeatherArg = Vector indicating the columns of the data to be cleaned
  
  # Error checks
  if (!all(WeatherArg %in% c("Time", 
                             "Temperature", 
                             "Humidity", 
                             "Pressure",
                             "Wind.Speed",
                             "Events",
                             "Wind.Direction"))) {
    stop("Invalid argument WeatherArg")
  }
  if (length(WeatherArg) != 2) {
    stop("Invalid length of WeatherArg")
  }
  if (!is.data.frame(Data)) {
    stop("Data is not data frame")
  }
  if (!all(colnames(Data) %in% c("Time", 
                                 "Temperature", 
                                 "Humidity", 
                                 "Pressure",
                                 "Wind.Speed",
                                 "Events",
                                 "Wind.Direction")) && 
      !all(c("Time", 
             "Temperature", 
             "Humidity", 
             "Pressure",
             "Wind.Speed",
             "Events",
             "Wind.Direction") %in% colnames(Data))) {
    stop("Invalid Data")
  }
  
  MyCleanData <- Data[ , WeatherArg]
  if ( WeatherArg[ 2] == "Temperature") {
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Temperature), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Temperature == -9999, ]
  }
  if ( WeatherArg[ 2] == "Humidity") {
    MyCleanData <- MyCleanData[ !MyCleanData$Humidity == "N/A", ]
    MyCleanData$Humidity <- as.numeric( MyCleanData$Humidity)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Humidity), ]
  }
  if ( WeatherArg[ 2] == "Pressure") {
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Pressure), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Pressure == -9999, ]
    MyCleanData <- MyCleanData[ !MyCleanData$Pressure == 0, ]
  }
  if ( WeatherArg[ 2] == "Wind.Speed") {
    MyCleanData <- MyCleanData[ !MyCleanData$Wind.Speed == "Calm", ]
    MyCleanData$Wind.Speed <- as.numeric( MyCleanData$Wind.Speed)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Wind.Speed), ]
    MyCleanData <- MyCleanData[ !MyCleanData$Wind.Speed == -9999, ]
  }
  if ( WeatherArg[ 2] == "Events") {
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Events), ]
    MyCleanData[ MyCleanData$Events == "", "Events"] <- "No weather event"
    MyCleanData$Events <- factor( MyCleanData$Events)
  }
  if ( WeatherArg [ 2] == "Wind.Direction") {
    MyCleanData$Wind.Direction <- as.numeric( MyCleanData$Wind.Direction)
    MyCleanData <- MyCleanData[ !is.na( MyCleanData$Wind.Direction), ]
    MyCleanData <- MyCleanData[ MyCleanData$Wind.Direction <= 360, ]
    MyCleanData[ MyCleanData$Wind.Direction == 360, "Wind.Direction"] <- 0
  }
  return(MyCleanData)
}

WeatherAverages <- function( City, WeatherArg, TimeDif = date, Average = mean) {
  # Calculates the daily, weekly or monthly mean of the chosen weather data
  # Args: City = data frame either Heathrow or Dundee
  #       WeatherArg = vector length 2 with first arguement "Time"
  #       TimeDif = function: either date, week or month
  #       Average = chosen function to calculate averages
  
  # Error checks
  # We can use error checks in CleanData to check City and WeatherArg are valid
  if (!(c(TimeDif) %in% c(date, week, month))) {
    stop("Invalid argument TimeDif")
  }
  if (!(c(Average) %in% c(mean, median))) {
    stop("Invalid argument Average")
  }
  
  MyData <- CleanData( City, WeatherArg)
  MyData2 <- MyData[ , WeatherArg]
  if ( identical( TimeDif, date)) {
    MyData2$Time <- date( MyData2$Time)
  } else if ( identical( TimeDif, week)) {
    YearNum <- year( MyData2$Time)
    WeekNum <- week( MyData2$Time)
    MyData2$Time <- paste( YearNum, formatC( WeekNum, width = 2, flag = "0"), sep = "-")
  } else if ( identical( TimeDif, month)) {
    YearNum <- year( MyData2$Time)
    MonthNum <- month( MyData2$Time)
    MyData2$Time <- paste( YearNum, formatC( MonthNum, width = 2, flag = "0"), sep = "-")
  }
  MyData2[ , WeatherArg[ 2]] <- as.numeric( MyData2[ , WeatherArg[ 2]])
  MyData2 <- MyData2[ !is.na( MyData2[ , WeatherArg[ 2]]), ]
  MyMeanData <- aggregate( MyData2[ WeatherArg[ 2]], by = MyData2[ "Time"], FUN = Average)
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
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14),
           axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%d %b %Y")
}

VisualizeAverages2 <- function( City1, City2, WeatherArg, TimeDif = date, Average = mean) {
  # Creates a scatter graph of time vs chosen weather arguement  
  data1 <- WeatherAverages( City1, WeatherArg, TimeDif, Average)
  data2 <- WeatherAverages( City2, WeatherArg, TimeDif, Average)
  ggplot() +
    geom_point( data = data1, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'City1'")) +
    geom_point( data = data2, aes_string( WeatherArg[ 1], WeatherArg[ 2], colour = "'City2'")) +
    scale_colour_manual( labels = c("Heathrow", "Dundee"), values = c( "blue", "red")) +
    labs( colour = "Cities") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14),
           axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)) +
    if (identical(TimeDif, week)) {
      scale_x_discrete(name = "Time by Year and Week", 
                       breaks = c("2014-01", "2014-13", "2014-26", "2014-39",
                                  "2015-01", "2015-13", "2015-26", "2015-39",
                                  "2016-01", "2016-13", "2016-26", "2016-39"))
    }
}

CorrelatedValues <- function( City1, City2, WeatherArg) {
  # Calculates correlation of a weather pattern between two cities
  # Args: City1/City2 = data frames either Heathrow or Dundee
  # WeatherArg = vector length 2 with first arguement "Time"
  AverageCity1 <- WeatherAverages( City1, WeatherArg)
  AverageCity2 <- WeatherAverages( City2, WeatherArg)
  AverageCity1b <- AverageCity1[ AverageCity1$Time %in% AverageCity2$Time, ]
  AverageCity2b <- AverageCity2[ AverageCity2$Time %in% AverageCity1$Time, ]
  cor( AverageCity1b[[ 2]], AverageCity2b[[ 2]], method = "spearman")
} 

VisualizeCorrelations <- function(City1, City2, WeatherArg) {
  AverageCity1 <- WeatherAverages( City1, WeatherArg)
  AverageCity2 <- WeatherAverages( City2, WeatherArg)
  AverageCity1b <- AverageCity1[ AverageCity1$Time %in% AverageCity2$Time, ]
  AverageCity2b <- AverageCity2[ AverageCity2$Time %in% AverageCity1$Time, ]
  ggplot() +
    geom_point(aes_string( AverageCity1b[[ 2]], AverageCity2b[[ 2]])) +
    xlab( paste( "Heathrow", colnames(AverageCity1b)[2], sep = " ")) +
    ylab( paste( "Dundee", colnames(AverageCity2b)[2], sep = " ")) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12))
}

VisualizeRelationshipsBoxplot <- function( City, WeatherArg, Rain = F, Colour = "Blues") {
  # Gives a graphical representation of the relationship between 2 weather variables
  # in the given City
  MyData <- CleanData( City, WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  if ( Rain == T) {
    if ( identical( City, heathrow)) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    } else if ( identical( City, dundee)) {
      levels( MyData$Events) <- c( "No Rain", "Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    }
  }
  MyData$Occurrences <- MyData$Events
  levels(MyData$Occurrences) <- as.character(aggregate(MyData, MyData["Events"], FUN = length)[[2]])
  ggplot( data = MyData, na.rm = T, aes_string( WeatherArg[ 1], WeatherArg[ 2])) +
    geom_boxplot( aes_string( fill = "Occurrences")) +
    ggtitle( City) +
    scale_fill_brewer( palette = Colour) +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14)) 
}

VisualizeRelationshipsScatter <- function( City, WeatherArg, Rain = F) {
  # Gives a graphical representation of the relationship between 2 weather variables
  # in the given City
  MyData <- CleanData( City, WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  if ( Rain == T) {
    if ( identical( City, heathrow)) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    } else if ( identical( City, dundee)) {
      levels( MyData$Events) <- c( "No Rain", "Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    }
  }
  levels(MyData$Events) <- paste(aggregate(MyData, MyData["Events"], length)[[1]],
                                 aggregate(MyData, MyData["Events"], length)[[2]], sep = " - ")
  MyData2 <- aggregate(MyData, by = list(MyData[, 1], MyData[, 2]), length)
  MyData2 <- MyData2[, c(1:3)]
  names(MyData2) <- c(WeatherArg[ 2], WeatherArg[ 1], "Count")
  ggplot( data = MyData2, na.rm = T, aes_string( WeatherArg[ 1], WeatherArg[ 2])) +
    geom_point( aes( colour = Events, size = Count)) +
    ggtitle( City) +
    scale_colour_hue(name = "Events - Occurrences") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14)) 
}

VisualizePolar <- function( City, TempRange = c( 0, 15)) {
  # Produces a 360 degree plot showing how wind direction is related to temperature 
  # change in the given city
  MyData <- CleanData( City, c( "Wind.Direction", "Temperature"))
  MyData <- CleanData( MyData, rev(c( "Wind.Direction", "Temperature")))
  MyMeanData <- aggregate( MyData, by = MyData[ "Wind.Direction"], FUN = mean)
  MyMeanData <- rbind(MyMeanData, c("Wind.Direction" = 360, MyMeanData[1,2], "Wind.Direction" = 360))
  ggplot( data = MyMeanData, na.rm = T) +
    geom_line( aes( x = MyMeanData$Wind.Direction, y = MyMeanData$Temperature)) +
    ylim( TempRange) +
    coord_polar( theta = "x") +
    ggtitle( City) +
    xlab("Wind.Direction") +
    ylab("Temperature") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           panel.grid.minor = element_line( colour = "grey"))
}

VisualizeCorrelationRain <- function( City, WeatherArg, Rain = F) {
  # Gives a graphical representation of the relationship between 2 weather variables
  # in the given City
  MyData <- CleanData( City, WeatherArg)
  MyData <- CleanData( MyData, rev(WeatherArg))
  if ( Rain == T) {
    if ( identical( City, heathrow)) {
      levels( MyData$Events) <- c( "No Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    } else if ( identical( City, dundee)) {
      levels( MyData$Events) <- c( "No Rain", "Rain", "No Rain", "Rain", "Rain", "Rain", "Rain", "Rain", "Rain")
    }
  }
  levels(MyData$Events) <- paste(aggregate(MyData, MyData["Events"], length)[[1]],
                                 aggregate(MyData, MyData["Events"], length)[[2]], sep = " - ")
  ggplot( data = MyData, na.rm = T, aes_string(WeatherArg[ 2])) +
    geom_density( aes_string( colour = "Events")) +
    ggtitle( City) +
    scale_colour_hue(name = "Events - Occurrences") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           axis.title = element_text(size = 16),
           axis.text = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14)) 
}

VisualizePolarWindSpeed <- function( City) {
  # Produces a 360 degree plot showing how wind direction is related to temperature 
  # change in the given city
  MyData <- CleanData( City, c( "Wind.Direction", "Wind.Speed"))
  MyData <- CleanData( MyData, rev(c( "Wind.Direction", "Wind.Speed")))
  MyMeanData <- aggregate( MyData, by = MyData[ "Wind.Direction"], FUN = mean)
  MyMeanData <- rbind(MyMeanData, c("Wind.Direction" = 360, MyMeanData[1,2], "Wind.Direction" = 360))
  ggplot( data = MyMeanData, na.rm = T) +
    geom_line( aes( x = MyMeanData$Wind.Direction, y = MyMeanData$Wind.Speed)) +
    coord_polar( theta = "x") +
    ylim(c(0,30)) +
    xlab("Wind.Direction") +
    ylab("Wind.Speed") +
    theme( panel.background = element_rect( fill = "white"),
           panel.grid.major = element_line( colour = "grey"),
           panel.grid.minor = element_line( colour = "grey"))
}

