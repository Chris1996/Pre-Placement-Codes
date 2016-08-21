pollutantmean <- function(directory,pollutant,id=1:332) {
  # 'directory' is a character vector of length 1 indicating the location of
  # the csv files
  
  # 'pollutant'is a character vector of length 1 indicating the name of the
  # pollutant for which we will calculate the mean; either sulfate or nitrate
  
  # 'id' is an integer vector indicating the monitor ID numbers to be used
  
  # Return the mean of the pollutant across all monitors listed in the id 
  # vector (ignoring NA values)
  
  setwd(directory)
  
  temp <- list.files()  #list files in specdata
  
  my_data <- data.frame(Date=as.Date(character()),   #create empty data frame
                        sulfate=character(),
                        nitrate=character(),
                        ID=character())
  for (i in id) {x <- temp[i]
  my_data <- rbind(read.csv(temp[i]),my_data)   #combine each data frame
    }
  
  x <- my_data[!is.na(my_data[[pollutant]]),]  #remove rows with NAs in the pollutant column
 
  mean(x[[pollutant]])  #calculate the mean
  
}