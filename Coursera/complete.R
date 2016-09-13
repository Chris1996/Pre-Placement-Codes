complete <- function(directory, id=1:332) {
  # 'directory' is a character vector of length 1 indicating the location of
  # the csv files
  
  # 'id' is an integer vector indicating the monitor ID numbers to be used
  
  # Returns a data frame of the form:
  # id  nobs
  # 1   117
  # 2   1041
  # ...
  # Where 'id' is the monitor ID number and 'nobs' is the number of complete
  # cases
  
  setwd(directory)
  
  temp <- list.files()  #list files in specdata
  
  x <- data.frame(id = numeric(), nobs = numeric()) #create empty data frame
    
  for (i in id) {
  my_data <- read.csv(temp[i])   
  my_data2 <- my_data[complete.cases(my_data),]  #remove rows with NAs
  x <- rbind(x, data.frame(id = i,nobs = length(my_data2[,1])) ) #add new info to data frame
  
  }
  
  x
  
}
