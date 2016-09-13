corr <- function(directory, threshold = 0) {
  # 'directory' is a character vector of length 1 indicating the location of
  # the csv files
  
  # 'threshold' is a numeric vector of length 1 indicating the number of completely
  # observed observations required to compute the correlation between nitrate and 
  # sulfate
  
  x <- complete(directory)
  temp <- list.files()  #list files in specdata
  j <- 1
   
  for (i in 1:332) {if (x[i,2]>threshold) {
      my_data <- read.csv(temp[i])
      my_data2 <- my_data[complete.cases(my_data),]
      y[j] <- cor(my_data2[,2],my_data2[,3])
      j <- j+1
      }
     
     
   }
y
  
}