#weeks 1 quiz code
my_data <- read.csv("hw1_data.csv")

head(my_data,2) #extract first two rows of the data

tail(my_data,2) #extract last two  rows of the data

my_data[["Ozone"]][47] #value of Ozone in the 47th row

sum(is.na(my_data[["Ozone"]])) #number of missing values in Ozone column

Ozone <- my_data["Ozone"]
Ozone2 <- Ozone[!is.na(Ozone)] #remove missing values
mean(Ozone2) #compute mean of the Ozone column

my_data2 <- my_data[complete.cases(my_data),] #remove rows with missing values
x <- my_data2[my_data2["Ozone"]>31 & my_data2["Temp"]>90,] #rows with Ozone>31 and Temp>90
mean(x[["Solar.R"]]) 

my_data3 <- my_data[my_data["Month"]==6,] 
mean(my_data3[["Temp"]]) #mean Temp in June

my_data4 <- my_data2[my_data2["Month"]==5,]
max(my_data4[["Ozone"]]) #max value of Ozone in May
