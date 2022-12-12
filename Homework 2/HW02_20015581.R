#  Company    : Stevens 
#  Project    : HW02_EDA
#  Purpose    : EDA
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       :
#  Comments   :


rm(list = ls())

## Loading the data

data <- read.csv("/Users/rushabhthakkar/Downloads/breast-cancer-wisconsin.csv", na.strings = "?")
data

#Summarizing each column (e.g. min, max, mean )

summary(data)

#Identifying missing values
is.na(data)

#Replacing the missing values with the “mean” of the column.
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
View(data)

#Displaying the frequency table of “Class” vs. F6
fTable <- table(data$Class, data$F6)
ftable(fTable)
View(fTable)

#Displaying the scatter plot of F1 to F6, one pair at a time
plot(data[2:7], main= "Scatter plot of F1 to F6", ph=10 , col=2)


#Show histogram box plot for columns F7 to F9
boxplot(data[8:10], main = "Histogram Box Plot")


#Delete all the objects from your R- environment. 
rm(list = ls())
ls()

#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
data2 <- read.csv("/Users/rushabhthakkar/Downloads/breast-cancer-wisconsin.csv", na.strings = "?")
data2


#Remove any row with a missing value in any of the columns.
udata <- na.omit(data2)
udata



