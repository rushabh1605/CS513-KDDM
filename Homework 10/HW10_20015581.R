#  Company    : Stevens 
#  Project    : HW10
#  Purpose    : SOM
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 12/10/2022
#  Comments   :


rm(list = ls())
set.seed(123)
# Choosing File
f<-file.choose()

#Loading the data

data <- read.csv(f, na.strings = 0)
View(data)

dim(data)

#Delete NA enteries
data<-na.omit(data)
dim(data)
data <- data[,-1]
summary(data)

#install.packages("kohonen")
library("kohonen")
?som()

training <- data[,-1]

#SOM Model
data_som <- som(as.matrix(training), grid=somgrid(3,1))

summary(data_som)
str(data_som)
data_som$unit.classif

#Matrix
table(cluster=data_som$unit.classif , data[,1])



