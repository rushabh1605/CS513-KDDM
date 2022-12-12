#  Company    : Stevens 
#  Project    : HW08_2
#  Purpose    : kmeans
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 12/10/2022
#  Comments   :


rm(list = ls())

# Choosing File
f<-file.choose()

# Loading the data

data <- read.csv(f, na.strings = 0)
View(data)

dim(data)
summary(data)

#Delete NA enteries
data<-na.omit(data)
dim(data)
summary(data)
data <- data[,-1]


kmeans_2<- kmeans(data[,-1],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,data[,1])
