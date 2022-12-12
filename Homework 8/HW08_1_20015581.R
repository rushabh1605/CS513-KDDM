#  Company    : Stevens 
#  Project    : HW08_1
#  Purpose    : hclust
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


data_dist<-dist(data[,-1])
hclust_results<-hclust(data_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,data[,1])