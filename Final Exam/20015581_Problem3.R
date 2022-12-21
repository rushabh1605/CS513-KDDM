#  Company    : Stevens 
#  Project    : Final Exam Problem 3
#  Purpose    : C5.0
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 12/17/2022
#  Comments   :


rm(list = ls())

# Choosing File
f<-file.choose()

# Loading the data

data <- read.csv(f, na.strings = 0)
View(data)

dim(data)

#Delete NA enteries
data<-na.omit(data)
dim(data)
summary(data)
data$STATEFIPS = NULL
data$zipcode = NULL
data$total = NULL

factordata <- data
#factor(factordata$STATE)
#factordata
#Spliting Data
index = sample(nrow(factordata), as.integer(.7*nrow(factordata)))

training <-factordata[index,]
View(training)
testing <- factordata[-index,]
View(testing)

class(factordata$STATE)
#install.packages('C50')
library(C50)
#Implement C 5.0
class(training$STATE)
model<-C5.0(factor(training$STATE)~.,training[,-1])
summary(model)
plot(model)

#Prediction on Testing Data
prediction<-predict(model,testing[,-1],type="class") 

#Calculating Error Rate
wrong<-sum(testing$STATE!=prediction)
error_rate<-sum(wrong)/length(testing$STATE)
error_rate

#Calculating Accuracy
accuracy<- 1 - error_rate
accuracy 

library(caret)
confusion_matrix <- confusionMatrix(prediction,factor(testing$STATE))
confusion_matrix
