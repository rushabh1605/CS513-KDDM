#  Company    : Stevens 
#  Project    : Final Exam Problem 2
#  Purpose    : RF
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
factordata<-data

#Spliting Data
index = sample(nrow(factordata), as.integer(.7*nrow(factordata)))

training <-factordata[index,]
View(training)
testing <- factordata[-index,]
View(testing)


#install.packages("randomForest")
library(randomForest) 


#Creating Random Forest
fit<-randomForest(factor(STATE)~., data=training , importance=TRUE, ntree=220)

#Identifying Important Features
importance(fit)

#dev.off()
varImpPlot(fit)

#Prediction on Testing Data
prediction<-predict(fit,testing)
CFM <- table(actual=testing$STATE,prediction = prediction)
CFM

#Calculating Error Rate
wrong<-sum(testing$STATE!=prediction)
error_rate<-sum(wrong)/length(testing$STATE)
error_rate

#Calculating Accuracy
accuracy<-sum(diag (CFM) /sum(CFM))
accuracy 

library(caret)
confusion_matrix <- confusionMatrix(prediction,factor(testing$STATE))
confusion_matrix

