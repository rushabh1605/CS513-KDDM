#  Company    : Stevens 
#  Project    : HW06.2
#  Purpose    : RF
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 11/14/2022
#  Comments   :


rm(list = ls())

# Choosing File
f<-file.choose()

# Loading the data

data <- read.csv(f, na.strings = "?")
View(data)

dim(data)

#Delete NA enteries
data<-na.omit(data)
dim(data)
summary(data)

factordata<-data

#Categorizing data
factordata$Class <- factor(data$Class, levels=c("2","4"), labels=c("Benign","Malignant"))
factordata

#Spliting Data
index = sample(nrow(factordata), as.integer(.7*nrow(factordata)))
index

training <-factordata[index,]
View(training)
testing <- factordata[-index,]
View(testing)


#install.packages("randomForest")
library(randomForest)

#Creating Random Forest
fit<-randomForest(factor(Class)~., data=training , importance=TRUE, ntree=220)

#Identifying Important Features
importance(fit)

#dev.off()
varImpPlot(fit)

#Prediction on Testing Data
prediction<-predict(fit,testing)
table(actual=testing$Class,prediction)

#Calculating Error Rate
wrong<-sum(testing[,11]!=prediction)
error_rate<-wrong/length(testing[,11])
error_rate

#Calculating Accuracy
accuracy<-1-error_rate
accuracy 
