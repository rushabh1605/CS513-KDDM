#  Company    : Stevens 
#  Project    : HW06.1
#  Purpose    : C5.0
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



install.packages('C50')
library(C50)

#Implement C 5.0
model<-C5.0(Class~.,training[,-1])
summary(model)
plot(model)

#Prediction using test 
prediction<-predict(model,testing[,-1],type="class") 

#Forming the confusin matrix
conf_matrix<-table(testing[,11],prediction)
conf_matrix
str(prediction)

#Calculating error rate 
wrong<-sum(testing[,11]!=prediction)
error_rate<-wrong/length(testing[,11])
error_rate

#Calculating Accuracy
accuracy<-1-error_rate
accuracy 
