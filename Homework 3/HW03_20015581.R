#################################################

#  Company    : Stevens 
#  Project    : HW03_knn
#  Purpose    : KNN
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 10/25/2022
#  Comments   :

#################################################

rm(list=ls())

# Choosing File
f<-file.choose();

# Loading the data

data <- read.csv(f, na.strings = "?")
View(data)

dim(data)
#Delete NA enteries
data<-na.omit(data)
dim(data)
summary(data)

#Categorizing data
factordata<-data
factordata$Class <- factor(data$Class, levels=c("2","4"), labels=c("Benign","Malignant"))
#Summarizing each column (e.g. min, max, mean )
factordata

#Spliting Data
index = sample(nrow(factordata), as.integer(.7*nrow(factordata)))
index

training <-factordata[index,]
View(training)
testing <- factordata[-index,]
View(testing)

library(kknn)

#KNN for k=3
predict_k_3 <-kknn(formula=Class~., training , testing, k=3,kernel = "rectangular")
predict_k_3
fit<-fitted(predict_k_3)
table(Actual =testing$Class, Fitted=fit)

#Accuracy for k=3
accuracy_k_3 <-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy_k_3(table(Actual=testing$Class,Fitted=fit))

#KNN for k=5
predict_k_5 <-kknn(formula=Class~., training , testing, k=5,kernel = "rectangular")
predict_k_5
fit<-fitted(predict_k_5)
table(Actual =testing$Class, Fitted=fit)

#Accuracy for k=5
accuracy_k_5 <-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy_k_5(table(Actual=testing$Class,Fitted=fit))

#KNN for k=10
predict_k_10 <-kknn(formula=Class~., training , testing, k=10,kernel = "rectangular")
predict_k_10
fit<-fitted(predict_k_10)
table(Actual =testing$Class, Fitted=fit)

#Accuracy for k=10
accuracy_k_10 <-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy_k_10(table(Actual=testing$Class,Fitted=fit))

#################################################

