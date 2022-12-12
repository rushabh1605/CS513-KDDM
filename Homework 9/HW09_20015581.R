#  Company    : Stevens 
#  Project    : HW09
#  Purpose    : SVM
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

#Delete NA enteries
data<-na.omit(data)
dim(data)
summary(data)

#Create test and training dataset
index <- seq (1,nrow(data),by=5)
test<-data[index,]
training<-data[-index,]

#install.packages("e1071")
library(e1071)
?svm()
## svm

#Training model
svm.model <- svm( factor(diagnosis)~ ., data =training)
svm.pred <- predict(svm.model,  test )

#Confusion Matrix 
tab <- table(Predicted = svm.pred, Actual = test$diagnosis )
tab

#Misclassification Error
1-sum(diag(tab))/sum(tab)

