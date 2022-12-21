#  Company    : Stevens 
#  Project    : Final Exam Problem 1
#  Purpose    : Cart
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

#Spliting Data
index = sample(nrow(factordata), as.integer(.7*nrow(factordata)))
index

training <-factordata[index,]
View(training)
testing <- factordata[-index,]
View(testing)
#install.packages('rpart')
library(rpart)
#Implementing CART 
cart <- rpart(STATE ~ ., data = training, method = "class")

#Predicting class for test set
predicted <- predict(cart, testing, type = "class")
print(length(predicted))
print(length(testing$STATE))

#Confusion Matrix
conf_matrix <- table(predicted,testing$STATE)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)


library(caret)
confusion_matrix <- confusionMatrix(predicted,factor(testing$STATE))
confusion_matrix
