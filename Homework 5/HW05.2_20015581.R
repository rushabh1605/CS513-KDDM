#  Company    : Stevens 
#  Project    : HW05.2_CART
#  Purpose    : CART
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
install.packages('rpart')
library(rpart)
#Implementing CART 
cart <- rpart(Class ~ ., data = training, method = "class")

#Predicting class for test set
predicted <- predict(cart, testing, type = "class")
print(length(predicted))
print(length(testing$Class))

#Confusion Matrix
conf_matrix <- table(predicted,testing$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

