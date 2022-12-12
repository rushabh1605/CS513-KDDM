#  Company    : Stevens 
#  Project    : HW04_NB
#  Purpose    : NB
#  First Name : Rushabh
#  Last Name  : Thakkar
#  Id			    : 20015581
#  Date       : 11/14/2022
#  Comments   :


rm(list = ls())

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

#Import package 'e1071' for Naive Bayes Classifier and class package
install.packages("e1071")
install.packages(class)
library(e1071)
library(class)

#Implementing NaiveBayes
model_naive<- naiveBayes(Class ~ ., data = training)

#Predicting target class for the Validation set
predict_naive <- predict(model_naive, testing)

conf_matrix <- table(predict_nb=predict_naive,class=testing$Class)
print(conf_matrix)

#Output of Naive Bayes Classifier
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

