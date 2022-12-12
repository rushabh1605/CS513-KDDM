#  Company    : Stevens 
#  Project    : HW07
#  Purpose    : ANN
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
data <- data[,-1]
temp<-factor(data$diagnosis)
data$diagnosis <- temp
class(data$diagnosis)


#Delete NA enteries
data<-data.frame(lapply(na.omit(data),as.numeric))
dim(data)
summary(data)



index <- seq (1,nrow(data),by=5)
test<- data[index,]
training<-data[-index,]

#install.packages("neuralnet")
library("neuralnet")

net_data<- neuralnet( diagnosis~. , data = training, hidden=5, threshold=0.01)

#Plot the neural network
plot(net_data)

## test should have only the input colum
ann <-compute(net_data, test[,-1])
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
length(test$diagnosis)

table(predition=ann_cat, Actual=test$diagnosis)

wrong<- (test$diagnosis!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

