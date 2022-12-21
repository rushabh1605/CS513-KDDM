#  Company    : Stevens 
#  Project    : Final Exam Problem 1
#  Purpose    : Kmeans clustering
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
# Kmeans 
k_means2 <- kmeans(factordata[,-1],2,nstart=10) 

k_means2
k_means2$cluster
k_means2$centers
k_means2$size
k_means2$totss
k_means2$withinss
k_means2$betweenss


# Tabulating clustered
table(k_means2$cluster,factordata$STATE)

ftable(k_means2$cluster,factordata$STATE)


# Heirarchical clustering
df_dist <- dist(factordata[,-1], diag=TRUE)

#hclust
hclust_result <- hclust(df_dist)
dndo<- as.dendrogram(hclust_result)

# Tabulating clustered 
hclust_2<-cutree(hclust_result,4)
table(hclust_2,factordata[,1])

