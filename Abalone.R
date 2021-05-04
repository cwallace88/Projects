library(cluster)
library(fpc)
library(readr)
#Importing
library(readr)
abalone <- read_csv("abalone.csv")
#PReprocessing
str(abalone)
table(abalone$Sex, abalone$Rings)
#mean of infant
round(mean(abalone$Rings[abalone$Sex=="I"]))
#adding the 1.5 for conversion to years
round(mean(abalone$Rings[abalone$Sex=="I"]) + 1.5)


#removed 
abalone_copy <- read_csv("abalone copy.csv")
#reproducinble 
set.seed(1234)
#checking to see if converstion work
str(abalone_copy)
#see the difference since the infant value is gone
table(abalone_copy$Sex, abalone_copy$Rings)


# Creating a copied variable
abalonekmeans <- abalone_copy
sum(is.na(abalonekmeans))
abalonekmeans <- na.omit(abalonekmeans)
#removing class variable
abalonekmeans$Sex <- NULL
#normalizing the data
abalonekmeans <- scale(abalonekmeans)


# kmeans = 5
kc <- kmeans(abalonekmeans, 5)
#cluster to class evaluation
table(abalone_copy$Sex, kc$cluster,  dnn=c('Class in the dataset', 'Cluster mumber'))
print(kc)
clusplot(abalone_copy, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Try k=15
kc <- kmeans(abalonekmeans, 15)
table(abalone_copy$Sex, kc$cluster, dnn=c('Class in the dataset', 'Cluster mumber'))
print(kc)
clusplot(abalone_copy, kc$cluster, color=TRUE, shade=TRUE)


#Try k=8
kc <- kmeans(abalonekmeans, 8)
table(abalone_copy$Sex, kc$cluster, dnn=c('Class in the dataset', 'Cluster mumber'))
print(kc)
clusplot(abalone_copy, kc$cluster, color=TRUE, shade=TRUE)

