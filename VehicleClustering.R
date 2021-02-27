# Load cluster package
library(cluster)

# Read the CSV file. 
library(readr)
vehicle <- read_csv("Desktop/Machine Learning/Week 10/vehicle.csv")


# 2. Data Preprocessing
#result is reproducible
set.seed(1234)
#copy data to new variable
myvehicle <- vehicle
#view the first 6 rows of the dataset
head(myvehicle)
#removing the class variable
myvehicle$Class <- NULL
#colSums(is.na(myvehicle))
#normilze the dataset with scale function
myvehicle <- scale(myvehicle)

# 3. K-means clustering
#Run the method and store the result in kc variable
kc<-kmeans(myvehicle, 4)
#output the result
print(kc)
#components
kc$size
kc$iter

library(fpc)
# 4. cluster to class evaluation
table(vehicle$Class, kc$cluster,  dnn=c('Class in the dataset', 'Cluster mumber'))
#sum of table
sum(table(vehicle$Class, kc$cluster))

# 5. cluster plot
clusplot(myvehicle, kc$cluster, color=TRUE, shade=TRUE)

#6.A
kc$size
kc$betweenss
kc$withinss
kc$iter

#6.B
myvehicle5 <- vehicle
myvehicle5$Class <- NULL
myvehicle5 <- scale(myvehicle5)
kc5<-kmeans(myvehicle5, 5)
print(kc5)
kc5$size
kc5$betweenss
kc5$withinss
kc5$iter

#6.C
myvehicle2 <- vehicle
myvehicle2$Class <- NULL
myvehicle2 <- scale(myvehicle2)
kc2<-kmeans(myvehicle2, 2)
print(kc2)
kc2$size
kc2$betweenss
kc2$withinss
kc2$iter

#6.D
myvehicle12 <- vehicle
myvehicle12$Class <- NULL
myvehicle12 <- scale(myvehicle12)
kc12<-kmeans(myvehicle12, 12)
print(kc12)
kc12$centers
kc12$totss
kc12$size
kc12$betweenss
kc12$withinss
kc12$iter
