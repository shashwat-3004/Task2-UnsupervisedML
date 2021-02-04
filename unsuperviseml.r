####################################
library(tidyverse)
library(factoextra)
####################################
# Reading the data
data<-read.csv("Iris.csv")
head(data)
####################################
# Summary of data
summary(data)
####################################
# Removing non-numeric varaibles
species<-data$Species
data<-data[,c(-1,-6)]
head(iris)

#####################################
# To find optimal number of cluster
fviz_nbclust(data,FUNcluster = kmeans,method="wss") 

#####################################
# K-Means Clustering

k_mean<-kmeans(x=data,centers=3,nstart = 25)
k_mean

######################################
# The cluster plot
fviz_cluster(k_mean,data=data)+ scale_fill_discrete(labels=c("Iris-Setosa","Iris-Versicolor","Iris-Virginica"))

######################################

## Preparing all the needed data
clustered_data<-data.frame(Species=species,Cluster_no=k_mean$cluster)
head(clustered_data)


full_data<-cbind(data,clustered_data)
full_data$Cluster_no<-factor(full_data$Cluster_no)
head(full_data)

######################################
# Centroids of the clusters
centers<-k_mean$centers
centers<-as.data.frame(centers)
centers

######################################
## Plot to see the cluster as well as the centroid

full_data%>%ggplot()+geom_point(aes(x =SepalLengthCm,y=SepalWidthCm,colour=Cluster_no))+
  geom_text(aes(x=5,y=4),label="Iris-Setosa",size=3,colour="red")+
  geom_text(aes(x=5.5,y=2.5),label="Iris-virginica",colour="blue",size=3)+
  geom_text(aes(x=7,y=3.4),label="Iris-versicolor",colour="darkgreen",size=3)+
  geom_point(data=centers,aes(x=SepalLengthCm,y=SepalWidthCm))

######################################