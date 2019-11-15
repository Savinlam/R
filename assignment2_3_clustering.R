library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)# clustering algorithms & visualization
library(tidyverse)
library(corrplot)

#import the file/data
WineData<-read.csv('Wine_data.csv', header=TRUE,sep=',')
#check whether the data is complete
any(is.na(WineData))
#remove missing value
WineData<-na.omit(WineData)
any(is.na(WineData))

#study the data
head(WineData)
str(WineData)
summary(WineData)
corrmatrix<-cor(WineData)
corrplot(corrmatrix,method='ellipse')
corrmatrix

#meaure the Euclidean distance with factoextra package

distance<-get_dist(WineData)
fviz_dist(distance,gradient = list(low="#00AFBB", mid = "white", high = "#FC4E07"))

#Hierarchical clustering
distwinedata<-dist(WineData,method = "euclidean") #calculate distance matrix,remove the first column TYPE
hmdl=hclust(distwinedata,method = 'complete') #Hirearchical clustering

#Analyze results with Dendrogram and common sense
dendhmdl=as.dendrogram(hmdl)
plot(dendhmdl,main="Hierachial clustering")

#cut the tree and assign to clusters
clustermember=cutree(hmdl,k=3)

#visulaize the clustering
plot(clustermember)

#K-meas to test K=3

kmeansmdl_k3=kmeans(WineData,centers = 3)
kmeansmdl_k3
plot(WineData,col=kmeansmdl_k3$cluster,pch=16)

#choosing the nr of clusters with Elbow method 
set.seed(123)

fviz_nbclust(WineData, kmeans, method = "wss")

#choosing the nr of clusters with silhouette
fviz_nbclust(WineData, kmeans, method = "silhouette")

#choosing the nr of clusters with Gap Statistic
set.seed(123)
gap_stat <- clusGap(WineData, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

##extracting results
# Compute k-means clustering with k = 3
final <- kmeans(WineData, 3, nstart = 25)
print(final)
#visualize the result 
fviz_cluster(final, data = WineData)
WineData %>% 
  mutate(cluster_k3=final$cluster) %>% 
  group_by(cluster_k3) %>% 
  summarise_all('mean')

#check the outliers
WineData %>% 
  as_data_frame() %>% 
  select_if(is.numeric) %>% 
  gather(-TYPE,key = "variable",value = "value") %>% 
  ggplot(aes(x=1,y=value))+
  geom_boxplot()+
  facet_wrap(~variable, scales = "free")

##check the variables with outliers
boxplot(WineData$ALCALINITY)
boxplot(WineData$ASH)
boxplot(WineData$COLOR)


##detect the outliers affect on clustering model
#1,check he center
final$centers
#2, obtain cluters 
final$cluster
#3, list the centers for each obersavation
centers<-final$centers[final$cluster,]
centers
#4,calculate the distance between the objects and cluster centers to determine the outliers 
distances <- sqrt(rowSums((WineData[-1] - centers)^2))
distances

#5, check the distance
orders<-distances[order(distances,decreasing=T)]
orders
#6, find the 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]
print(outliers)
#7, print the details about the outliers in dataset
print(WineData[outliers,])

#test the model with removing the 5 outliers
winedata_move5outliers<-WineData[-outliers,]

# Compute k-means clustering with k = 3 by new data
final <- kmeans(winedata_move5outliers[-1], 3, nstart = 25)
print(final)
#check the between_SS / total_SS
  

