rm(list=ls())
#install.packages('rattle')
data(wine, package='rattle')
head(wine)
wine.stand <- scale(wine[-1])  # To standarize the variables
# K-Means clustering
k.means.fit <- kmeans(wine.stand, 3) # k = 3
attributes(k.means.fit)
# Centroids:
k.means.fit$centers
# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(wine.stand, nc=6) 
library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(wine[,1],k.means.fit$cluster)

## Hierarchical clustering
d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 
#confusion matrix
table(wine[,1],groups)

## example: European protein intake
url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
head(food)
set.seed(123456789) ## to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat
## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)
## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7
set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])
library(cluster)
clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg, main='Dendrogram') ## dendrogram
groups <- cutree(foodagg, k=4) # cut tree into 3 clusters
rect.hclust(foodagg, k=4, border="red") 

## example 2: unsupervised customer classification
rm(list=ls())
install.packages("RCurl")
install.packages("gdata")
require(RCurl)
require(gdata)
url='http://www.salemmarafi.com/wp-content/uploads/2014/04/clustering-vanilla.xls'
offers<-read.xls(url)
head(offers)
transactions<-read.table('transactions.csv', sep = ';', header=T)
head(transactions)
# Create transaction matrix (a pivot table like in Excel way!)
library(reshape)
pivot<-melt(transactions[1:2])
#CustomerLastName as id variables
pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
pivot<-cbind(offers,pivot[-1])
# write.csv(file="pivot.csv",pivot) # to save your data
cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)
head(cluster.data)
library(cluster)
D=daisy(cluster.data, metric='gower')
H.fit <- hclust(D, method="ward")
plot(H.fit) # display dendrogram
groups <- cutree(H.fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 
# 2D representation of the Segmentation:
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')
# Merge Data
cluster.deals<-merge(transactions[1:2],groups,by.x = "CustomerLastName", by.y = "row.names")
colnames(cluster.deals)<-c("Name","Offer","Cluster")
head(cluster.deals)
# Get top deals by cluster
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])
head(cluster.topDeals)
# export the data
write.csv(file="topdeals.csv",cluster.topDeals,row.names=F)

## example 3 Clustering social network
teens <- read.csv("snsdata.csv")
head(teens,3)
dim(teens)
str(teens) #description
summary(teens$age) #age variable summary
teens = na.omit(teens)
dim(teens)
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale)) #standardize
teen_clusters <- kmeans(interests_z, 5) #divide into 5 clusters
teen_clusters$size
teen_clusters$centers
par(mfrow=c(2,2))
# visualize clusters by group in pie charts
pie(colSums(interests[teen_clusters$cluster==1,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==2,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==3,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==4,]),cex=0.5)