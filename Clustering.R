#Environment Setting
getwd()
setwd("./data/Winetest")

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

#Sampling training and testing data
set.seed(11233)
pt4<-sample(2,nrow(white),replace = TRUE,prob = c(0.7,0.3))
ctrain<-white[pt4==1,]
ctest<-white[pt4==2,]
ctrain2<-ctrain[,1:11]

#Hierarchical clustering
hcw<-hclust(dist(ctrain2),method = "ward.D") #build the cluster tree
plot(hcw)
rect.hclust(hcw,k=7) #draw rectangle around tree clusters
groups<-cutree(hcw,k=7) #cut the tree into k groups
ggplot(NULL,aes(x=ctrain[,1],y=ctrain[,2]))+geom_point(col=groups)+theme_dark()
plot(groups,col=ctrain$quality)
table(ctrain$quality,groups)

#Partition clustering-kmeans
km<-kmeans(ctrain2,7)
km
table(ctrain$quality,km$cluster)

#Partition clustering-kmediods
library(fpc)
kme<-pamk(ctrain2) # pamk will auto-optimize the k
kme$nc #optimized result of k
par(mfrow=c(1,2))
plot(kme$pamobject) #plot the result

#dendisty-based clustering
dbc<-dbscan(ctrain2,eps=4,MinPts=20)
table(dbc$cluster)
plot(dbc$cluster,col=ctrain$quality)
plotcluster(ctrain2,dbc$cluster)

#Verification
library(cluster)
kme2<-pam(ctrain2,7)
par(mfrow=c(1,2))
table(ctrain$quality,kme2$clustering)
plot(kme2$clustering,col=ctrain$quality)
par(mfrow=c(1,2))
plot(kme2)
par(mfrow=c(1,1))