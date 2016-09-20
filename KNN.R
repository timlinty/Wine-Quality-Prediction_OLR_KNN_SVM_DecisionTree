#Environment Setting
getwd()
setwd("./data/Winetest")

library(class)

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")


#Sampling training and testing data
set.seed(666)
pt<-sample(2,nrow(white),replace = TRUE,prob=c(0.7,0.3))
trainw2<-white[pt==1,1:ncol(white)-1] #trainX
testw2<-white[pt==2,1:ncol(white)-1] #testX
trainw2q<-white[pt==1,ncol(white)] #trainY
testw2q<-white[pt==2,ncol(white)] #testY

#KNN modeling
predictw2<-knn(trainw2,testw2,cl=trainw2q,k=5) #predictY
predictw2<-as.numeric(levels(predictw2))[predictw2] #turn vector to number(for verification)
#Verification by cross table
CrossTable(x = testw2q, y = predictw2, prop.chisq=FALSE) 
#Accuracy
accuracy_knn<-mean(testw2q==predictw2)
knntestresult<-cbind(testw2,testw2q,predictw2)
knntestresult$correct<-as.numeric(testw2q==predictw2)
ggplot(knntestresult,aes(x=pH,y=alcohol,colour=factor(correct)))+geom_point()+theme_dark()
#Measuring Predictive Accuracy
mae_knn<-mean(abs(testw2q-predictw2))
