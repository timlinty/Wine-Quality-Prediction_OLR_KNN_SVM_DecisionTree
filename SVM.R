#Environment Setting
getwd()
setwd("./data/Winetest")

library(e1071)

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

#Sampling training and testing data
set.seed(66)
pt2<-sample(2,nrow(white),replace = TRUE, prob=c(0.7,0.3))
trainw3<-white[pt2==1,]
testw3<-white[pt==2,]
trainw3$quality<-factor(trainw3$quality)
testw3$quality<-factor(testw3$quality)

#SVM modeling
svmmodel<-svm(quality~.,data=trainw3,kernal="polynomial",cost=100,scale=FALSE)
#tunning the best parameter (try and error)
tuned<-tune(svm, quality~.,data=trainw3,kernal="polynomial",ranges = list(cost=c(0.01,0.1,1,10,100))) 
#prediction
svmp<-predict(svmmodel,testw3,type="class")
svmp<-as.numeric(levels(svmp))[svmp]
#Accuracy
accuracy_svm<-mean(testw3$quality==svmp)
accuracy_svm
svmresult<-as.data.frame(cbind(testw3,svmp))
svmresult$correct<-as.numeric(testw3$quality==svmp)
ggplot(svmresult,aes(x=pH,y=alcohol,colour=factor(correct)))+geom_point()+theme_dark()
levels(testw3$quality)
testw3$quality<-as.numeric(levels(testw3$quality))[testw3$quality]
#Measuring Predictive Accuracy
mae_svm<-mean(abs(as.numeric(testw3$quality)-svmp))