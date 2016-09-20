#Environment Setting
getwd()
setwd("./data/Winetest")

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

library(ggplot2)
library(MASS)

#Ordinal Logistic Regression

#Sampling training and testing data
trainsize<-round(nrow(white)*.7)
set.seed(123)
picknum<-sample(1:nrow(white),trainsize)
trainw<-white[picknum,]
testw<-white[-picknum,]
trainw$quality<-as.factor(trainw$quality)

#Full model fitting
olrfit<-polr(quality~.,data=trainw,Hess=TRUE)
olrfitcoef<-summary(olrfit)$coef
#Manually get the p-value of each coef
pvalue<-pnorm(abs(olrfitcoef[,3]),lower.tail=FALSE)*2 
olrfitcoef<-cbind(olrfitcoef,"p value"=round(pvalue,2))
#Auto variable selected:Backward
olrfitback<-step(olrfit,direction = "backward") 
olrfitcoef<-summary(olrfitback)$coef
pvalue<-pnorm(abs(olrfitcoef[,3]),lower.tail=FALSE)*2
olrfitcoef<-cbind(olrfitcoef,"p value"=round(pvalue,2))
#Confident Interval
confint(olrfitback)
#Odds value 
exp(coef(olrfitback))

#Prediction and Validation
testw<-cbind(testw,predict(olrfitback,testw,type = "probs"))
testw<-cbind(testw,"pquality"= predict(olrfitback,testw))
testw$pquality<-as.numeric(levels(testw$pquality))[testw$pquality] #turn level to numeric
sum(as.numeric(testw$quality==testw$pquality))
#Accuracy
accuracy<-sum(as.numeric(testw$quality==testw$pquality))/nrow(testw[,c("quality","pquality")])
#Measuring Predictive Accuracy
mae_olr<-mean(abs(testw$quality-testw$pquality))  

