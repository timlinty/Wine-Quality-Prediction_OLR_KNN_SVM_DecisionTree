#Environment Setting
getwd()
setwd("./data/Winetest")

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

#Sampling training and testing data
set.seed(5566)
pt6<-sample(2,nrow(white),replace = TRUE,prob = c(0.7,0.3))
white$quality<-as.factor(white$quality)
traindt<-white[pt6==1,]
testdt<-white[pt6==2,]

#Classification tree 01: party (for categorical dependent variable)
library(party)
fitdt<-ctree(quality~.,data=traindt)
plot(fitdt,type="simple")
fitdtre<-predict(fitdt,testdt)
testdt<-cbind(testdt,"pre-value"=fitdtre)
testdt$quality<-as.numeric(levels(testdt$quality))[testdt$quality]
testdt$`pre-value`<-as.numeric(levels(testdt$`pre-value`))[testdt$`pre-value`]
table(testdt$quality,testdt$`pre-value`)
#Accuracy
accuracy_dt<-mean(testdt$quality==testdt$`pre-value`)
#Measuring Predictive Accuracy
mae_dt<-mean(abs(testdt$quality-testdt$`pre-value`))

#Classification tree 02: rpart(for continous dependent variable, need prune)
library(rpart)
traindt2<-white[pt6==1,]
testdt2<-white[pt6==2,]
fitdt2<-rpart(quality~.,data=traindt2,control = rpart.control(minsplit = 15))
plot(fitdt2)
fitdt2$cptable
opt <- which.min(fitdt2$cptable[,"xerror"]) # prune the tree
cp <- fitdt2$cptable[opt, "CP"]
fitdt2_prune <- prune(fitdt2, cp = cp)
plot(fitdt2_prune)
text(fitdt2, use.n=T)
fitdt2re<-predict(fitdt2_prune,testdt2)
fitdt2re
testdt2<-cbind(testdt2,"pre-value"=fitdt2re)
str(testdt2)
testdt2$quality<-as.numeric(levels(testdt2$quality))[testdt2$quality]
testdt2$`pre-value`<-as.numeric(levels(testdt2$`pre-value`))[testdt2$`pre-value`]
table(testdt2$quality,testdt2$`pre-value`)
#Accuracy
accuracy_dt2<-mean(testdt2$quality==testdt2$`pre-value`)
#Measuring Predictive Accuracy
mae_dt2<-mean(abs(testdt2$quality-testdt2$`pre-value`))


