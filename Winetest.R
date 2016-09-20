#Environment Setting
getwd()
setwd("./data/Winetest")

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

#EDA
library(ggplot2)
summary(white)
str(white)
table(white$quality)
ggplot(data=white,aes(quality))+geom_bar(fill="white")+ggtitle("Bar of quality") +theme_dark()+ scale_x_continuous(breaks = seq(3,10, 1), lim = c(3, 10))

#correlation
library(corrplot)
library(PerformanceAnalytics)
cortable<-cor(white)
corrplot(cortable,method = "number")
chart.Correlation(cortable)
cor(white$density,white$residual.sugar)

#x strong correlation plot
ggplot(white,aes(x=density,y=residual.sugar,color=quality))+geom_point()+theme_dark()+ggtitle("density VS r.s VS quality")+xlim(0.98,1.015)+ylim(0,40)+geom_smooth(method=lm,se=FALSE)

#x y strong correlation plot
ggplot(white,aes(x=alcohol,y=quality))+geom_point()+theme_dark()+ggtitle("density VS r.s VS quality")+geom_smooth(method=lm,se=FALSE)

#multivariate plot 
ggplot(white,aes(x=alcohol))+geom_histogram(aes(fill=factor(quality)),position="dodge",binwidth = 1)+theme_dark()+ggtitle("Multivariate plot")+ scale_fill_brewer(type = "seq", palette = 15)
ggplot(white,aes(x=factor(quality),y=alcohol,fill=quality))+geom_boxplot()+theme_dark()


#Ordinal Logistic Regression
library(MASS)
trainsize<-round(nrow(white)*.7)
set.seed(123)
picknum<-sample(1:nrow(white),trainsize)
trainw<-white[picknum,]
testw<-white[-picknum,]
trainw$quality<-as.factor(trainw$quality)
olrfit<-polr(quality~.,data=trainw,Hess=TRUE)
olrfitcoef<-summary(olrfit)$coef
pvalue<-pnorm(abs(olrfitcoef[,3]),lower.tail=FALSE)*2 #Manually get the p-value of each coef
olrfitcoef<-cbind(olrfitcoef,"p value"=round(pvalue,2))

olrfitback<-step(olrfit,direction = "backward") #Auto variable selected:Backward
olrfitcoef<-summary(olrfitback)$coef
pvalue<-pnorm(abs(olrfitcoef[,3]),lower.tail=FALSE)*2
olrfitcoef<-cbind(olrfitcoef,"p value"=round(pvalue,2))
confint(olrfitback)
exp(coef(olrfitback))

testw<-cbind(testw,predict(olrfitback,testw,type = "probs"))
testw<-cbind(testw,"pquality"= predict(olrfitback,testw))
testw$pquality<-as.numeric(levels(testw$pquality))[testw$pquality] #turn level to numeric
sum(as.numeric(testw$quality==testw$pquality))
accuracy<-sum(as.numeric(testw$quality==testw$pquality))/nrow(testw[,c("quality","pquality")])
olr_mae<-mean(abs(testw$quality-testw$pquality))  #Measuring Predictive Accuracy


#KNN
library(class)
set.seed(666)
pt<-sample(2,nrow(white),replace = TRUE,prob=c(0.7,0.3))
trainw2<-white[pt==1,1:ncol(white)-1] #trainX
testw2<-white[pt==2,1:ncol(white)-1] #testX
trainw2q<-white[pt==1,ncol(white)] #trainY
testw2q<-white[pt==2,ncol(white)] #testY
predictw2<-knn(trainw2,testw2,cl=trainw2q,k=5) #predictY
predictw2<-as.numeric(levels(predictw2))[predictw2] #turn vector to number
CrossTable(x = testw2q, y = predictw2, prop.chisq=FALSE) 
accuracy_knn<-mean(testw2q==predictw2)
knntestresult<-cbind(testw2,testw2q,predictw2)
knntestresult$correct<-as.numeric(testw2q==predictw2)
ggplot(knntestresult,aes(x=pH,y=alcohol,colour=factor(correct)))+geom_point()+theme_dark()
olr_knn<-mean(abs(testw2q-predictw2))

#SVM
library(e1071)
set.seed(66)
pt2<-sample(2,nrow(white),replace = TRUE, prob=c(0.7,0.3))
trainw3<-white[pt2==1,]
testw3<-white[pt==2,]
trainw3$quality<-factor(trainw3$quality)
testw3$quality<-factor(testw3$quality)
svmmodel<-svm(quality~.,data=trainw3,kernal="polynomial",cost=100,scale=FALSE)
tuned<-tune(svm, quality~.,data=trainw3,kernal="polynomial",ranges = list(cost=c(0.01,0.1,1,10,100))) #tunning the best parameter
svmp<-predict(svmmodel,testw3,type="class")
svmp<-as.numeric(levels(svmp))[svmp]
accuracy_svm<-mean(testw3$quality==svmp)
accuracy_svm
svmresult<-as.data.frame(cbind(testw3,svmp))
svmresult$correct<-as.numeric(testw3$quality==svmp)
ggplot(svmresult,aes(x=pH,y=alcohol,colour=factor(correct)))+geom_point()+theme_dark()

levels(testw3$quality)
testw3$quality<-as.numeric(levels(testw3$quality))[testw3$quality]
olr_svm<-mean(abs(as.numeric(testw3$quality)-svmp))

#Clustering
head(white)
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
library(fpc)
dbc<-dbscan(ctrain2,eps=4,MinPts=20)
table(dbc$cluster)
plot(dbc$cluster,col=ctrain$quality)
plotcluster(ctrain2,dbc$cluster)


library(cluster)
kme2<-pam(ctrain2,7)
par(mfrow=c(1,2))
table(ctrain$quality,kme2$clustering)
plot(kme2$clustering,col=ctrain$quality)
par(mfrow=c(1,2))
plot(kme2)
par(mfrow=c(1,1))


#decision tree/classification

set.seed(5566)
pt6<-sample(2,nrow(white),replace = TRUE,prob = c(0.7,0.3))
traindt<-white[pt6==1,]
testdt<-white[pt6==2,]
library(party)
fitdt<-ctree(quality~.,data=traindt)
plot(fitdt,type="simple")




#Regression
fitall<-lm(quality~.,data=red)
summary(fitall)$coef


#Variable selection
#All possible
library(leaps)
fitap<-regsubsets(quality~.,data=red,nbest = 2)
par(mfrow=c(1,2))
plot(fitap,scale="adjr2") #criteria=adj-R Square
plot(fitap,scale="bic")   #criteria=bic

#Automation
null<-lm(quality~1,data=red)
full<-lm(quality~.,data=red)
step(null, scope=list(upper=full,lower=null), direction = "forward")    #forward
step(full, scope=list(upper=full,lower=null), direction = "backward")   #backward
step(null, scope=list(upper=full,lower=null), direction = "both")       #stepwise
fomodel<-lm(formula = quality ~ alcohol + volatile.acidity + sulphates + 
              total.sulfur.dioxide + chlorides + pH + free.sulfur.dioxide, 
            data = red)
anova(fomodel)
confint(full,"citric.acid")
summary(full)



vistep(fitw)
fitbest<-lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
              total.sulfur.dioxide + pH + sulphates + alcohol, data = red)
summary(fitbest)
aov(lm(quality~.,data=red))
anova(lm(quality~.,data=red))
anova(full)

hist(red$quality)
hist(white$quality)



?knn
