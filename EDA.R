#Environment Setting
getwd()
setwd("./data/Winetest")

#Data loading
white<-read.csv("winequality-white.csv",header=TRUE,sep=";")

#Exploratory analysis
library(ggplot2)

str(white)
haed(white)
dim(white)
names(white)
summary(white)
str(white)
table(white$quality)

#barchart of wine quality
ggplot(data=white,aes(quality))+geom_bar(fill="white")+ggtitle("Bar of quality") +theme_dark()+ scale_x_continuous(breaks = seq(3,10, 1), lim = c(3, 10))

#histogram of all other variables
ggplot(data=white,aes(fixed.acidity))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(volatile.acidity))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(citric.acid))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(residual.sugar))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(chlorides))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(free.sulfur.dioxide))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(total.sulfur.dioxide))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(density))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(pH))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(sulphates))+geom_histogram(fill="white",col="black")+theme_dark()
ggplot(data=white,aes(alcohol))+geom_histogram(fill="white",col="black")+theme_dark()

#Bivariate Plots Section(Correlation)
library(corrplot)
library(PerformanceAnalytics)
cortable<-cor(white)
corrplot(cortable,method = "number")
chart.Correlation(cortable)
cor(white$density,white$residual.sugar)
ggplot(white,aes(x=alcohol,y=quality))+geom_point()+theme_dark()+ggtitle("density VS r.s VS quality")+geom_smooth(method=lm,se=FALSE)

#Multivariate Plots 
ggplot(white,aes(x=alcohol))+geom_histogram(aes(fill=factor(quality)),position="dodge",binwidth = 1)+theme_dark()+ggtitle("Multivariate plot")+ scale_fill_brewer(type = "seq", palette = 15)
ggplot(white,aes(x=factor(quality),y=alcohol,fill=quality))+geom_boxplot()+theme_dark()


