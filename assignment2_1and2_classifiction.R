library(dplyr)
library(ggplot2)
library(NbClust)
library(purrr)
library(corrplot)
library(tidyverse)

##part 1
##load the data
HData<-read.csv("Housing_data.csv",header = TRUE,sep = ",")

##check the missing values in the data
any(is.na(HData))

##check the structure, general information of the data
str(HData)
summary(HData)

##check the outliers for every variables
boxplot(HData$PCCR)
boxplot(HData$PRLZ)
boxplot(HData$INDUS)
boxplot(HData$NOX)
boxplot(HData$AVR)
boxplot(HData$AGE)
boxplot(HData$DIS)
boxplot(HData$RAD)
boxplot(HData$TAX)
boxplot(HData$MEDV)

##quick plot of all varialbes for data
HData %>% 
  as_data_frame() %>% 
  select_if(is.numeric) %>% 
  gather(key = "variable",value = "value") %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")

HData %>% 
  as_data_frame() %>% 
  select_if(is.numeric) %>% 
  gather(key = "variable",value = "value") %>% 
  ggplot(aes(x=1,y=value))+
  geom_boxplot()+
  facet_wrap(~variable, scales = "free")

##perform coorelation analysis between all variables
cor=cor(HData)
corrplot(cor(HData),'ellipse')

HData1<-HData %>% 
  as_data_frame() %>% 
  select_if(is.numeric) %>% 
  gather(-MEDV,key = "variable",value = "value")

##visualize the relationship the varialbes with price of house(MEDV)
HData1%>% 
  ggplot(aes(x=value,y=MEDV))+
  geom_point()+
  facet_wrap(~variable, scales = "free")

plot(HData$AVR,HData$MEDV)

plot(HData$MEDV, HData$PCCR)
a=mean(HData$PCCR)
abline(a=mean(HData$PCCR),b=0,col='red')
points(x=mean(HData$MEDV),y=mean(HData$PCCR),pch=16,col='blue')

hist(HData$MEDV)

plot(HData$PRLZ)
##build model
model1=lm(MEDV~PCCR +PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,data = HData)
summary(model1)
res1=residuals(model1)
n=length(res1)
mse=sum(res1^2)/n
#improve model
model2=lm(MEDV~PCCR +PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0,data = HData)
summary(model2)
coef(model2)
ypred2=coef(model2)[1]*HData$PCCR+coef(model2)[2]*HData$PRLZ+coef(model2)[3]*HData$NOX
+coef(model2)[4]*HData$AVR+coef(model2)[5]*HData$AGE+coef(model2)[6]*HData$DIS
+coef(model2)[7]*HData$RAD+coef(model2)[8]*HData$TAX


res=residuals(model2)
plot(res,pch=16,col='blue',ylim = c(-60,60))
abline(a=-10*sd(res),b=0,col='red')
abline(a=10*sd(res),b=0,col='red')

##Analyzing the optimal model
ggplot(HData,aes(x=1:length(MEDV),y=MEDV))+
  geom_line(col='blue')+
  geom_point(col='blue')+
  geom_line(aes(x=1:length(MEDV),y=fitted.values(model2)),col='red')+
  geom_point(aes(x=1:length(MEDV),y=fitted.values(model2)),col='red')+
  xlab("Observation")+ 
  ylab("MEDV")+
  ggtitle('Actual ("blue") vs Fitted ("red") Values')

res=residuals(model2)
plot(res,pch=16,col='blue',ylim = c(-70,70))
abline(a=-10*sd(res),b=0,col='red')
abline(a=10*sd(res),b=0,col='red')

plot(density(res))

##Preidict the data with model

data1<-c(0.03221,4,4.21,0.432,6.121,64.2,4.1300,3,254)
data2<-c(0.06733,5,7.21,0.443,6.783,67.8,4.9832,2,267)
data3<-c(0.04211,0,3.33,0.454,6.345,62.5,5.0322,2,231)
data4<-c(0.05328,8,2.65,0.476,6.754,59.8,6.0412,1,255)


pred1=coef(model2)[1]*data1[1]+coef(model2)[2]*data1[2]+coef(model2)[3]*data1[4]+coef(model2)[4]*data1[5]+coef(model2)[5]*data1[6]+coef(model2)[6]*data1[7]+coef(model2)[7]*data1[8]+coef(model2)[8]*data1[9]
pred2=coef(model2)[1]*data2[1]+coef(model2)[2]*data2[2]+
  coef(model2)[3]*data2[4]+coef(model2)[4]*data2[5]+
  coef(model2)[5]*data2[6]+coef(model2)[6]*data2[7]+
  coef(model2)[7]*data2[8]+coef(model2)[8]*data2[9]

pred3=coef(model2)[1]*data3[1]+coef(model2)[2]*data3[2]+
  coef(model2)[3]*data3[4]+coef(model2)[4]*data3[5]+
  coef(model2)[5]*data3[6]+coef(model2)[6]*data3[7]+
  coef(model2)[7]*data3[8]+coef(model2)[8]*data3[9]

pred4=coef(model2)[1]*data4[1]+coef(model2)[2]*data4[2]+
  coef(model2)[3]*data4[4]+coef(model2)[4]*data4[5]+
  coef(model2)[5]*data4[6]+coef(model2)[6]*data4[7]+
  coef(model2)[7]*data4[8]+coef(model2)[8]*data4[9]

###part 2

rm(list = ls())
library(rpart)
library(rpart.plot)
library(caTools)
library(class)
library(ggplot2)
library(dplyr)

library(NbClust)
library(purrr)
library(corrplot)
library(tidyverse)

#import the data
ThData<-read.csv('Thyroid_data.csv',header = TRUE,sep = ',')

#check the data
str(ThData)
summary(ThData)
any(is.na(ThData))

#visulaize the data
plot(ThData)

table(ThData$CLASS)
ggplot(ThData,aes(x=CLASS))+
  geom_histogram()
#visulaize the distribution of the factor variables
ThData %>% 
  as_data_frame() %>% 
  select_if(is.numeric) %>% 
  gather(-CLASS,key = "variable",value = "value") %>% 
  ggplot(aes(x=value,fill=as.factor(CLASS)))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")


##another way to check the data visually

ggplot(ThData,aes(x=MAD.TSH,fill=as.factor(CLASS)))+
  geom_histogram(bins = 50)+
  scale_x_continuous(breaks = seq(-5,60,5))
ggplot(ThData,aes(x=T3,fill=as.factor(CLASS)))+
  geom_histogram(bins = 50)+
  scale_x_continuous(breaks = seq(60,200,5))
ggplot(ThData,aes(x=TSH,fill=as.factor(CLASS)))+
  geom_histogram(bins = 50)+
  scale_x_continuous(breaks = seq(-5,60,5))
ggplot(ThData,aes(x=TST,fill=as.factor(CLASS)))+
  geom_histogram(bins = 50)+
  scale_x_continuous(breaks = seq(-5,30,5))
ggplot(ThData,aes(x=TSTR,fill=as.factor(CLASS)))+
  geom_histogram(bins = 50)+
  scale_x_continuous(breaks = seq(-5,20,5))


##normalize the data
clss<-ThData %>% 
  select(1)
data_norm<-function(x){((x-min(x))/(max(x)-min(x)))}
ThData_norm<-as.data.frame(lapply(ThData[-1], data_norm))
summary(ThData_norm)

ThData_norm<-bind_cols(clss,ThData_norm)


##decision tree
sample<-sample.split(ThData_norm$CLASS,SplitRatio = 0.7)
train<-subset(ThData_norm,sample==TRUE)
test<-subset(ThData_norm,sample==FALSE)

##minbucket=1
treemdl1<-rpart(CLASS~.,data=train,method='class',minbucket=1)
rpart.plot(treemdl1)


treetrain<-predict(treemdl1,train,type = 'class')
tabtrain<-table(train$CLASS,treetrain)
tabtrain

##use model on test data
treetest<-predict(treemdl1,test,type = 'class')
tabtest<-table(test$CLASS,treetest)
tabtest
##------test with different minbucket
##minbucket=5
treemdl5<-rpart(CLASS~.,data=train,method='class',minbucket=15)
rpart.plot(treemdl5)


treetrain<-predict(treemdl5,train,type = 'class')
tabtrain<-table(train$CLASS,treetrain)
tabtrain

##use model on test data
treetest<-predict(treemdl5,test,type = 'class')
tabtest<-table(test$CLASS,treetest)
tabtest

##KNN
##normalize the data
clss<-ThData %>% 
  select(1)
data_norm<-function(x){((x-min(x))/(max(x)-min(x)))}
ThData_norm<-as.data.frame(lapply(ThData[-1], data_norm))
summary(ThData_norm)

ThData_norm<-bind_cols(clss,ThData_norm)


sample<-sample.split(ThData_norm$CLASS,SplitRatio = 0.7)
train<-subset(ThData_norm,sample==TRUE)
test<-subset(ThData_norm,sample==FALSE)

trainpred<-knn(train[,2:6],train[,2:6],factor(train$CLASS),10)
testpred<-knn(train[,2:6],test[,2:6],factor(train$CLASS),10)

#conduct matrix
tab_train=table(train$CLASS,trainpred)
tab_train
tab_test=table(test$CLASS,testpred)
tab_test

#check the error

KNNtrain<-1-mean(train$CLASS==trainpred)
KNNtrain
KNNtest<-1-mean(test$CLASS==testpred)
KNNtest

#check the different number of neighbours
KNNtrain=matrix(0,nrow =50,ncol = 200 )
KNNtest=matrix(0,nrow =50,ncol = 200 )
for (i in 1:50){
  for (j in 1:200) {
    trainpred<-knn(train[,2:6],train[,2:6],factor(train$CLASS),i)
    testpred<-knn(train[,2:6],test[,2:6],factor(train$CLASS),i)
    KNNtrain[i,j]=1-mean(train$CLASS==trainpred)
    KNNtest[i,j]=1-mean(test$CLASS==testpred)
    
  }
}
KNNtrain<-rowMeans(KNNtrain)
KNNtest<-rowMeans(KNNtest)

resultsdf=data.frame(neighbors=1:50,train_perf=KNNtrain,test_perf=KNNtest)
ggplot(resultsdf,aes(x=neighbors,y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors,y=test_perf,col='red'))



##KNN without scaling
##normalize the data


sample<-sample.split(ThData$CLASS,SplitRatio = 0.7)
train<-subset(ThData,sample==TRUE)
test<-subset(ThData,sample==FALSE)

#check the different number of neighbours
KNNtrain=matrix(0,nrow =50,ncol = 200 )
KNNtest=matrix(0,nrow =50,ncol = 200 )
for (i in 1:50){
  for (j in 1:200) {
    trainpred<-knn(train[,2:6],train[,2:6],factor(train$CLASS),i)
    testpred<-knn(train[,2:6],test[,2:6],factor(train$CLASS),i)
    KNNtrain[i,j]=1-mean(train$CLASS==trainpred)
    KNNtest[i,j]=1-mean(test$CLASS==testpred)
    
  }
}
KNNtrain<-rowMeans(KNNtrain)
KNNtest<-rowMeans(KNNtest)

resultsdf=data.frame(neighbors=1:50,train_perf=KNNtrain,test_perf=KNNtest)
ggplot(resultsdf,aes(x=neighbors,y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors,y=test_perf,col='red'))
