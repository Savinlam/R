library(dplyr)
library(ggplot2)
library(NbClust)
library(purrr)
library(corrplot)
library(tidyverse)

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



