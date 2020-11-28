#ucitamo optimalni random forest

library(ISLR)
library(tree)
library(OneR)
library(Hmisc)
library(gmodels)
library(rJava)
library(RWeka)
library(caret)
library(e1071)
library(MASS)
library(rpart)
library(mice)
library(VIM)
library(ggplot2)
library(statip)


#Ripper
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
jRip.attrition<-JRip(Attrition~., data=atrain)
JRip.pred<-predict(jRip.attrition, atest, type ="class")
t<-table(JRip.pred, atest$Attrition)
(t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
summary(jRip.attrition)
#display rules:
jRip.attrition


