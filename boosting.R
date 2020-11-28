library(MASS)
library(caret)
library(rpart)
library(mice)
library(VIM)
library(ggplot2)
library(statip)
library(e1071)
library(ISLR)
library(tree)
library(ipred)

# Ucitavanje podataka (izvornih) u data frame
atrain<-read.csv("Data/attrition_train.csv", header=TRUE)
atest<-read.csv("Data/attrition_test.csv", header=TRUE)

# Kratka analiza podataka
names(atrain)
dim(atrain)
summary(atrain)

names(atest)
dim(atest)
summary(atest)

str(atrain)
str(atest)

# Provjera nedostajucih vrijednosti
md.pattern(atrain)
aggr(atrain)

md.pattern(atest)
aggr(atest) # Iz ovog je ocito da atest nema nedostajucih vrijednosti, sto je olaksanje

#Boosting prva iteracija
atrain$Attrition<-ifelse(atrain$Attrition=="Yes",1,0)
atest$Attrition<-ifelse(atest$Attrition=="Yes",1,0)
atrain$Attrition <- as.character(atrain$Attrition)

atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=10000)

boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
confusionMatrix(as.factor(boost.pred),as.factor(atest$Attrition), positive = "1")


#Druga iteracija#
atrain$Attrition<-ifelse(atrain$Attrition=="Yes",1,0)
atest$Attrition<-ifelse(atest$Attrition=="Yes",1,0)
atrain$Attrition <- as.character(atrain$Attrition)

boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=10000)

boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
confusionMatrix(as.factor(boost.pred),as.factor(atest$Attrition), positive = "1")


