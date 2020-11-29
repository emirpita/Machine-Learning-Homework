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

library(randomForest)
library(gbm)
library(ROSE)

# Ucitavanje podataka (izvornih) u data frame
attrition_train<-read.csv("Data/attrition_train.csv", header=TRUE)
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 

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

atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
atrain$Attrition <- as.character(atrain$Attrition)

boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=10000)

boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
confusionMatrix(as.factor(boost.pred),as.factor(atest$Attrition), positive = "1")


#Druga iteracija#
atrain$Attrition<-ifelse(atrain$Attrition=="Yes",1,0)
atest$Attrition<-ifelse(atest$Attrition=="Yes",1,0)

atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
atrain$Attrition <- as.character(atrain$Attrition)
boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=10000)

boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
confusionMatrix(as.factor(boost.pred),as.factor(atest$Attrition), positive = "1")

#TRECA ITERACIJA
#oversampling
data_over<-ovun.sample(Attrition~., data = atrain, 
                       method="over", N=1960)$data
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
atrain$Attrition <- as.character(atrain$Attrition)
over.fit <- gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
over.pred <- predict.gbm(over.fit, atest, type = "response", n.trees=10000)
over.pred<-ifelse(over.pred>mean(over.pred),1,0)
confusionMatrix(as.factor(over.pred),as.factor(atest$Attrition), positive = "1")


#CETVRTA ITERACIJA
#undersampling
data_under<-ovun.sample(Attrition~., data = atrain, 
                        method="under", N=600, seed = 1)$data

atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
atrain$Attrition <- as.character(atrain$Attrition)
under.fit <- gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
under.pred <- predict.gbm(under.fit, atest, type = "response", n.trees=10000)
under.pred<-ifelse(over.pred>mean(over.pred),1,0)
confusionMatrix(as.factor(under.pred),as.factor(atest$Attrition), positive = "1")

