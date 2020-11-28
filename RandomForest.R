library(ggplot2)
library(statip)
library(MASS)
library(randomForest)
library(caret)
library(rpart)
library(gbm)
library(caret)
library(ROSE)

getwd()
setwd("E:/MU/Zadaca1/")

atrain<-read.csv("Data/attrition_train.csv", header=TRUE)
atrain<-read.csv("Data/attrition_train.csv", header=TRUE)

#potrebno popuniti nedostajuce vrijednosti

#prva itercija
atrain$Attrition <- as.factor(atrain$Attrition)
atrain_forest <- randomForest(Attrition~.-X-EmployeeCount, data=atrain, mtry=3, importance =TRUE, ntree=3000)
atrain_forest

rforest.pred<-predict(atrain_forest,atest,type="class")
confusionMatrix(rforest.pred,atest$Attrition)
importance(atrain_forest)


#druga iteracija - nakon transformacije i skaliranja
atrain_forest_second <- randomForest(Attrition~., data=atrain, mtry=3, importance =TRUE, ntree=3000)
atrain_forest_second

rforest.predSecond<-predict(atrain_forest_second,atest,type="class")
confusionMatrix(rforest.predSecond,atest$Attrition)
importance(atrain_forest_second)


#treca iteracija
#oversampling
data_over<-ovun.sample(Attrition~., data = atrain, 
                       method="over", N=1960)$data

over.fit <- randomForest(Attrition~., data=data_over, mtry=3, importance =TRUE, ntree=3000)
over.pred <- predict(over.fit, newdata = atest, type="class")
confusionMatrix(over.pred,atest$Attrition)


#cetvrta iteracija
#undersampling
data_under<-ovun.sample(Attrition~., data = atrain, 
                        method="under", N=600, seed = 1)$data

under.fit <- randomForest(Attrition~.-X-Over18, data=data_under, mtry=3, importance =TRUE, ntree=3000)
under.pred <- predict(under.fit, newdata = atest, type="class")
confusionMatrix(under.pred,atest$Attrition)
