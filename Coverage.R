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
library(rpart.plot)


#Ripper
attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
jRip.attrition<-JRip(Attrition~., data=attrition_train)
JRip.pred<-predict(jRip.attrition, attrition_test, type ="class")
t<-table(JRip.pred, attrition_test$Attrition)
(t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
summary(jRip.attrition)
#display rules:
jRip.attrition


#pravila preko rpart
attrition_train$Attrition <- as.factor(attrition_train$Attrition)
attrition_test$Attrition <- as.factor(attrition_test$Attrition)
rpart.attrition <- rpart(Attrition~., data=attrition_train)
rpart.pred <- predict(rpart.attrition, attrition_test, type="class")
confusionMatrix(rpart.pred, attrition_test$Attrition)
plot(rpart.attrition)
text(rpart.attrition,cex=.6, pos=1, offset=0.7)
rpart.attrition
rpart.rules(rpart.attrition)



