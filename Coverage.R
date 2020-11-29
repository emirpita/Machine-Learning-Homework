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
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
jRip.attrition<-JRip(Attrition~., data=atrain)
JRip.pred<-predict(jRip.attrition, atest, type ="class")
t<-table(JRip.pred, atest$Attrition)
(t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
summary(jRip.attrition)
#display rules:
jRip.attrition


#pravila za rpart
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 

atrain$Attrition <- as.factor(atrain$Attrition)
atest$Attrition <- as.factor(atest$Attrition)
rpart.attrition <- rpart(Attrition~., data=atrain)
rpart.pred <- predict(rpart.attrition, atest, type="class")
confusionMatrix(rpart.pred, atest$Attrition)
plot(rpart.attrition)
text(rpart.attrition,cex=.6, pos=1, offset=0.7)
rpart.attrition
rpart.rules(rpart.attrition)





