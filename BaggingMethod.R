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
atrain<-read.csv("Data/attrition_train.csv", header=TRUE)
atest<-read.csv("Data/attrition_test.csv", header=TRUE)

# Postavljanje faktor varijabli
atrain$Attrition <- as.factor(atrain$Attrition)
atest$Attrition <- as.factor(atest$Attrition)

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

#PRVA ITERACIJA
#potrebno za prvu iteraciju
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
#jedan nacin
atrain.bag <- bagging(
  formula = Attrition ~ .,
  data = atrain,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)
#drugi nacin - ista stvar (nbagg = 25 po defaultu)
atrain.bag <- bagging(Attrition ~ ., data = atrain, coob=TRUE)
#predikcija
atrain.pred<-predict(atrain.bag,newdata=atest, type="class")
confusionMatrix(atrain.pred,atest$Attrition)


#DRUGA ITERACIJA
#nakon transformacije, skaliranja, normalizacije i popunjavanja nedostajućih vrijednosti
#bagging metoda preko randomForest funkcije
atrain.bag<-randomForest(Attrition~.-X, data=atrain, mtry=13, importance =TRUE, ntree=3000)
#predikcija
atrain.pred<-predict(atrain.bag,newdata=atest, type="class")
confusionMatrix(atrain.pred,atest$Attrition)

#TRECA ITERACIJA
#oversampling
data_over<-ovun.sample(Attrition~., data = atrain, 
                       method="over", N=1960)$data

over.fit <- randomForest(Attrition~., data=data_over, mtry=13, importance =TRUE, ntree=3000)
over.pred <- predict(over.fit, newdata = atest, type="class")
confusionMatrix(over.pred,atest$Attrition)


#CETVRTA ITERACIJA
#undersampling
data_under<-ovun.sample(Attrition~., data = atrain, 
                        method="under", N=600, seed = 1)$data

under.fit <- randomForest(Attrition~.-X, data=data_under, mtry=13, importance =TRUE, ntree=3000)
under.pred <- predict(under.fit, newdata = atest, type="class")
confusionMatrix(under.pred,atest$Attrition)

