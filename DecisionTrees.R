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


# Prva iteracija: istreniramo drvo sa postojecim podacima (izvornim):
# Ocito je da se varijabla X treba izbaciti: predstavlja neku vrstu ID-a, ali
# cemo to uraditi u drugoj iteraciji

# Drvo odlucivanja 1: koristimo rpart, jer moze raditi u nekim slucajevima
# i sa nedostajucim vrijednostima, sto je ustanovljeno pri analizi varijabli
rpart.attrition <- rpart(Attrition~., data=atrain)
rpart.pred <- predict(rpart.attrition, atest, type="class")
confusionMatrix(rpart.pred, atest$Attrition) 
plot(rpart.attrition)
text(rpart.attrition,cex=.6, pos=1, offset=0.7)
rpart.attrition


# Drvo odlucivanja 2: koristimo paket tree
attrition.tree<-tree(Attrition~., data=atrain)
tree.pred<-predict(attrition.tree, atest, type ="class") 
confusionMatrix(tree.pred, atest$Attrition)
plot(attrition.tree)
text(attrition.tree,cex=.6, pos=1, offset=0.7)
attrition.tree

# U ovoj tacki izvrsimo transformacije
# 1. Dopuna nedostajucih vrijednosti (AddMissingValues.R)
# 2. Konsolidacija (Consolidate.R)
# 3. Transformacija i skaliranje (DataTransformationAndScaling.R)
# Odnosno, skripta GlobalTransformation
# U skripti ViewDataInfo su info o data framovima


# Iteracija 2
# rpart drvo
rpart.attrition <- rpart(Attrition~., data=atrain)
rpart.pred <- predict(rpart.attrition, atest, type="class")
confusionMatrix(rpart.pred, atest$Attrition)
plot(rpart.attrition)
text(rpart.attrition,cex=.6, pos=1, offset=0.7)
rpart.attrition
# Komentar: Sve poraslo, greska opala, 

# tree drvo
attrition.tree<-tree(Attrition~., data=atrain)
tree.pred<-predict(attrition.tree, atest, type ="class") 
confusionMatrix(tree.pred, atest$Attrition)
plot(attrition.tree)
text(attrition.tree,cex=.6, pos=1, offset=0.7)
attrition.tree
#Komentar: Sve se poboljsalo znatno

#oversampling
dec.data_over<-ovun.sample(Attrition~., data = atrain, method="over", N=10000)$data
#undersampling
dec.data_under<-ovun.sample(Attrition~., data = atrain, method="under", N=400, seed = 1)$data

#Drvo 1 rpart
rpart.over.fit <- rpart(Attrition~., data=dec.data_over)
rpart.over.pred <- predict(rpart.over.fit, atest, type="class")
confusionMatrix(rpart.over.pred, atest$Attrition)

rpart.under.fit <- rpart(Attrition~., data=dec.data_under)
rpart.under.pred <- predict(rpart.under.fit, atest, type="class")
confusionMatrix(rpart.under.pred, atest$Attrition)

# Drvo 2 tree
tree.over.fit<-tree(Attrition~., data=dec.data_over)
tree.over.pred<-predict(tree.over.fit, atest, type ="class") 
confusionMatrix(tree.over.pred, atest$Attrition)

tree.under.fit<-tree(Attrition~., data=dec.data_over)
tree.under.pred<-predict(tree.under.fit, atest, type ="class") 
confusionMatrix(tree.under.pred, atest$Attrition)
