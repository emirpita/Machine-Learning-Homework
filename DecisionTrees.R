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

# Ucitavanje podataka (izvornih) u data frame
atrain<-read.csv("data/attrition_train.csv", header=TRUE)
atest<-read.csv("data/attrition_test.csv", header=TRUE)

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

# Drvo odlucivanja 1: koristimo rpart, jer moze raditi u nekim slucajevima
# i sa nedostajucim vrijednostima, sto je ustanovljeno pri analizi varijabli

# Prva iteracija: istreniramo drvo sa postojecim podacima (izvornim):
# Ocito je da se varijabla X treba izbaciti: predstavlja neku vrstu ID-a, ali
# cemo to uraditi u drugoj iteraciji
rpart.attrition <- rpart(Attrition~., data=atrain)
rpart.pred <- predict(rpart.attrition, atest, type="class")

# Konfuzijska matrica na 2 nacina: prvi koristeci confusionMatrix
# drugi koristeci lab 

#confusionMatrix(rpart.attrition, atest$Attrition) 
# Error: `data` and `reference` should be factors with the same levels.

rpart.conf<-table(atest$Attrition,rpart.pred) 

#Tacnost (TP+TN/all):
rpart.acc<-(rpart.conf[1,1]+rpart.conf[2,2])/(sum(rpart.conf))
#Stepen greske (FP+FN/all):
rpart.err<-(rpart.conf[1,2]+rpart.conf[2,1])/(sum(rpart.conf))
#Osjetljivost=Recall (sensitivity) - TPR (TP/Positive):
rpart.sens<-rpart.conf[2,2]/(rpart.conf[2,1]+rpart.conf[2,2])
#Specificnost (specificity) - TNR (TN/Negative):
rpart.spec<-rpart.conf[1,1]/(rpart.conf[1,1]+rpart.conf[1,2])
#Preciznost (TP/TP+FP):
rpart.prec<-rpart.conf[2,2]/(rpart.conf[2,2]+rpart.conf[1,2])

rpart.conf
rpart.acc
rpart.err
rpart.sens
rpart.spec
rpart.prec

plot(rpart.attrition)
text(rpart.attrition,cex=.6, pos=1, offset=0.7)
rpart.attrition


# Drvo odlucivanja 2: koristimo paket tree
attrition.tree<-tree(Attrition~., data=atrain)
tree.pred<-predict(attrition.tree, atest, type ="class") 

# Konfuzijska matrica na 2 nacina: prvi koristeci confusionMatrix
# drugi koristeci lab 

confusionMatrix(rpart.attrition, atest$Attrition)

tree.conf<-table(atest$Attrition,tree.pred) 

#Tacnost (TP+TN/all):
tree.acc<-(tree.conf[1,1]+tree.conf[2,2])/(sum(tree.conf))
#Stepen greske (FP+FN/all):
tree.err<-(tree.conf[1,2]+tree.conf[2,1])/(sum(tree.conf))
#Osjetljivost=Recall (sensitivity) - TPR (TP/Positive):
tree.sens<-tree.conf[2,2]/(tree.conf[2,1]+tree.conf[2,2])
#Specificnost (specificity) - TNR (TN/Negative):
tree.spec<-tree.conf[1,1]/(tree.conf[1,1]+tree.conf[1,2])
#Preciznost (TP/TP+FP):
tree.prec<-tree.conf[2,2]/(tree.conf[2,2]+tree.conf[1,2])

tree.conf
tree.acc
tree.err
tree.sens
tree.spec
tree.prec
 
plot(attrition.tree)
text(attrition.tree,cex=.6, pos=1, offset=0.7)
attrition.tree
