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

#provjera koje varijable imaju prazne redove
colnames(atrain)[colSums(is.na(atrain)) > 0]

#provjera tipova tih varijabli
typeof(atrain$Department)
typeof(atrain$EducationField)
typeof(atrain$JobRole)
typeof(atrain$Over18)
typeof(atrain$PercentSalaryHike)
typeof(atrain$StandardHours)
typeof(atrain$TrainingTimesLastYear)

#popunjavamo ih sa mode vrijednostima(možemo staviti i neku drugu funkciju)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

atrain[is.na(atrain$Department),'Department']<-getmode(atrain[!is.na(atrain$Department),'Department'])
atrain[is.na(atrain$EducationField),'EducationField']<-getmode(atrain[!is.na(atrain$EducationField),'EducationField'])
atrain[is.na(atrain$JobRole),'JobRole']<-getmode(atrain[!is.na(atrain$JobRole),'JobRole'])
atrain[is.na(atrain$Over18),'Over18']<-getmode(atrain[!is.na(atrain$Over18),'Over18'])
atrain[is.na(atrain$PercentSalaryHike),'PercentSalaryHike']<-getmode(atrain[!is.na(atrain$PercentSalaryHike),'PercentSalaryHike'])
atrain[is.na(atrain$StandardHours),'StandardHours']<-getmode(atrain[!is.na(atrain$StandardHours),'StandardHours'])
atrain[is.na(atrain$TrainingTimesLastYear),'TrainingTimesLastYear']<-getmode(atrain[!is.na(atrain$TrainingTimesLastYear),'TrainingTimesLastYear'])

