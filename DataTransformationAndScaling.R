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

#funkcije za transformaciju i skaliranje varijabli
# min-max
min_max<-function(x){
  transformed_x<-(x-min(x))/(max(x)-min(x))
}
# z-score transformation
z_score<-function(x){
  z<-(x-mean(x))/sd(x)
}

# decimal scaling
decimal<-function(x){
  max_x<-max(x)
  max_x<-as.character(max_x)
  j<-nchar(max_x)
  y<-x/(10^j)
  return(y)
}

#distribucije varijabli
prop.table(table(atrain$X)) #102
prop.table(table(atrain$Attrition)) #character
prop.table(table(atrain$BusinessTravel))
prop.table(table(atrain$DailyRate))#vrlika distribucija
prop.table(table(atrain$DistanceFromHome))
prop.table(table(atrain$Education))
prop.table(table(atrain$EducationField))
prop.table(table(atrain$EmployeeCount))
prop.table(table(atrain$EmployeeNumber))#102
prop.table(table(atrain$EnvironmentSatisfaction))
prop.table(table(atrain$Gender))
prop.table(table(atrain$HourlyRate))#onako
prop.table(table(atrain$JobInvolvement))
prop.table(table(atrain$JobLevel))
prop.table(table(atrain$JobRole))
prop.table(table(atrain$JobSatisfaction))
prop.table(table(atrain$MaritalStatus))
prop.table(table(atrain$MonthlyIncome))#oggromna
prop.table(table(atrain$MonthlyRate))#ogromna
prop.table(table(atrain$NumCompaniesWorked))
prop.table(table(atrain$Over18))
prop.table(table(atrain$OverTime))
prop.table(table(atrain$PercentSalaryHike))
prop.table(table(atrain$PerformanceRating))
prop.table(table(atrain$StandardHours))
prop.table(table(atrain$StockOptionLevel))
prop.table(table(atrain$TotalWorkingYears))
prop.table(table(atrain$TrainingTimesLastYear))
prop.table(table(atrain$WorkLifeBalance))
prop.table(table(atrain$YearsAtCompany))
prop.table(table(atrain$YearsInCurrentRole))
prop.table(table(atrain$YearsSinceLastPromotion))
prop.table(table(atrain$YearsWithCurrManager))
prop.table(table(atrain$BirthDate))#onako

#transformisat cemo varijable koje imaju veliku distribuciju
#to su varijable:  DailyRate, EmployeeNumber, HourlyRate, MonthlyIncome, MonthlyRate

#nakon analize utvrdjeno je da najbolji rezultat dobijamo skaliranjem sa funkcijom z_score
atrain[,c(4,10,20)]<-lapply(atrain[,c(4,10,20)],z_score)
summary(atrain)
atrain<-read.csv("Data/attrition_train.csv", header=TRUE)

#provjera smaknutosti varijabli
skewness(atrain$HourlyRate, na.rm = TRUE)
skewness(atrain$MonthlyIncome, na.rm = TRUE)
skewness(atrain$NumCompaniesWorked, na.rm = TRUE)
skewness(atrain$TotalWorkingYears, na.rm = TRUE)
skewness(atrain$YearsAtCompany, na.rm = TRUE)
skewness(atrain$YearsSinceLastPromotion, na.rm = TRUE)

hist(atrain$PercentSalaryHike)

#primjena log funkcije na varijable koje su smaknute
atrain$HourlyRate<-log(atrain$HourlyRate)
atrain$MonthlyIncome<-log(atrain$MonthlyIncome)

