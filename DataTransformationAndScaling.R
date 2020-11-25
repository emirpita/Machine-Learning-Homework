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
#to su varijable: X, DailyRate, EmployeeNumber, HourlyRate, MonthlyIncome, MonthlyRate, BirthDate
#za sve ove varijable kreirat cemo nove varijable koje ce sadrzavati njihove transformacije
#mozemo sve varijable transformisati funkcijom main_max za pocetak
#ako zelimo izmijeniti ove varijable u pocetnom setu, a ne praviti nove dodatne varijable onda mozemo 
#koristiti funkcju lapply kao u lv3

atrain$X1<-min_max(atrain$X)
atrain$DailyRate1<-min_max(atrain$DailyRate)
atrain$EmployeeNumber1<-min_max(atrain$EmployeeNumber)
atrain$HourlyRate1<-min_max(atrain$HourlyRate)
atrain$MonthlyIncome1<-min_max(atrain$MonthlyIncome)
atrain$MonthlyRate1<-min_max(atrain$MonthlyRate)
atrain$BirthDate1<-min_max(atrain$BirthDate)

