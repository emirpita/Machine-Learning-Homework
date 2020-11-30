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

# Kratka analiza podataka
names(attrition_train)
dim(attrition_train)
summary(attrition_train)

names(atest)
dim(atest)
summary(atest)

str(attrition_train)
str(atest)

# Provjera nedostajucih vrijednosti
md.pattern(attrition_train)
aggr(attrition_train)

md.pattern(atest)
aggr(atest) # Iz ovog je ocito da atest nema nedostajucih vrijednosti, sto je olaksanje

#provjera koje varijable imaju prazne redove
colnames(attrition_train)[colSums(is.na(attrition_train)) > 0]

skewness(attrition_train$HourlyRate, na.rm = TRUE)
skewness(attrition_train$MonthlyIncome, na.rm = TRUE)
skewness(attrition_train$NumCompaniesWorked, na.rm = TRUE)
skewness(attrition_train$TotalWorkingYears, na.rm = TRUE)
skewness(attrition_train$YearsAtCompany, na.rm = TRUE)
skewness(attrition_train$YearsSinceLastPromotion, na.rm = TRUE)

#distribucije varijabli
prop.table(table(attrition_train$X)) #102
prop.table(table(attrition_train$Attrition)) #character
prop.table(table(attrition_train$BusinessTravel))
prop.table(table(attrition_train$DailyRate))#vrlika distribucija
prop.table(table(attrition_train$DistanceFromHome))
prop.table(table(attrition_train$Education))
prop.table(table(attrition_train$EducationField))
prop.table(table(attrition_train$EmployeeCount))
prop.table(table(attrition_train$EmployeeNumber))#102
prop.table(table(attrition_train$EnvironmentSatisfaction))
prop.table(table(attrition_train$Gender))
prop.table(table(attrition_train$HourlyRate))#onako
prop.table(table(attrition_train$JobInvolvement))
prop.table(table(attrition_train$JobLevel))
prop.table(table(attrition_train$JobRole))
prop.table(table(attrition_train$JobSatisfaction))
prop.table(table(attrition_train$MaritalStatus))
prop.table(table(attrition_train$MonthlyIncome))#oggromna
prop.table(table(attrition_train$MonthlyRate))#ogromna
prop.table(table(attrition_train$NumCompaniesWorked))
prop.table(table(attrition_train$Over18))
prop.table(table(attrition_train$OverTime))
prop.table(table(attrition_train$PercentSalaryHike))
prop.table(table(attrition_train$PerformanceRating))
prop.table(table(attrition_train$StandardHours))
prop.table(table(attrition_train$StockOptionLevel))
prop.table(table(attrition_train$TotalWorkingYears))
prop.table(table(attrition_train$TrainingTimesLastYear))
prop.table(table(attrition_train$WorkLifeBalance))
prop.table(table(attrition_train$YearsAtCompany))
prop.table(table(attrition_train$YearsInCurrentRole))
prop.table(table(attrition_train$YearsSinceLastPromotion))
prop.table(table(attrition_train$YearsWithCurrManager))
prop.table(table(attrition_train$BirthDate))#onako

#transformisat cemo varijable koje imaju veliku distribuciju
#to su varijable:  DailyRate, EmployeeNumber, HourlyRate, MonthlyIncome, MonthlyRate

#nakon analize utvrdjeno je da najbolji rezultat dobijamo skaliranjem sa funkcijom z_score