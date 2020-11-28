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

skewness(atrain$HourlyRate, na.rm = TRUE)
skewness(atrain$MonthlyIncome, na.rm = TRUE)
skewness(atrain$NumCompaniesWorked, na.rm = TRUE)
skewness(atrain$TotalWorkingYears, na.rm = TRUE)
skewness(atrain$YearsAtCompany, na.rm = TRUE)
skewness(atrain$YearsSinceLastPromotion, na.rm = TRUE)

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