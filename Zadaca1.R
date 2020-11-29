library(ggplot2)
library(statip)
library(Hmisc)


attrition_train<-read.csv("Data/attrition_train.csv", header=TRUE)

#tip attrition_train -> lista
typeof(attrition_train)

#ispisivanje liste
attrition_train

#funckija za varijable
str(attrition_train)

#TIPOVI VARIJABLI
typeof(attrition_train$X)
typeof(attrition_train$Attrition)
typeof(attrition_train$BusinessTravel)
typeof(attrition_train$DailyRate)
typeof(attrition_train$Department)
typeof(attrition_train$DistanceFromHome)
typeof(attrition_train$Education)
typeof(attrition_train$EducationField)
typeof(attrition_train$EmployeeCount)
typeof(attrition_train$EmployeeNumber)
typeof(attrition_train$EnvironmentSatisfaction)
typeof(attrition_train$Gender)
typeof(attrition_train$HourlyRate)
typeof(attrition_train$JobInvolvement)
typeof(attrition_train$JobLevel)
typeof(attrition_train$JobRole)
typeof(attrition_train$JobSatisfaction)
typeof(attrition_train$MaritalStatus)
typeof(attrition_train$MonthlyIncome)
typeof(attrition_train$MonthlyRate)
typeof(attrition_train$NumCompaniesWorked)
typeof(attrition_train$Over18)
typeof(attrition_train$OverTime)
typeof(attrition_train$PercentSalaryHike)
typeof(attrition_train$PerformanceRating)
typeof(attrition_train$RelationshipSatisfaction)
typeof(attrition_train$StandardHours)
typeof(attrition_train$StockOptionLevel)
typeof(attrition_train$TotalWorkingYears)
typeof(attrition_train$TrainingTimesLastYear)
typeof(attrition_train$WorkLifeBalance)
typeof(attrition_train$YearsAtCompany)
typeof(attrition_train$YearsInCurrentRole)
typeof(attrition_train$YearsSinceLastPromotion)
typeof(attrition_train$YearsWithCurrManager)
typeof(attrition_train$BirthDate)

#DISTRIBUCIJA
prop.table(table(attrition_train$X))
prop.table(table(attrition_train$Attrition))
prop.table(table(attrition_train$BusinessTravel))
prop.table(table(attrition_train$DailyRate))
prop.table(table(attrition_train$Department))
prop.table(table(attrition_train$DistanceFromHome))
prop.table(table(attrition_train$Education))
prop.table(table(attrition_train$EducationField))
prop.table(table(attrition_train$EmployeeCount))
prop.table(table(attrition_train$EmployeeNumber))
prop.table(table(attrition_train$EnvironmentSatisfaction))
prop.table(table(attrition_train$Gender))
prop.table(table(attrition_train$HourlyRate))
prop.table(table(attrition_train$JobInvolvement))
prop.table(table(attrition_train$JobLevel))
prop.table(table(attrition_train$JobRole))
prop.table(table(attrition_train$JobSatisfaction))
prop.table(table(attrition_train$MaritalStatus))
prop.table(table(attrition_train$MonthlyIncome))
prop.table(table(attrition_train$MonthlyRate))
prop.table(table(attrition_train$NumCompaniesWorked))
prop.table(table(attrition_train$Over18))
prop.table(table(attrition_train$OverTime))
prop.table(table(attrition_train$PercentSalaryHike))
prop.table(table(attrition_train$PerformanceRating))
prop.table(table(attrition_train$RelationshipSatisfaction))
prop.table(table(attrition_train$StandardHours))
prop.table(table(attrition_train$StockOptionLevel))
prop.table(table(attrition_train$TotalWorkingYears))
prop.table(table(attrition_train$TrainingTimesLastYear))
prop.table(table(attrition_train$WorkLifeBalance))
prop.table(table(attrition_train$YearsAtCompany))
prop.table(table(attrition_train$YearsInCurrentRole))
prop.table(table(attrition_train$YearsSinceLastPromotion))
prop.table(table(attrition_train$YearsWithCurrManager))
prop.table(table(attrition_train$BirthDate))

#DESKRIPTIVNA STATISTIKA
#summary(dat) - daje i min i max i medianu i mean i 1st quantile i 3rd qu
summary(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')])

#min
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,min)

#max
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,max)

#range
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,range)

#razlika izmedu gornje i donje vrijednosti (ne mogu briti vrijednosti koje imaju NAN)
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,IQR)

#quantile
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,quantile)

#midrange
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, midrange)

#Prosjecna vrijednost
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, mean)

#Variansa
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, var)

#Standardna devijacija
apply(attrition_train[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, sd)


#Veza izmedu varijabli
#Attrition
attrition_train$Attrition<-ifelse(attrition_train$Attrition=="Yes",1,0)

cor<-rcorr(as.matrix(attrition_train[,c("X", "Attrition", "DistanceFromHome",        
                               "EmployeeNumber", "Education",               
                               "HourlyRate", "JobLevel","EnvironmentSatisfaction","JobInvolvement","WorkLifeBalance","RelationshipSatisfaction","PerformanceRating", "JobSatisfaction","StockOptionLevel",       
                               "MonthlyIncome","MonthlyRate","NumCompaniesWorked","TotalWorkingYears","YearsAtCompany","YearsInCurrentRole",      
                               "YearsSinceLastPromotion","YearsWithCurrManager" )]))
