library(ggplot2)
library(statip)
library(Hmisc)

getwd()
setwd("E:/MU/Zadaca1/")

atrain<-read.csv("Data/attrition_train.csv", header=TRUE)

#tip atrain -> lista
typeof(atrain)

#ispisivanje liste
atrain

#funckija za varijable
str(atrain)

#TIPOVI VARIJABLI
typeof(atrain$X)
typeof(atrain$Attrition)
typeof(atrain$BusinessTravel)
typeof(atrain$DailyRate)
typeof(atrain$Department)
typeof(atrain$DistanceFromHome)
typeof(atrain$Education)
typeof(atrain$EducationField)
typeof(atrain$EmployeeCount)
typeof(atrain$EmployeeNumber)
typeof(atrain$EnvironmentSatisfaction)
typeof(atrain$Gender)
typeof(atrain$HourlyRate)
typeof(atrain$JobInvolvement)
typeof(atrain$JobLevel)
typeof(atrain$JobRole)
typeof(atrain$JobSatisfaction)
typeof(atrain$MaritalStatus)
typeof(atrain$MonthlyIncome)
typeof(atrain$MonthlyRate)
typeof(atrain$NumCompaniesWorked)
typeof(atrain$Over18)
typeof(atrain$OverTime)
typeof(atrain$PercentSalaryHike)
typeof(atrain$PerformanceRating)
typeof(atrain$StandardHours)
typeof(atrain$StockOptionLevel)
typeof(atrain$TotalWorkingYears)
typeof(atrain$TrainingTimesLastYear)
typeof(atrain$WorkLifeBalance)
typeof(atrain$YearsAtCompany)
typeof(atrain$YearsInCurrentRole)
typeof(atrain$YearsSinceLastPromotion)
typeof(atrain$YearsWithCurrManager)
typeof(atrain$BirthDate)

#DISTRIBUCIJA
prop.table(table(atrain$X))
prop.table(table(atrain$Attrition))
prop.table(table(atrain$BusinessTravel))
prop.table(table(atrain$DailyRate))
prop.table(table(atrain$DistanceFromHome))
prop.table(table(atrain$Education))
prop.table(table(atrain$EducationField))
prop.table(table(atrain$EmployeeCount))
prop.table(table(atrain$EmployeeNumber))
prop.table(table(atrain$EnvironmentSatisfaction))
prop.table(table(atrain$Gender))
prop.table(table(atrain$HourlyRate))
prop.table(table(atrain$JobInvolvement))
prop.table(table(atrain$JobLevel))
prop.table(table(atrain$JobRole))
prop.table(table(atrain$JobSatisfaction))
prop.table(table(atrain$MaritalStatus))
prop.table(table(atrain$MonthlyIncome))
prop.table(table(atrain$MonthlyRate))
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
prop.table(table(atrain$BirthDate))

#DESKRIPTIVNA STATISTIKA
#summary(dat) - daje i min i max i medianu i mean i 1st quantile i 3rd qu
summary(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')])

#min
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,min)

#max
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,max)

#range
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,range)

#razlika izmedu gornje i donje vrijednosti (ne mogu briti vrijednosti koje imaju NAN)
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,IQR)

#quantile
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2,quantile)

#midrange
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, midrange)

#Prosjecna vrijednost
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, mean)

#Variansa
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, var)

#Standardna devijacija
apply(atrain[,c('X', 'DailyRate', 'DistanceFromHome', 'EmployeeCount', 'EmployeeNumber', 'HourlyRate', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'TotalWorkingYears', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')],2, sd)


#Veza izmedu varijabli
#https://dzchilds.github.io/eda-for-bio/exploring-relationships-between-two-variables.html


#Pearson?s correlation coefficient takes a value of 0 if two variables are uncorrelated, and a value of +1 or -1 if they are perfectly related. 
#?Perfectly related? means we can predict the exact value of one variable given knowledge of the other. A positive value indicates that high values 
#in one variable is associated with high values of the second. A negative value indicates that high values of one variable is associated with low 
#values of the second

#Attrition
cor<-rcorr(as.matrix(atrain[,c("X", "Attrition", "BusinessTravel","DailyRate","Department","DistanceFromHome",        
                               "Education","EducationField","EmployeeCount","EmployeeNumber","EnvironmentSatisfaction","Gender",                  
                               "HourlyRate","JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus",          
                               "MonthlyIncome","MonthlyRate","NumCompaniesWorked","Over18","OverTime","PercentSalaryHike",      
                               "PerformanceRating","RelationshipSatisfaction","StandardHours" ,          
                               "StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear",   
                               "WorkLifeBalance","YearsAtCompany","YearsInCurrentRole",      
                               "YearsSinceLastPromotion","YearsWithCurrManager","BirthDate" )]))
cor$r
cor$P

cor(atrain$Attrition, atrain$BirthDate)


#DailyRate
cor(atrain$DailyRate, atrain$DistanceFromHome)
cor(atrain$DailyRate, atrain$EmployeeNumber)
cor(atrain$DailyRate, atrain$HourlyRate)
cor(atrain$DailyRate, atrain$MonthlyIncome)
cor(atrain$DailyRate, atrain$MonthlyRate)
cor(atrain$DailyRate, atrain$NumCompaniesWorked)
cor(atrain$DailyRate, atrain$TotalWorkingYears)
cor(atrain$DailyRate, atrain$YearsAtCompany)
cor(atrain$DailyRate, atrain$YearsInCurrentRole)
cor(atrain$DailyRate, atrain$YearsSinceLastPromotion)
cor(atrain$DailyRate, atrain$YearsWithCurrManager)

#DistanceFromHome
cor(atrain$DistanceFromHome, atrain$DailyRate)
cor(atrain$DistanceFromHome, atrain$EmployeeNumber)
cor(atrain$DistanceFromHome, atrain$HourlyRate)
cor(atrain$DistanceFromHome, atrain$MonthlyIncome)
cor(atrain$DistanceFromHome, atrain$MonthlyRate)
cor(atrain$DistanceFromHome, atrain$NumCompaniesWorked)
cor(atrain$DistanceFromHome, atrain$TotalWorkingYears)
cor(atrain$DistanceFromHome, atrain$YearsAtCompany)
cor(atrain$DistanceFromHome, atrain$YearsInCurrentRole)
cor(atrain$DistanceFromHome, atrain$YearsSinceLastPromotion)
cor(atrain$DistanceFromHome, atrain$YearsWithCurrManager)

#EmployeeNumber
cor(atrain$EmployeeNumber, atrain$DailyRate)
cor(atrain$EmployeeNumber, atrain$DistanceFromHome)
cor(atrain$EmployeeNumber, atrain$HourlyRate)
cor(atrain$EmployeeNumber, atrain$MonthlyIncome)
cor(atrain$EmployeeNumber, atrain$MonthlyRate)
cor(atrain$EmployeeNumber, atrain$NumCompaniesWorked)
cor(atrain$EmployeeNumber, atrain$TotalWorkingYears)
cor(atrain$EmployeeNumber, atrain$YearsAtCompany)
cor(atrain$EmployeeNumber, atrain$YearsInCurrentRole)
cor(atrain$EmployeeNumber, atrain$YearsSinceLastPromotion)
cor(atrain$EmployeeNumber, atrain$YearsWithCurrManager)

#HourlyRate
cor(atrain$HourlyRate, atrain$DailyRate)
cor(atrain$HourlyRate, atrain$DistanceFromHome)
cor(atrain$HourlyRate, atrain$EmployeeNumber)
cor(atrain$HourlyRate, atrain$MonthlyIncome)
cor(atrain$HourlyRate, atrain$MonthlyRate)
cor(atrain$HourlyRate, atrain$NumCompaniesWorked)
cor(atrain$HourlyRate, atrain$TotalWorkingYears)
cor(atrain$HourlyRate, atrain$YearsAtCompany)
cor(atrain$HourlyRate, atrain$YearsInCurrentRole)
cor(atrain$HourlyRate, atrain$YearsSinceLastPromotion)
cor(atrain$HourlyRate, atrain$YearsWithCurrManager)

#MonthlyIncome
cor(atrain$MonthlyIncome, atrain$DailyRate)
cor(atrain$MonthlyIncome, atrain$DistanceFromHome)
cor(atrain$MonthlyIncome, atrain$EmployeeNumber)
cor(atrain$MonthlyIncome, atrain$HourlyRate)
cor(atrain$MonthlyIncome, atrain$MonthlyRate)
cor(atrain$MonthlyIncome, atrain$NumCompaniesWorked)
cor(atrain$MonthlyIncome, atrain$TotalWorkingYears)
cor(atrain$MonthlyIncome, atrain$YearsAtCompany)
cor(atrain$MonthlyIncome, atrain$YearsInCurrentRole)
cor(atrain$MonthlyIncome, atrain$YearsSinceLastPromotion)
cor(atrain$MonthlyIncome, atrain$YearsWithCurrManager)

#MonthlyRate
cor(atrain$MonthlyRate, atrain$DailyRate)
cor(atrain$MonthlyRate, atrain$DistanceFromHome)
cor(atrain$MonthlyRate, atrain$EmployeeNumber)
cor(atrain$MonthlyRate, atrain$HourlyRate)
cor(atrain$MonthlyRate, atrain$MonthlyIncome)
cor(atrain$MonthlyRate, atrain$NumCompaniesWorked)
cor(atrain$MonthlyRate, atrain$TotalWorkingYears)
cor(atrain$MonthlyRate, atrain$YearsAtCompany)
cor(atrain$MonthlyRate, atrain$YearsInCurrentRole)
cor(atrain$MonthlyRate, atrain$YearsSinceLastPromotion)
cor(atrain$MonthlyRate, atrain$YearsWithCurrManager)

#NumCompaniesWorked
cor(atrain$NumCompaniesWorked, atrain$DailyRate)
cor(atrain$NumCompaniesWorked, atrain$DistanceFromHome)
cor(atrain$NumCompaniesWorked, atrain$EmployeeNumber)
cor(atrain$NumCompaniesWorked, atrain$HourlyRate)
cor(atrain$NumCompaniesWorked, atrain$MonthlyIncome)
cor(atrain$NumCompaniesWorked, atrain$MonthlyRate)
cor(atrain$NumCompaniesWorked, atrain$TotalWorkingYears)
cor(atrain$NumCompaniesWorked, atrain$YearsAtCompany)
cor(atrain$NumCompaniesWorked, atrain$YearsInCurrentRole)
cor(atrain$NumCompaniesWorked, atrain$YearsSinceLastPromotion)
cor(atrain$NumCompaniesWorked, atrain$YearsWithCurrManager)

#TotalWorkingYears
cor(atrain$TotalWorkingYears, atrain$DailyRate)
cor(atrain$TotalWorkingYears, atrain$DistanceFromHome)
cor(atrain$TotalWorkingYears, atrain$EmployeeNumber)
cor(atrain$TotalWorkingYears, atrain$HourlyRate)
cor(atrain$TotalWorkingYears, atrain$MonthlyIncome)
cor(atrain$TotalWorkingYears, atrain$MonthlyRate)
cor(atrain$TotalWorkingYears, atrain$NumCompaniesWorked)
cor(atrain$TotalWorkingYears, atrain$YearsAtCompany)
cor(atrain$TotalWorkingYears, atrain$YearsInCurrentRole)
cor(atrain$TotalWorkingYears, atrain$YearsSinceLastPromotion)
cor(atrain$TotalWorkingYears, atrain$YearsWithCurrManager)

#YearsAtCompany
cor(atrain$YearsAtCompany, atrain$DailyRate)
cor(atrain$YearsAtCompany, atrain$DistanceFromHome)
cor(atrain$YearsAtCompany, atrain$EmployeeNumber)
cor(atrain$YearsAtCompany, atrain$HourlyRate)
cor(atrain$YearsAtCompany, atrain$MonthlyIncome)
cor(atrain$YearsAtCompany, atrain$MonthlyRate)
cor(atrain$YearsAtCompany, atrain$NumCompaniesWorked)
cor(atrain$YearsAtCompany, atrain$TotalWorkingYears)
cor(atrain$YearsAtCompany, atrain$YearsInCurrentRole)
cor(atrain$YearsAtCompany, atrain$YearsSinceLastPromotion)
cor(atrain$YearsAtCompany, atrain$YearsWithCurrManager)

#YearsInCurrentRole
cor(atrain$YearsInCurrentRole, atrain$DailyRate)
cor(atrain$YearsInCurrentRole, atrain$DistanceFromHome)
cor(atrain$YearsInCurrentRole, atrain$EmployeeNumber)
cor(atrain$YearsInCurrentRole, atrain$HourlyRate)
cor(atrain$YearsInCurrentRole, atrain$MonthlyIncome)
cor(atrain$YearsInCurrentRole, atrain$MonthlyRate)
cor(atrain$YearsInCurrentRole, atrain$NumCompaniesWorked)
cor(atrain$YearsInCurrentRole, atrain$TotalWorkingYears)
cor(atrain$YearsInCurrentRole, atrain$YearsAtCompany)
cor(atrain$YearsInCurrentRole, atrain$YearsSinceLastPromotion)
cor(atrain$YearsInCurrentRole, atrain$YearsWithCurrManager)

#YearsSinceLastPromotion
cor(atrain$YearsSinceLastPromotion, atrain$DailyRate)
cor(atrain$YearsSinceLastPromotion, atrain$DistanceFromHome)
cor(atrain$YearsSinceLastPromotion, atrain$EmployeeNumber)
cor(atrain$YearsSinceLastPromotion, atrain$HourlyRate)
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyIncome)
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyRate)
cor(atrain$YearsSinceLastPromotion, atrain$NumCompaniesWorked)
cor(atrain$YearsSinceLastPromotion, atrain$TotalWorkingYears)
cor(atrain$YearsSinceLastPromotion, atrain$YearsAtCompany)
cor(atrain$YearsSinceLastPromotion, atrain$YearsInCurrentRole)
cor(atrain$YearsSinceLastPromotion, atrain$YearsWithCurrManager)

#YearsWithCurrManager
cor(atrain$YearsWithCurrManager, atrain$DailyRate)
cor(atrain$YearsWithCurrManager, atrain$DistanceFromHome)
cor(atrain$YearsWithCurrManager, atrain$EmployeeNumber)
cor(atrain$YearsWithCurrManager, atrain$HourlyRate)
cor(atrain$YearsWithCurrManager, atrain$MonthlyIncome)
cor(atrain$YearsWithCurrManager, atrain$MonthlyRate)
cor(atrain$YearsWithCurrManager, atrain$NumCompaniesWorked)
cor(atrain$YearsWithCurrManager, atrain$TotalWorkingYears)
cor(atrain$YearsWithCurrManager, atrain$YearsAtCompany)
cor(atrain$YearsWithCurrManager, atrain$YearsInCurrentRole)
cor(atrain$YearsWithCurrManager, atrain$YearsSinceLastPromotion)


#KENDALL
#DailyRate
cor(atrain$DailyRate, atrain$DistanceFromHome, method = "kendall")
cor(atrain$DailyRate, atrain$EmployeeNumber, method = "kendall")
cor(atrain$DailyRate, atrain$HourlyRate, method = "kendall")
cor(atrain$DailyRate, atrain$MonthlyIncome, method = "kendall")
cor(atrain$DailyRate, atrain$MonthlyRate, method = "kendall")
cor(atrain$DailyRate, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$DailyRate, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$DailyRate, atrain$YearsAtCompany, method = "kendall")
cor(atrain$DailyRate, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$DailyRate, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$DailyRate, atrain$YearsWithCurrManager, method = "kendall")

#DistanceFromHome
cor(atrain$DistanceFromHome, atrain$DailyRate, method = "kendall")
cor(atrain$DistanceFromHome, atrain$EmployeeNumber, method = "kendall")
cor(atrain$DistanceFromHome, atrain$HourlyRate, method = "kendall")
cor(atrain$DistanceFromHome, atrain$MonthlyIncome, method = "kendall")
cor(atrain$DistanceFromHome, atrain$MonthlyRate, method = "kendall")
cor(atrain$DistanceFromHome, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$DistanceFromHome, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$DistanceFromHome, atrain$YearsAtCompany, method = "kendall")
cor(atrain$DistanceFromHome, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$DistanceFromHome, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$DistanceFromHome, atrain$YearsWithCurrManager, method = "kendall")

#EmployeeNumber
cor(atrain$EmployeeNumber, atrain$DailyRate, method = "kendall")
cor(atrain$EmployeeNumber, atrain$DistanceFromHome, method = "kendall")
cor(atrain$EmployeeNumber, atrain$HourlyRate, method = "kendall")
cor(atrain$EmployeeNumber, atrain$MonthlyIncome, method = "kendall")
cor(atrain$EmployeeNumber, atrain$MonthlyRate, method = "kendall")
cor(atrain$EmployeeNumber, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$EmployeeNumber, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$EmployeeNumber, atrain$YearsAtCompany, method = "kendall")
cor(atrain$EmployeeNumber, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$EmployeeNumber, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$EmployeeNumber, atrain$YearsWithCurrManager, method = "kendall")

#HourlyRate
cor(atrain$HourlyRate, atrain$DailyRate, method = "kendall")
cor(atrain$HourlyRate, atrain$DistanceFromHome, method = "kendall")
cor(atrain$HourlyRate, atrain$EmployeeNumber, method = "kendall")
cor(atrain$HourlyRate, atrain$MonthlyIncome, method = "kendall")
cor(atrain$HourlyRate, atrain$MonthlyRate, method = "kendall")
cor(atrain$HourlyRate, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$HourlyRate, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$HourlyRate, atrain$YearsAtCompany, method = "kendall")
cor(atrain$HourlyRate, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$HourlyRate, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$HourlyRate, atrain$YearsWithCurrManager, method = "kendall")

#MonthlyIncome
cor(atrain$MonthlyIncome, atrain$DailyRate, method = "kendall")
cor(atrain$MonthlyIncome, atrain$DistanceFromHome, method = "kendall")
cor(atrain$MonthlyIncome, atrain$EmployeeNumber, method = "kendall")
cor(atrain$MonthlyIncome, atrain$HourlyRate, method = "kendall")
cor(atrain$MonthlyIncome, atrain$MonthlyRate, method = "kendall")
cor(atrain$MonthlyIncome, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$MonthlyIncome, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$MonthlyIncome, atrain$YearsAtCompany, method = "kendall")
cor(atrain$MonthlyIncome, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$MonthlyIncome, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$MonthlyIncome, atrain$YearsWithCurrManager, method = "kendall")

#MonthlyRate
cor(atrain$MonthlyRate, atrain$DailyRate, method = "kendall")
cor(atrain$MonthlyRate, atrain$DistanceFromHome, method = "kendall")
cor(atrain$MonthlyRate, atrain$EmployeeNumber, method = "kendall")
cor(atrain$MonthlyRate, atrain$HourlyRate, method = "kendall")
cor(atrain$MonthlyRate, atrain$MonthlyIncome, method = "kendall")
cor(atrain$MonthlyRate, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$MonthlyRate, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$MonthlyRate, atrain$YearsAtCompany, method = "kendall")
cor(atrain$MonthlyRate, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$MonthlyRate, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$MonthlyRate, atrain$YearsWithCurrManager, method = "kendall")

#NumCompaniesWorked
cor(atrain$NumCompaniesWorked, atrain$DailyRate, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$DistanceFromHome, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$EmployeeNumber, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$HourlyRate, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$MonthlyIncome, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$MonthlyRate, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$YearsAtCompany, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$NumCompaniesWorked, atrain$YearsWithCurrManager, method = "kendall")

#TotalWorkingYears
cor(atrain$TotalWorkingYears, atrain$DailyRate, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$DistanceFromHome, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$EmployeeNumber, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$HourlyRate, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$MonthlyIncome, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$MonthlyRate, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$YearsAtCompany, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$TotalWorkingYears, atrain$YearsWithCurrManager, method = "kendall")

#YearsAtCompany
cor(atrain$YearsAtCompany, atrain$DailyRate, method = "kendall")
cor(atrain$YearsAtCompany, atrain$DistanceFromHome, method = "kendall")
cor(atrain$YearsAtCompany, atrain$EmployeeNumber, method = "kendall")
cor(atrain$YearsAtCompany, atrain$HourlyRate, method = "kendall")
cor(atrain$YearsAtCompany, atrain$MonthlyIncome, method = "kendall")
cor(atrain$YearsAtCompany, atrain$MonthlyRate, method = "kendall")
cor(atrain$YearsAtCompany, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$YearsAtCompany, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$YearsAtCompany, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$YearsAtCompany, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$YearsAtCompany, atrain$YearsWithCurrManager, method = "kendall")

#YearsInCurrentRole
cor(atrain$YearsInCurrentRole, atrain$DailyRate, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$DistanceFromHome, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$EmployeeNumber, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$HourlyRate, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$MonthlyIncome, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$MonthlyRate, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$YearsAtCompany, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$YearsSinceLastPromotion, method = "kendall")
cor(atrain$YearsInCurrentRole, atrain$YearsWithCurrManager, method = "kendall")

#YearsSinceLastPromotion
cor(atrain$YearsSinceLastPromotion, atrain$DailyRate, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$DistanceFromHome, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$EmployeeNumber, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$HourlyRate, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyIncome, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyRate, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$YearsAtCompany, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$YearsSinceLastPromotion, atrain$YearsWithCurrManager, method = "kendall")

#YearsWithCurrManager
cor(atrain$YearsWithCurrManager, atrain$DailyRate, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$DistanceFromHome, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$EmployeeNumber, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$HourlyRate, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$MonthlyIncome, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$MonthlyRate, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$NumCompaniesWorked, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$TotalWorkingYears, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$YearsAtCompany, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$YearsInCurrentRole, method = "kendall")
cor(atrain$YearsWithCurrManager, atrain$YearsSinceLastPromotion, method = "kendall")


#SPEARMAN
#DailyRate
cor(atrain$DailyRate, atrain$DistanceFromHome, method = "spearman")
cor(atrain$DailyRate, atrain$EmployeeNumber, method = "spearman")
cor(atrain$DailyRate, atrain$HourlyRate, method = "spearman")
cor(atrain$DailyRate, atrain$MonthlyIncome, method = "spearman")
cor(atrain$DailyRate, atrain$MonthlyRate, method = "spearman")
cor(atrain$DailyRate, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$DailyRate, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$DailyRate, atrain$YearsAtCompany, method = "spearman")
cor(atrain$DailyRate, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$DailyRate, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$DailyRate, atrain$YearsWithCurrManager, method = "spearman")

#DistanceFromHome
cor(atrain$DistanceFromHome, atrain$DailyRate, method = "spearman")
cor(atrain$DistanceFromHome, atrain$EmployeeNumber, method = "spearman")
cor(atrain$DistanceFromHome, atrain$HourlyRate, method = "spearman")
cor(atrain$DistanceFromHome, atrain$MonthlyIncome, method = "spearman")
cor(atrain$DistanceFromHome, atrain$MonthlyRate, method = "spearman")
cor(atrain$DistanceFromHome, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$DistanceFromHome, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$DistanceFromHome, atrain$YearsAtCompany, method = "spearman")
cor(atrain$DistanceFromHome, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$DistanceFromHome, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$DistanceFromHome, atrain$YearsWithCurrManager, method = "spearman")

#EmployeeNumber
cor(atrain$EmployeeNumber, atrain$DailyRate, method = "spearman")
cor(atrain$EmployeeNumber, atrain$DistanceFromHome, method = "spearman")
cor(atrain$EmployeeNumber, atrain$HourlyRate, method = "spearman")
cor(atrain$EmployeeNumber, atrain$MonthlyIncome, method = "spearman")
cor(atrain$EmployeeNumber, atrain$MonthlyRate, method = "spearman")
cor(atrain$EmployeeNumber, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$EmployeeNumber, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$EmployeeNumber, atrain$YearsAtCompany, method = "spearman")
cor(atrain$EmployeeNumber, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$EmployeeNumber, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$EmployeeNumber, atrain$YearsWithCurrManager, method = "spearman")

#HourlyRate
cor(atrain$HourlyRate, atrain$DailyRate, method = "spearman")
cor(atrain$HourlyRate, atrain$DistanceFromHome, method = "spearman")
cor(atrain$HourlyRate, atrain$EmployeeNumber, method = "spearman")
cor(atrain$HourlyRate, atrain$MonthlyIncome, method = "spearman")
cor(atrain$HourlyRate, atrain$MonthlyRate, method = "spearman")
cor(atrain$HourlyRate, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$HourlyRate, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$HourlyRate, atrain$YearsAtCompany, method = "spearman")
cor(atrain$HourlyRate, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$HourlyRate, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$HourlyRate, atrain$YearsWithCurrManager, method = "spearman")

#MonthlyIncome
cor(atrain$MonthlyIncome, atrain$DailyRate, method = "spearman")
cor(atrain$MonthlyIncome, atrain$DistanceFromHome, method = "spearman")
cor(atrain$MonthlyIncome, atrain$EmployeeNumber, method = "spearman")
cor(atrain$MonthlyIncome, atrain$HourlyRate, method = "spearman")
cor(atrain$MonthlyIncome, atrain$MonthlyRate, method = "spearman")
cor(atrain$MonthlyIncome, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$MonthlyIncome, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$MonthlyIncome, atrain$YearsAtCompany, method = "spearman")
cor(atrain$MonthlyIncome, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$MonthlyIncome, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$MonthlyIncome, atrain$YearsWithCurrManager, method = "spearman")

#MonthlyRate
cor(atrain$MonthlyRate, atrain$DailyRate, method = "spearman")
cor(atrain$MonthlyRate, atrain$DistanceFromHome, method = "spearman")
cor(atrain$MonthlyRate, atrain$EmployeeNumber, method = "spearman")
cor(atrain$MonthlyRate, atrain$HourlyRate, method = "spearman")
cor(atrain$MonthlyRate, atrain$MonthlyIncome, method = "spearman")
cor(atrain$MonthlyRate, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$MonthlyRate, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$MonthlyRate, atrain$YearsAtCompany, method = "spearman")
cor(atrain$MonthlyRate, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$MonthlyRate, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$MonthlyRate, atrain$YearsWithCurrManager, method = "spearman")

#NumCompaniesWorked
cor(atrain$NumCompaniesWorked, atrain$DailyRate, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$DistanceFromHome, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$EmployeeNumber, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$HourlyRate, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$MonthlyIncome, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$MonthlyRate, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$YearsAtCompany, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$NumCompaniesWorked, atrain$YearsWithCurrManager, method = "spearman")

#TotalWorkingYears
cor(atrain$TotalWorkingYears, atrain$DailyRate, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$DistanceFromHome, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$EmployeeNumber, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$HourlyRate, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$MonthlyIncome, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$MonthlyRate, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$YearsAtCompany, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$TotalWorkingYears, atrain$YearsWithCurrManager, method = "spearman")

#YearsAtCompany
cor(atrain$YearsAtCompany, atrain$DailyRate, method = "spearman")
cor(atrain$YearsAtCompany, atrain$DistanceFromHome, method = "spearman")
cor(atrain$YearsAtCompany, atrain$EmployeeNumber, method = "spearman")
cor(atrain$YearsAtCompany, atrain$HourlyRate, method = "spearman")
cor(atrain$YearsAtCompany, atrain$MonthlyIncome, method = "spearman")
cor(atrain$YearsAtCompany, atrain$MonthlyRate, method = "spearman")
cor(atrain$YearsAtCompany, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$YearsAtCompany, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$YearsAtCompany, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$YearsAtCompany, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$YearsAtCompany, atrain$YearsWithCurrManager, method = "spearman")

#YearsInCurrentRole
cor(atrain$YearsInCurrentRole, atrain$DailyRate, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$DistanceFromHome, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$EmployeeNumber, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$HourlyRate, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$MonthlyIncome, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$MonthlyRate, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$YearsAtCompany, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$YearsSinceLastPromotion, method = "spearman")
cor(atrain$YearsInCurrentRole, atrain$YearsWithCurrManager, method = "spearman")

#YearsSinceLastPromotion
cor(atrain$YearsSinceLastPromotion, atrain$DailyRate, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$DistanceFromHome, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$EmployeeNumber, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$HourlyRate, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyIncome, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$MonthlyRate, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$YearsAtCompany, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$YearsSinceLastPromotion, atrain$YearsWithCurrManager, method = "spearman")

#YearsWithCurrManager
cor(atrain$YearsWithCurrManager, atrain$DailyRate, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$DistanceFromHome, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$EmployeeNumber, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$HourlyRate, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$MonthlyIncome, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$MonthlyRate, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$NumCompaniesWorked, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$TotalWorkingYears, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$YearsAtCompany, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$YearsInCurrentRole, method = "spearman")
cor(atrain$YearsWithCurrManager, atrain$YearsSinceLastPromotion, method = "spearman")




#---------------------------------------------------------------------------------------------------



