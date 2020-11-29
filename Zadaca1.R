library(ggplot2)
library(statip)
library(Hmisc)

getwd()
setwd("E:/MU/Zadaca1/")

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
#https://dzchilds.github.io/eda-for-bio/exploring-relationships-between-two-variables.html


#Pearson?s correlation coefficient takes a value of 0 if two variables are uncorrelated, and a value of +1 or -1 if they are perfectly related. 
#?Perfectly related? means we can predict the exact value of one variable given knowledge of the other. A positive value indicates that high values 
#in one variable is associated with high values of the second. A negative value indicates that high values of one variable is associated with low 
#values of the second

#Attrition
cor<-rcorr(as.matrix(attrition_train[,c("X", "Attrition", "BusinessTravel","DailyRate","Department","DistanceFromHome",        
                               "Education","EducationField","EmployeeCount","EmployeeNumber","EnvironmentSatisfaction","Gender",                  
                               "HourlyRate","JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus",          
                               "MonthlyIncome","MonthlyRate","NumCompaniesWorked","Over18","OverTime","PercentSalaryHike",      
                               "PerformanceRating","RelationshipSatisfaction","StandardHours" ,          
                               "StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear",   
                               "WorkLifeBalance","YearsAtCompany","YearsInCurrentRole",      
                               "YearsSinceLastPromotion","YearsWithCurrManager","BirthDate" )]))
cor$r
cor$P

cor(attrition_train$Attrition, attrition_train$BirthDate)
#rucna provjera korelacija izmedju attrition i ostalih varijabli
attrition_train$Attrition<-ifelse(attrition_train$Attrition=="Yes",1,0)
typeof(atrain$Attrition)

cor(attrition_train$Attrition, attrition_train$X)
cor(attrition_train$Attrition, attrition_train$Attrition)
cor(attrition_train$Attrition, attrition_train$BusinessTravel)
cor(attrition_train$Attrition, attrition_train$DailyRate)
cor(attrition_train$Attrition, attrition_train$Department)
cor(attrition_train$Attrition, attrition_train$DistanceFromHome)
cor(attrition_train$Attrition, attrition_train$Education)
cor(attrition_train$Attrition, attrition_train$EducationField)
cor(attrition_train$Attrition, attrition_train$EmployeeCount)
cor(attrition_train$Attrition, attrition_train$EmployeeNumber)
cor(attrition_train$Attrition, attrition_train$EnvironmentSatisfaction)
cor(attrition_train$Attrition, attrition_train$Gender)
cor(attrition_train$Attrition, attrition_train$HourlyRate)
cor(attrition_train$Attrition, attrition_train$JobInvolvement)
cor(attrition_train$Attrition, attrition_train$JobLevel)
cor(attrition_train$Attrition, attrition_train$JobRole)
cor(attrition_train$Attrition, attrition_train$JobSatisfaction)
cor(attrition_train$Attrition, attrition_train$MaritalStatus)
cor(attrition_train$Attrition, attrition_train$MonthlyIncome)
cor(attrition_train$Attrition, attrition_train$MonthlyRate)
cor(attrition_train$Attrition, attrition_train$NumCompaniesWorked)
cor(attrition_train$Attrition, attrition_train$Over18)
cor(attrition_train$Attrition, attrition_train$OverTime)
cor(attrition_train$Attrition, attrition_train$PercentSalaryHike)
cor(attrition_train$Attrition, attrition_train$PerformanceRating)
cor(attrition_train$Attrition, attrition_train$RelationshipSatisfaction)
cor(attrition_train$Attrition, attrition_train$StandardHours)
cor(attrition_train$Attrition, attrition_train$StockOptionLevel)
cor(attrition_train$Attrition, attrition_train$TotalWorkingYears)
cor(attrition_train$Attrition, attrition_train$TrainingTimesLastYear)
cor(attrition_train$Attrition, attrition_train$WorkLifeBalance)
cor(attrition_train$Attrition, attrition_train$YearsAtCompany)
cor(attrition_train$Attrition, attrition_train$YearsInCurrentRole)
cor(attrition_train$Attrition, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$Attrition, attrition_train$YearsWithCurrManager)
cor(attrition_train$Attrition, attrition_train$BirthDate)



#DailyRate
cor(attrition_train$DailyRate, attrition_train$DistanceFromHome)
cor(attrition_train$DailyRate, attrition_train$EmployeeNumber)
cor(attrition_train$DailyRate, attrition_train$HourlyRate)
cor(attrition_train$DailyRate, attrition_train$MonthlyIncome)
cor(attrition_train$DailyRate, attrition_train$MonthlyRate)
cor(attrition_train$DailyRate, attrition_train$NumCompaniesWorked)
cor(attrition_train$DailyRate, attrition_train$TotalWorkingYears)
cor(attrition_train$DailyRate, attrition_train$YearsAtCompany)
cor(attrition_train$DailyRate, attrition_train$YearsInCurrentRole)
cor(attrition_train$DailyRate, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$DailyRate, attrition_train$YearsWithCurrManager)

#DistanceFromHome
cor(attrition_train$DistanceFromHome, attrition_train$DailyRate)
cor(attrition_train$DistanceFromHome, attrition_train$EmployeeNumber)
cor(attrition_train$DistanceFromHome, attrition_train$HourlyRate)
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyIncome)
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyRate)
cor(attrition_train$DistanceFromHome, attrition_train$NumCompaniesWorked)
cor(attrition_train$DistanceFromHome, attrition_train$TotalWorkingYears)
cor(attrition_train$DistanceFromHome, attrition_train$YearsAtCompany)
cor(attrition_train$DistanceFromHome, attrition_train$YearsInCurrentRole)
cor(attrition_train$DistanceFromHome, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$DistanceFromHome, attrition_train$YearsWithCurrManager)

#EmployeeNumber
cor(attrition_train$EmployeeNumber, attrition_train$DailyRate)
cor(attrition_train$EmployeeNumber, attrition_train$DistanceFromHome)
cor(attrition_train$EmployeeNumber, attrition_train$HourlyRate)
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyIncome)
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyRate)
cor(attrition_train$EmployeeNumber, attrition_train$NumCompaniesWorked)
cor(attrition_train$EmployeeNumber, attrition_train$TotalWorkingYears)
cor(attrition_train$EmployeeNumber, attrition_train$YearsAtCompany)
cor(attrition_train$EmployeeNumber, attrition_train$YearsInCurrentRole)
cor(attrition_train$EmployeeNumber, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$EmployeeNumber, attrition_train$YearsWithCurrManager)

#HourlyRate
cor(attrition_train$HourlyRate, attrition_train$DailyRate)
cor(attrition_train$HourlyRate, attrition_train$DistanceFromHome)
cor(attrition_train$HourlyRate, attrition_train$EmployeeNumber)
cor(attrition_train$HourlyRate, attrition_train$MonthlyIncome)
cor(attrition_train$HourlyRate, attrition_train$MonthlyRate)
cor(attrition_train$HourlyRate, attrition_train$NumCompaniesWorked)
cor(attrition_train$HourlyRate, attrition_train$TotalWorkingYears)
cor(attrition_train$HourlyRate, attrition_train$YearsAtCompany)
cor(attrition_train$HourlyRate, attrition_train$YearsInCurrentRole)
cor(attrition_train$HourlyRate, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$HourlyRate, attrition_train$YearsWithCurrManager)

#MonthlyIncome
cor(attrition_train$MonthlyIncome, attrition_train$DailyRate)
cor(attrition_train$MonthlyIncome, attrition_train$DistanceFromHome)
cor(attrition_train$MonthlyIncome, attrition_train$EmployeeNumber)
cor(attrition_train$MonthlyIncome, attrition_train$HourlyRate)
cor(attrition_train$MonthlyIncome, attrition_train$MonthlyRate)
cor(attrition_train$MonthlyIncome, attrition_train$NumCompaniesWorked)
cor(attrition_train$MonthlyIncome, attrition_train$TotalWorkingYears)
cor(attrition_train$MonthlyIncome, attrition_train$YearsAtCompany)
cor(attrition_train$MonthlyIncome, attrition_train$YearsInCurrentRole)
cor(attrition_train$MonthlyIncome, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$MonthlyIncome, attrition_train$YearsWithCurrManager)

#MonthlyRate
cor(attrition_train$MonthlyRate, attrition_train$DailyRate)
cor(attrition_train$MonthlyRate, attrition_train$DistanceFromHome)
cor(attrition_train$MonthlyRate, attrition_train$EmployeeNumber)
cor(attrition_train$MonthlyRate, attrition_train$HourlyRate)
cor(attrition_train$MonthlyRate, attrition_train$MonthlyIncome)
cor(attrition_train$MonthlyRate, attrition_train$NumCompaniesWorked)
cor(attrition_train$MonthlyRate, attrition_train$TotalWorkingYears)
cor(attrition_train$MonthlyRate, attrition_train$YearsAtCompany)
cor(attrition_train$MonthlyRate, attrition_train$YearsInCurrentRole)
cor(attrition_train$MonthlyRate, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$MonthlyRate, attrition_train$YearsWithCurrManager)

#NumCompaniesWorked
cor(attrition_train$NumCompaniesWorked, attrition_train$DailyRate)
cor(attrition_train$NumCompaniesWorked, attrition_train$DistanceFromHome)
cor(attrition_train$NumCompaniesWorked, attrition_train$EmployeeNumber)
cor(attrition_train$NumCompaniesWorked, attrition_train$HourlyRate)
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyIncome)
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyRate)
cor(attrition_train$NumCompaniesWorked, attrition_train$TotalWorkingYears)
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsAtCompany)
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsInCurrentRole)
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsWithCurrManager)

#TotalWorkingYears
cor(attrition_train$TotalWorkingYears, attrition_train$DailyRate)
cor(attrition_train$TotalWorkingYears, attrition_train$DistanceFromHome)
cor(attrition_train$TotalWorkingYears, attrition_train$EmployeeNumber)
cor(attrition_train$TotalWorkingYears, attrition_train$HourlyRate)
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyIncome)
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyRate)
cor(attrition_train$TotalWorkingYears, attrition_train$NumCompaniesWorked)
cor(attrition_train$TotalWorkingYears, attrition_train$YearsAtCompany)
cor(attrition_train$TotalWorkingYears, attrition_train$YearsInCurrentRole)
cor(attrition_train$TotalWorkingYears, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$TotalWorkingYears, attrition_train$YearsWithCurrManager)

#YearsAtCompany
cor(attrition_train$YearsAtCompany, attrition_train$DailyRate)
cor(attrition_train$YearsAtCompany, attrition_train$DistanceFromHome)
cor(attrition_train$YearsAtCompany, attrition_train$EmployeeNumber)
cor(attrition_train$YearsAtCompany, attrition_train$HourlyRate)
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyIncome)
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyRate)
cor(attrition_train$YearsAtCompany, attrition_train$NumCompaniesWorked)
cor(attrition_train$YearsAtCompany, attrition_train$TotalWorkingYears)
cor(attrition_train$YearsAtCompany, attrition_train$YearsInCurrentRole)
cor(attrition_train$YearsAtCompany, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$YearsAtCompany, attrition_train$YearsWithCurrManager)

#YearsInCurrentRole
cor(attrition_train$YearsInCurrentRole, attrition_train$DailyRate)
cor(attrition_train$YearsInCurrentRole, attrition_train$DistanceFromHome)
cor(attrition_train$YearsInCurrentRole, attrition_train$EmployeeNumber)
cor(attrition_train$YearsInCurrentRole, attrition_train$HourlyRate)
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyIncome)
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyRate)
cor(attrition_train$YearsInCurrentRole, attrition_train$NumCompaniesWorked)
cor(attrition_train$YearsInCurrentRole, attrition_train$TotalWorkingYears)
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsAtCompany)
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsSinceLastPromotion)
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsWithCurrManager)

#YearsSinceLastPromotion
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DailyRate)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DistanceFromHome)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$EmployeeNumber)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$HourlyRate)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyIncome)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyRate)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$NumCompaniesWorked)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$TotalWorkingYears)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsAtCompany)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsInCurrentRole)
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsWithCurrManager)

#YearsWithCurrManager
cor(attrition_train$YearsWithCurrManager, attrition_train$DailyRate)
cor(attrition_train$YearsWithCurrManager, attrition_train$DistanceFromHome)
cor(attrition_train$YearsWithCurrManager, attrition_train$EmployeeNumber)
cor(attrition_train$YearsWithCurrManager, attrition_train$HourlyRate)
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyIncome)
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyRate)
cor(attrition_train$YearsWithCurrManager, attrition_train$NumCompaniesWorked)
cor(attrition_train$YearsWithCurrManager, attrition_train$TotalWorkingYears)
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsAtCompany)
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsInCurrentRole)
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsSinceLastPromotion)


#KENDALL
#DailyRate
cor(attrition_train$DailyRate, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$DailyRate, attrition_train$YearsWithCurrManager, method = "kendall")

#DistanceFromHome
cor(attrition_train$DistanceFromHome, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$DistanceFromHome, attrition_train$YearsWithCurrManager, method = "kendall")

#EmployeeNumber
cor(attrition_train$EmployeeNumber, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$EmployeeNumber, attrition_train$YearsWithCurrManager, method = "kendall")

#HourlyRate
cor(attrition_train$HourlyRate, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$HourlyRate, attrition_train$YearsWithCurrManager, method = "kendall")

#MonthlyIncome
cor(attrition_train$MonthlyIncome, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$MonthlyIncome, attrition_train$YearsWithCurrManager, method = "kendall")

#MonthlyRate
cor(attrition_train$MonthlyRate, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$MonthlyRate, attrition_train$YearsWithCurrManager, method = "kendall")

#NumCompaniesWorked
cor(attrition_train$NumCompaniesWorked, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsWithCurrManager, method = "kendall")

#TotalWorkingYears
cor(attrition_train$TotalWorkingYears, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsWithCurrManager, method = "kendall")

#YearsAtCompany
cor(attrition_train$YearsAtCompany, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$YearsAtCompany, attrition_train$YearsWithCurrManager, method = "kendall")

#YearsInCurrentRole
cor(attrition_train$YearsInCurrentRole, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsSinceLastPromotion, method = "kendall")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsWithCurrManager, method = "kendall")

#YearsSinceLastPromotion
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsWithCurrManager, method = "kendall")

#YearsWithCurrManager
cor(attrition_train$YearsWithCurrManager, attrition_train$DailyRate, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$DistanceFromHome, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$EmployeeNumber, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$HourlyRate, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyIncome, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyRate, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$NumCompaniesWorked, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$TotalWorkingYears, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsAtCompany, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsInCurrentRole, method = "kendall")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsSinceLastPromotion, method = "kendall")


#SPEARMAN
#DailyRate
cor(attrition_train$DailyRate, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$DailyRate, attrition_train$YearsWithCurrManager, method = "spearman")

#DistanceFromHome
cor(attrition_train$DistanceFromHome, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$DistanceFromHome, attrition_train$YearsWithCurrManager, method = "spearman")

#EmployeeNumber
cor(attrition_train$EmployeeNumber, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$EmployeeNumber, attrition_train$YearsWithCurrManager, method = "spearman")

#HourlyRate
cor(attrition_train$HourlyRate, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$HourlyRate, attrition_train$YearsWithCurrManager, method = "spearman")

#MonthlyIncome
cor(attrition_train$MonthlyIncome, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$MonthlyIncome, attrition_train$YearsWithCurrManager, method = "spearman")

#MonthlyRate
cor(attrition_train$MonthlyRate, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$MonthlyRate, attrition_train$YearsWithCurrManager, method = "spearman")

#NumCompaniesWorked
cor(attrition_train$NumCompaniesWorked, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$NumCompaniesWorked, attrition_train$YearsWithCurrManager, method = "spearman")

#TotalWorkingYears
cor(attrition_train$TotalWorkingYears, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$TotalWorkingYears, attrition_train$YearsWithCurrManager, method = "spearman")

#YearsAtCompany
cor(attrition_train$YearsAtCompany, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$YearsAtCompany, attrition_train$YearsWithCurrManager, method = "spearman")

#YearsInCurrentRole
cor(attrition_train$YearsInCurrentRole, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsSinceLastPromotion, method = "spearman")
cor(attrition_train$YearsInCurrentRole, attrition_train$YearsWithCurrManager, method = "spearman")

#YearsSinceLastPromotion
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$YearsSinceLastPromotion, attrition_train$YearsWithCurrManager, method = "spearman")

#YearsWithCurrManager
cor(attrition_train$YearsWithCurrManager, attrition_train$DailyRate, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$DistanceFromHome, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$EmployeeNumber, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$HourlyRate, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyIncome, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$MonthlyRate, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$NumCompaniesWorked, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$TotalWorkingYears, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsAtCompany, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsInCurrentRole, method = "spearman")
cor(attrition_train$YearsWithCurrManager, attrition_train$YearsSinceLastPromotion, method = "spearman")




#---------------------------------------------------------------------------------------------------



