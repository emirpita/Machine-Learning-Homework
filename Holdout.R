library(dplyr)

#holdout za rpart
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  rpart.attrition <- rpart(Attrition ~ BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=attrition_train, subset=atrain)
  rpart.pred <- predict(rpart.attrition, atest, type="class")
}
confusionMatrix(rpart.pred, atest$Attrition) 

#holdout za tree
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  attrition.tree <- tree(Attrition ~ BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=attrition_train, subset=atrain)
  tree.pred <- predict(attrition.tree, atest, type="class")
}
confusionMatrix(tree.pred, atest$Attrition) 

#holdout za bagging
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  atrain.bag<-randomForest(Attrition~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=attrition_train, subset=atrain, mtry=13, importance =TRUE, ntree=3000)
  atrain.pred<-predict(atrain.bag,atest, type="class")
}
confusionMatrix(atrain.pred,atest$Attrition)

#holdout za randomForest
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  atrain.randomForest<-randomForest(Attrition~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=attrition_train, subset=atrain, mtry=3, importance =TRUE, ntree=3000)
  randomForest.pred<-predict(atrain.randomForest,atest, type="class")
}
confusionMatrix(randomForest.pred,atest$Attrition)

#holdout za boosting
attrition_train$Attrition<-ifelse(attrition_train$Attrition=="Yes",1,0)

attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
attrition_train$Attrition <- as.character(attrition_train$Attrition)

for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  
  atrain <- attrition_train[atrain,]
  
  boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours-BirthDate, data=atrain, distribution="bernoulli", n.trees=10000, interaction.depth=8)
  boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=10000)
  
  boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
  
}

confusionMatrix(boost.pred,atest$Attrition)

