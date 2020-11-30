library(dplyr)
library(randomForest)
library(gbm)
library(ROSE)

#holdout za rpart
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  rpart.attrition <- rpart(Attrition ~ BusinessTravel+Department+DistanceFromHome+Education+
                           EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+
                           JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                           OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                           TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                           YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=attrition_train, subset=atrain)
  rpart.pred <- predict(rpart.attrition, atest, type="class")
}
confusionMatrix(rpart.pred, atest$Attrition) 

#holdout za tree
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  attrition.tree <- tree(Attrition ~ BusinessTravel+Department+DistanceFromHome+Education+
                         EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+
                         JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                         OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                         TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                         YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=attrition_train, subset=atrain)
  tree.pred <- predict(attrition.tree, atest, type="class")
}
confusionMatrix(tree.pred, atest$Attrition) 

#holdout za bagging
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  atrain.bag<-randomForest(Attrition~BusinessTravel+Department+DistanceFromHome+Education+
                           EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+
                           JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                           OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                           TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                           YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,
                           data=attrition_train, subset=atrain, mtry=13, importance =TRUE, ntree=3000)
  atrain.pred<-predict(atrain.bag,atest, type="class")
}
confusionMatrix(atrain.pred,atest$Attrition)

#holdout za randomForest
for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  atrain.randomForest<-randomForest(Attrition~BusinessTravel+Department+DistanceFromHome+Education+
                                    EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                                    JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+
                                    NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+
                                    RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+
                                    WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,
                                    data=attrition_train, subset=atrain, mtry=3, importance =TRUE, ntree=3000)
  randomForest.pred<-predict(atrain.randomForest,atest, type="class")
}
confusionMatrix(randomForest.pred,atest$Attrition)

#holdout za boosting
attrition_train$Attrition<-ifelse(attrition_train$Attrition=="Yes",1,0)

attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-
  lapply(attrition_train[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)

attrition_train$Attrition <- as.character(attrition_train$Attrition)

for(i in 1:10){
  set.seed(2^i)
  atrain<-sample(nrow(attrition_train),0.66*nrow(attrition_train))
  atest<-attrition_train[-atrain,]
  
  atrain <- attrition_train[atrain,]
  
  boost.atrain<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours-BirthDate, data=atrain, distribution="bernoulli", n.trees=1000, interaction.depth=8)
  boost.pred<-predict.gbm(boost.atrain, atest, type = "response", n.trees=1000)
  
  boost.pred<-ifelse(boost.pred>mean(boost.pred),1,0)
  
}

confusionMatrix(as.factor(boost.pred),as.factor(atest$Attrition))

#cross validation za rpart
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model
model <- train(Attrition ~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = attrition_train, method = "rpart",
               trControl = train.control)
pred <- predict(model, attrition_test)
# Summarize the results
print(model)
confusionMatrix(pred, attrition_test$Attrition)
confusionMatrix(model, "none")

#cross validation za tree
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model
model <- train(Attrition ~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = attrition_train, method = "ctree",
               trControl = train.control)
pred <- predict(model, attrition_test)
# Summarize the results
print(model)
confusionMatrix(pred, attrition_test$Attrition)
confusionMatrix(model, "none")

#cross validation za tree
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model
model <- train(Attrition ~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = attrition_train, method = "treebag",
               trControl = train.control)
pred <- predict(model, attrition_test)
# Summarize the results
print(model)
confusionMatrix(pred, attrition_test$Attrition)
confusionMatrix(model, "none")

#cross validation za tree
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model
model <- train(Attrition ~BusinessTravel+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data = attrition_train, method = "ranger",
               trControl = train.control)
pred <- predict(model, attrition_test)
# Summarize the results
print(model)
confusionMatrix(pred, attrition_test$Attrition)
confusionMatrix(model, "none")

#--------------------------------------------

attrition_train<-read.csv("Data/attrition_train.csv", header=TRUE)
#rpart
cv_table <- matrix(c(0,0,0,0),nrow=2,ncol=2)
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 
atrain<-atrain[sample(nrow(atrain)),]

folds <- cut(seq(1,nrow(atrain)),breaks=10,labels=FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- atrain[testIndexes, ]
  trainData <- atrain[-testIndexes, ]
  model <- rpart(Attrition~.,trainData)
  prediction <- predict(model, testData, type ="class")
  cv_table <- cv_table + table(prediction, testData$Attrition)
  
}
cv_table <- as.table(cv_table)
confusionMatrix(cv_table)



#random forest
cv_table <- matrix(c(0,0,0,0),nrow=2,ncol=2)
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 
atrain<-atrain[sample(nrow(atrain)),]
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)

folds <- cut(seq(1,nrow(atrain)),breaks=10,labels=FALSE)
for(i in 1:10){

  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- atrain[testIndexes, ]
  trainData <- atrain[-testIndexes, ]
  model <- randomForest(Attrition~.-X-EmployeeCount, data=atrain, mtry=3, importance =TRUE, ntree=3000, na.action = na.omit)
  prediction <- predict(model, testData, type ="class")
  cv_table <- cv_table + table(prediction, testData$Attrition)
  
}
cv_table <- as.table(cv_table)
confusionMatrix(cv_table)




#bagging
cv_table <- matrix(c(0,0,0,0),nrow=2,ncol=2)
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 
atrain<-atrain[sample(nrow(atrain)),]
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)

folds <- cut(seq(1,nrow(atrain)),breaks=10,labels=FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- atrain[testIndexes, ]
  trainData <- atrain[-testIndexes, ]
  model <- bagging(
    formula = Attrition ~ .,
    data = atrain,
    nbagg = 100,  
    coob = TRUE,
    control = rpart.control(minsplit = 2, cp = 0)
  )
  prediction <- predict(model, testData, type ="class")
  cv_table <- cv_table + table(prediction, testData$Attrition)
  
}
cv_table <- as.table(cv_table)
confusionMatrix(cv_table)



#boosting
cv_table <- matrix(c(0,0,0,0),nrow=2,ncol=2)
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 
atrain<-atrain[sample(nrow(atrain)),]

atrain$Attrition<-ifelse(atrain$Attrition=="Yes",1,0)
atest$Attrition<-ifelse(atest$Attrition=="Yes",1,0)
atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)]<-lapply(atrain[,c(2:3,5,7:8,11:12,14:18,22:23,25:26,28,31,36)],as.factor)
atrain$Attrition <- as.character(atrain$Attrition)


folds <- cut(seq(1,nrow(atrain)),breaks=10,labels=FALSE)

for(i in 1:10){

  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- atrain[testIndexes, ]
  trainData <- atrain[-testIndexes, ]
  model<-gbm(Attrition~.-EmployeeCount-Over18-StandardHours, data=na.omit(trainData), distribution="bernoulli", n.trees=10000, interaction.depth=8)
  prediction<-predict.gbm(model, testData, type = "response", n.trees=10000)
  prediction<-ifelse(prediction>mean(prediction),1,0)
  cv_table <- cv_table + table(prediction, testData$Attrition)
  
}
cv_table <- as.table(cv_table)
confusionMatrix(cv_table)
