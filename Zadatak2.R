#nakon pokretanja generalne skipte odradimo under-sampling za RF 
data_under<-ovun.sample(Attrition~., data = attrition_train, 
                        method="under", N=600, seed = 1)$data

under.fit <- randomForest(Attrition~.-X-Over18, data=data_under, mtry=3, importance =TRUE, ntree=3000)
under.pred <- predict(under.fit, newdata = attrition_test, type="class")
confusionMatrix(under.pred,attrition_test$Attrition)


