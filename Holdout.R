tree.ss.acc<-0
tree.ss.prec<-0
for(i in 1:10){
  set.seed(2^i)
  #trebale bi se izbaciti ove tri kolone jer imaju samo jednu vrijednost
  #ali opet javlja gresku, moguce da ima "na" vrijednosti
  atrain.tree<-glm(Attrition~.-Over18-EmployeeCount-StandardHours, family="binomial", data=atrain,na.action = na.exclude)
  tree.pred<-predict(atrain.tree, atest, type ="class") 
  tree.conf<-table(atest$Attrition,tree.pred) 

  #c50.acc<-(c50.conf[1,1]+c50.conf[2,2])/(sum(c50.conf))
  #c50.prec<-c50.conf[2,2]/(c50.conf[2,2]+c50.conf[1,2])
  
  tree.ss.acc<-tree.ss.acc+(tree.conf[1,1]+tree.conf[2,2])/(sum(tree.conf))
  tree.ss.prec<-tree.ss.prec+tree.conf[2,2]/(tree.conf[2,2]+tree.conf[1,2])
  
}

tree.ss.acc<-tree.ss.acc/10
tree.ss.prec<-tree.ss.prec/10