consolidate.education <- function(education) {
  if(education == 'Below College') {
    return(1)
  } else if(education == 'College') {
    return(2)
  } else if(education == 'Bachelor') {
    return(3)
  } else if(education == 'Master') {
    return(4)
  } else if(education == 'Doctor') {
    return(5)
  } else {
    return(education)
  }
}
consolidate.jobLevel <- function(jobLevel) {
  if(jobLevel == 'One') {
    return(1)
  } else if(jobLevel == 'Two') {
    return(2)
  } else if(jobLevel == 'Three') {
    return(3)
  } else if(jobLevel == 'Four') {
    return(4)
  } else if(jobLevel == 'Five') {
    return(5)
  } else {
    return(jobLevel)
  }
}
consolidate.enviromentSatisfaction <- function(eSatisfaction) {
  if(eSatisfaction == 'Low') {
    return(1)
  } else if(eSatisfaction == 'Medium') {
    return(2)
  } else if(eSatisfaction == 'High') {
    return(3)
  } else if(eSatisfaction == 'Very High') {
    return(4)
  }else {
    return(eSatisfaction)
  }
}
consolidate.jobInvolvment <- function(jobInvolvment) {
  if(jobInvolvment == 'Low') {
    return(1)
  } else if(jobInvolvment == 'Medium') {
    return(2)
  } else if(jobInvolvment == 'High') {
    return(3)
  } else if(jobInvolvment == 'Very High') {
    return(4)
  } else {
    return(jobInvolvment)
  }
}
consolidate.workLifeBalance <- function(workLifeBalance) {
  if(workLifeBalance == 'Bad') {
    return(1)
  } else if(workLifeBalance == 'Good') {
    return(2)
  } else if(workLifeBalance == 'Better') {
    return(3)
  } else if(workLifeBalance == 'Best') {
    return(4)
  } else {
    return(workLifeBalance)
  }
}

consolidate.relationshipSatisfaction <- function(relationshipSatisfaction) {
  if(relationshipSatisfaction == 'Low') {
    return(1)
  } else if(relationshipSatisfaction == 'Medium') {
    return(2)
  } else if(relationshipSatisfaction == 'High') {
    return(3)
  } else if(relationshipSatisfaction == 'Very High') {
    return(4)
  } else {
    return(relationshipSatisfaction)
  }
}

consolidate.performanceRating <- function(performanceRating) {
  if(performanceRating == 'Low') {
    return(1)
  } else if(performanceRating == 'Good') {
    return(2)
  } else if(performanceRating == 'Excellent') {
    return(3)
  } else if(performanceRating == 'Outstanding') {
    return(4)
  } else {
    return(performanceRating)
  }
}

consolidate.jobSatisfaction <- function(jobSatisfaction) {
  if(jobSatisfaction == 'Low') {
    return(1)
  } else if(jobSatisfaction == 'Medium') {
    return(2)
  } else if(jobSatisfaction == 'High') {
    return(3)
  } else if(jobSatisfaction == 'Very High') {
    return(4)
  } else {
    return(jobSatisfaction)
  }
}
consolidate.stockoptionlevel <- function(solevel) {
  if(solevel == 'One') {
    return(1)
  } else if(solevel == 'Zero') {
    return(0)
  } else {
    return(solevel)
  }
}
#normalizacija trening seta
attrition_train$Education <- as.numeric(sapply(as.character(attrition_train$Education), consolidate.education, USE.NAMES=FALSE))
attrition_train$EnvironmentSatisfaction <- as.numeric(sapply(as.character(attrition_train$EnvironmentSatisfaction), consolidate.enviromentSatisfaction, USE.NAMES=FALSE))
attrition_train$JobInvolvement <- as.numeric(sapply(as.character(attrition_train$JobInvolvement), consolidate.jobInvolvment, USE.NAMES=FALSE))
attrition_train$JobSatisfaction <- as.numeric(sapply(as.character(attrition_train$JobSatisfaction), consolidate.jobSatisfaction, USE.NAMES=FALSE))
attrition_train$PerformanceRating <- as.numeric(sapply(as.character(attrition_train$PerformanceRating), consolidate.performanceRating, USE.NAMES=FALSE))
attrition_train$RelationshipSatisfaction <- as.numeric(sapply(as.character(attrition_train$RelationshipSatisfaction), consolidate.relationshipSatisfaction, USE.NAMES=FALSE))
attrition_train$WorkLifeBalance <- as.numeric(sapply(as.character(attrition_train$WorkLifeBalance), consolidate.workLifeBalance, USE.NAMES=FALSE))
attrition_train$JobLevel <- as.numeric(sapply(as.character(attrition_train$JobLevel), consolidate.jobLevel, USE.NAMES=FALSE))
attrition_train$StockOptionLevel <- as.numeric(sapply(as.character(attrition_train$StockOptionLevel), consolidate.stockoptionlevel, USE.NAMES=FALSE))



