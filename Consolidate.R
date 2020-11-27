consolidate.education <- function(education) {
  if(education == 1) {
    return('Below College')
  } else if(education == 2) {
    return('College')
  } else if(education == 3) {
    return('Bachelor')
  } else if(education == 4) {
    return('Master')
  } else if(education == 5) {
    return('Doctor')
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
  if(eSatisfaction == 1) {
    return('Low')
  } else if(eSatisfaction == 2) {
    return('Medium')
  } else if(eSatisfaction == 3) {
    return('High')
  } else if(eSatisfaction == 4) {
    return('Very High')
  }else {
    return(eSatisfaction)
  }
}
consolidate.jobInvolvment <- function(jobInvolvment) {
  if(jobInvolvment == 1) {
    return('Low')
  } else if(jobInvolvment == 2) {
    return('Medium')
  } else if(jobInvolvment == 3) {
    return('High')
  } else if(jobInvolvment == 4) {
    return('Very High')
  } else {
    return(jobInvolvment)
  }
}
consolidate.workLifeBalance <- function(workLifeBalance) {
  if(workLifeBalance == 1) {
    return('Bad')
  } else if(workLifeBalance == 2) {
    return('Good')
  } else if(workLifeBalance == 3) {
    return('Better')
  } else if(workLifeBalance == 4) {
    return('Best')
  } else {
    return(workLifeBalance)
  }
}

consolidate.relationshipSatisfaction <- function(relationshipSatisfaction) {
  if(relationshipSatisfaction == 1) {
    return('Low')
  } else if(relationshipSatisfaction == 2) {
    return('Medium')
  } else if(relationshipSatisfaction == 3) {
    return('High')
  } else if(relationshipSatisfaction == 4) {
    return('Very High')
  } else {
    return(relationshipSatisfaction)
  }
}

consolidate.performanceRating <- function(performanceRating) {
  if(performanceRating == 1) {
    return('Low')
  } else if(performanceRating == 2) {
    return('Good')
  } else if(performanceRating == 3) {
    return('Excellent')
  } else if(performanceRating == 4) {
    return('Outstanding')
  } else {
    return(performanceRating)
  }
}

consolidate.jobSatisfaction <- function(jobSatisfaction) {
  if(jobSatisfaction == 1) {
    return('Low')
  } else if(jobSatisfaction == 2) {
    return('Medium')
  } else if(jobSatisfaction == 3) {
    return('High')
  } else if(jobSatisfaction == 4) {
    return('Very High')
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
atrain$Education <- as.factor(sapply(as.character(atrain$Education), consolidate.education, USE.NAMES=FALSE))
atrain$EnvironmentSatisfaction <- as.factor(sapply(as.character(atrain$EnvironmentSatisfaction), consolidate.enviromentSatisfaction, USE.NAMES=FALSE))
atrain$JobInvolvement <- as.factor(sapply(as.character(atrain$JobInvolvement), consolidate.jobInvolvment, USE.NAMES=FALSE))
atrain$JobSatisfaction <- as.factor(sapply(as.character(atrain$JobSatisfaction), consolidate.jobSatisfaction, USE.NAMES=FALSE))
atrain$PerformanceRating <- as.factor(sapply(as.character(atrain$PerformanceRating), consolidate.performanceRating, USE.NAMES=FALSE))
atrain$RelationshipSatisfaction <- as.factor(sapply(as.character(atrain$RelationshipSatisfaction), consolidate.relationshipSatisfaction, USE.NAMES=FALSE))
atrain$WorkLifeBalance <- as.factor(sapply(as.character(atrain$WorkLifeBalance), consolidate.workLifeBalance, USE.NAMES=FALSE))
atrain$JobLevel <- as.factor(sapply(as.character(atrain$JobLevel), consolidate.jobLevel, USE.NAMES=FALSE))
atrain$StockOptionLevel <- as.factor(sapply(as.character(atrain$StockOptionLevel), consolidate.stockoptionlevel, USE.NAMES=FALSE))


#normalizacija testnog seta
atest$Education <- as.factor(sapply(as.character(atest$Education), consolidate.education, USE.NAMES=FALSE))
atest$EnvironmentSatisfaction <- as.factor(sapply(as.character(atest$EnvironmentSatisfaction), consolidate.enviromentSatisfaction, USE.NAMES=FALSE))
atest$JobInvolvement <- as.factor(sapply(as.character(atest$JobInvolvement), consolidate.jobInvolvment, USE.NAMES=FALSE))
atest$JobSatisfaction <- as.factor(sapply(as.character(atest$JobSatisfaction), consolidate.jobSatisfaction, USE.NAMES=FALSE))
atest$PerformanceRating <- as.factor(sapply(as.character(atest$PerformanceRating), consolidate.performanceRating, USE.NAMES=FALSE))
atest$RelationshipSatisfaction <- as.factor(sapply(as.character(atest$RelationshipSatisfaction), consolidate.relationshipSatisfaction, USE.NAMES=FALSE))
atest$WorkLifeBalance <- as.factor(sapply(as.character(atest$WorkLifeBalance), consolidate.workLifeBalance, USE.NAMES=FALSE))
atest$JobLevel <- as.factor(sapply(as.character(atest$JobLevel), consolidate.jobLevel, USE.NAMES=FALSE))
atest$StockOptionLevel <- as.factor(sapply(as.character(atest$StockOptionLevel), consolidate.stockoptionlevel, USE.NAMES=FALSE))

