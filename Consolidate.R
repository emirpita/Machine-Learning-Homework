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
    return(color)
  }
}

atrain$Education <- as.factor(sapply(as.character(atrain$Education), consolidate.education, USE.NAMES=FALSE))
