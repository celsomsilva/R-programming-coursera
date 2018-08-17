best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",stringsAsFactors=FALSE )
  
  result<-c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23) #e depois names(vetor)
  vNames <- names(result)
  
  if(!(outcome %in% vNames)){
    print("invalid outcome")
    return()
  }
   else if(!(state %in% outcomeData$State)){
      print("invalid state")
      return()
   }
  
  bad <- is.na(outcomeData[,result[outcome]])
  
  outcomeDataOK <- outcomeData[!bad, ]
  
  df <- outcomeDataOK[outcomeDataOK$State == state, c(2,result[outcome])]
  df[which.min(df[,2]),1]
  
}