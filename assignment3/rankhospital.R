rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  #best <- function(state, outcome) {
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
    #outcomeSplit <- split(outcomeDataOK,outcomeDataOK$State,drop = TRUE)
    #outcomeState <- outcomeSplit[[state]]
    
    #best <- tapply(outcomeState$Hospital.Name ,outcomeState[result[outcome]], min)
    
    if(num == "best"){
      num <- 1
    }
    else if(num == "worst"){
      num <- nrow(df)
    }
    
    sort <- order(df[, 2], df[, 1])
    df[sort, ][num, 1]
    #df <- rbind(df, data.frame(id = i, nobs = row))
    #df <- data.frame(id = numeric(), nobs = numeric())
    #data.frame(hospital=unlisted_values, state=list_names, row.names=list_names)
    
  #}
  
}