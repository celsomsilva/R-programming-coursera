rankall <- function(outcome, num = "best") {
  
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    #best <- function(state, outcome) {
    ## Read outcome data
    outcomeData = read.csv("outcome-of-care-measures.csv", na.strings="Not Available",stringsAsFactors=FALSE )
    
    result=c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23) #e depois names(vetor)
    vNames = names(result)
    
    if(!(outcome %in% vNames)){
      print("invalid outcome")
      return()
    }
    else if(!(state %in% outcomeData$State)){
      print("invalid state")
      return()
    }
    
    bad = is.na(outcomeData[,result[outcome]])
    outcomeDataOK = outcomeData[!bad, ]
    
    states = unique(outcomeDataOK[, 7])
    outcomeDataOK = outcomeDataOK[, c(2, 7, result[outcome])] 
    rank_in_state = function(state) {
      df <- outcomeDataOK[outcomeDataOK[, 2] == state, ]
      if(num == "best"){
        num = 1
      }
      else if(num == "worst"){
        num = nrow(df)
      }
      sort = order(df[, 3], df[, 1])
      rs = df[sort, ][num, 1]
      c(rs, state)
    }
    output = do.call(rbind, lapply(states, rank_in_state))
    output = output[order(output[, 2]), ]
    rownames(output) = output[, 2]
    colnames(output) = c("hospital", "state")
    data.frame(output)
}