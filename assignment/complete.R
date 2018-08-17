complete <- function(directory, id = 1:332){
  
  df <- data.frame(id = numeric(), nobs = numeric())
  names(df)<-c("id","nobs")
   
  for (i in id) {
    #formatting the numbers 
    name <- sprintf("%03d",i)
    #formatting file names
    file <- paste(name, ".csv", sep="")
    myData <- read.csv(paste(directory, file, sep="/"))
    noNa <- myData[complete.cases(myData),]
    row <- nrow(noNa)
    df <- rbind(df, data.frame(id = i, nobs = row))
  }
  
  df
}