corr <- function(directory, threshold = 0){

  df <- complete(directory)
  dfcor <- df[df$nobs > threshold,]
  vec <- vector("numeric")
  for(i in dfcor[,"id"]){
    
    #formatting the numbers 
    name <- sprintf("%03d",i)
    #formatting file names
    file <- paste(name, ".csv", sep="")
    myData <- read.csv(paste(directory, file, sep="/"))
    noNa <- myData[complete.cases(myData),]
    vec <- c(vec,cor(noNa$sulfate, noNa$nitrate))
  }
  
    vec
}
  


