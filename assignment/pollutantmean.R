


pollutantmean <- function(directory, pollutant, id = 1:332){

x <- 0
y <- 0
for (i in id) {
  #formatting the numbers 
  name <- sprintf("%03d",i)
  #formatting file names
  file <- paste(name, ".csv", sep="")
  myData <- read.csv(paste(directory, file, sep="/"))
  col <- myData[pollutant]
  col <- col[!is.na(col)]
  y <- y + sum(col)
  x <- x + length(col)
}

y / x

}

