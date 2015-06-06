corr <- function(directory, threshold = 0) {
  if (substring(directory, nchar(directory)) != "/") {
    directory <- paste(directory, "/", sep = "");
  }
  
  source("complete.R");
  
  completes <- complete(directory);
  correlation <- vector()
  completes <- subset(completes, nobs > threshold)
  
  
  for(i in completes[, "id"]) {
    table <- read.csv(paste(directory, paste(formatC(i, width=3, flag="0"), ".csv", sep=""), sep=""))
    
    table2 <- table[complete.cases(table),]
    
    correlation <- c(correlation, cor(table2[, "nitrate"], table2[, "sulfate"]))
  }
  return(correlation)
}