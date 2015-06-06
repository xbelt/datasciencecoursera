complete <- function(directory, id = 1:332) {
  if (substring(directory, nchar(directory)) != "/") {
    directory <- paste(directory, "/", sep = "");
  }
  
  ids <- vector();
  nobs <- vector();
  
  for (i in id) {
    ids <- c(ids, i);
    table <- read.csv(paste(directory, paste(formatC(i, width=3, flag="0"), ".csv", sep=""), sep=""))
    
    table2 <- table[complete.cases(table),]
    nobs <- c(nobs, nrow(table2))
    
  }
  
  return(data.frame(id = ids, nobs = nobs))
}