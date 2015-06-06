pollutantmean <- function(directory, pollutant, id = 1:332) {
  if (substring(directory, nchar(directory)) != "/") {
    directory <- paste(directory, "/", sep = "");
  }
  
  polls <- vector()
  
  for(i in id) {
    table <- read.csv(paste(directory, paste(formatC(i, width=3, flag="0"), ".csv", sep=""), sep=""))
    
    temp_polls <- table[,pollutant];
    
    valid <- !is.na(temp_polls);
    polls <- c(polls, temp_polls[valid])
  }
  return(mean(polls))
  
}