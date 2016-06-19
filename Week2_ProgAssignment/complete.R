complete <- function(directory, id = 1:332) {
    
    summaryfiles <- data.frame()
    
    for (i in id) {
        currfileid <- as.character(i)
        currfile <- if(i < 10) {
            read.csv(paste(directory, "/", "00", currfileid, ".csv", sep = ""))
        } else if (i >= 10 & i < 100) {
            read.csv(paste(directory, "/", "0", currfileid, ".csv", sep = ""))
        } else {
            read.csv(paste(directory, "/", currfileid, ".csv", sep = ""))
        }
        
        obs <- sum(complete.cases(currfile))
        rowvect <- c(i, obs)
        
        summaryfiles <- rbind(summaryfiles, rowvect)
        
    }
    
    names(summaryfiles) <- c("id", "nobs")
    
    summaryfiles
}