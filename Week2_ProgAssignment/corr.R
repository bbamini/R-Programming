corr <- function(directory, threshold = 0) {
    
    id <- 1:332
    correlations <- c()
    
    for (i in id) {
        currfileid <- as.character(i)
        currfile <- if(i < 10) {
            read.csv(paste(directory, "/", "00", currfileid, ".csv", sep = ""))
        } else if (i >= 10 & i < 100) {
            read.csv(paste(directory, "/", "0", currfileid, ".csv", sep = ""))
        } else {
            read.csv(paste(directory, "/", currfileid, ".csv", sep = ""))
        }    
        
        completedata <- complete(directory, i)
        nobsval <- completedata[["nobs"]]
        
        if(nobsval > threshold) {
            usable <- complete.cases(currfile)
            usabledata <- currfile[usable, ]
            calculatedcor <- cor(usabledata[["sulfate"]], 
                                 usabledata[["nitrate"]])
            correlations <- append(correlations, calculatedcor)
        }
        }

    if(length(correlations) == 0){
        correlations <- numeric()
    }
    
    
    correlations
    
    
}