pollutantmean <- function(directory, pollutant, id = 1:332) {
    totalpollutantvect <- c()
    for (i in id) {
        currfileid <- as.character(i)
        currfile <- if(i < 10) {
            read.csv(paste(directory, "/", "00", currfileid, ".csv", sep = ""))
            } else if (i >= 10 & i < 100) {
            read.csv(paste(directory, "/", "0", currfileid, ".csv", sep = ""))
            } else {
            read.csv(paste(directory, "/", currfileid, ".csv", sep = ""))
            }
        
        totalpollutantvect <- append(totalpollutantvect, currfile[[pollutant]])
    }
    
    NAvals <- is.na(totalpollutantvect)
    newpollutantvect <- totalpollutantvect[!NAvals]
    mean(newpollutantvect)
}