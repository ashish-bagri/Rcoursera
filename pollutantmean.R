pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## calculates the mean of the pollutant sulphate or nitrate
    ## mean across the id 
    cwd <- getwd()
    setwd(directory)
    sum_ele <- 0
    count_ele <- 0
    for (fileid in id) {
        if (fileid < 10)
        {
            fileid <- paste("00", fileid, sep="")
        }
        if (fileid >= 10 && fileid < 100)
        {
            fileid <- paste( "0", fileid, sep="")
        }
        d <- read.csv(file=paste(fileid, ".csv", sep=""))
        s <- sum(d[,pollutant], na.rm = T)
        sum_ele <- sum_ele + s
        co <- length(na.omit(d[,pollutant]))
        count_ele <- count_ele + co
    }
    setwd(cwd)
    sum_ele / count_ele
}
    