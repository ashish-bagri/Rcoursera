corr <- function(directory, threshold = 0){
    
    # dir with csv files
    id = 1:332
    cwd <- getwd()
    setwd(directory)
    corr_vec <- vector(length = 0)
    
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
        if (sum(complete.cases(d)) > threshold)
        {
            # include    the data of this csv into correlation
            x <- cor(d[,"sulfate"], d[, "nitrate"], use="complete.obs")
            corr_vec <- c(corr_vec, x)
        }        
    }
    setwd(cwd)
    # can do summary(corr_vec)
    # head(corr_vec)
    corr_vec
}
    