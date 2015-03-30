complete <- function(directory, id = 1:332) {
    # dir has the csv files
    # id is the file names
    # find number of complete entries in each data point
    # create a data frame id nobs
    cwd <- getwd()
    setwd(directory)    
    B <- matrix(nrow=length(id), ncol=2)
    x <- c()
    y <- c()
    indx <- 1
    for (fileid in id) {
        myid <- fileid
        if (fileid < 10)
        {
            fileid <- paste("00", fileid, sep="")
        }
        if (fileid >= 10 && fileid < 100)
        {
            fileid <- paste( "0", fileid, sep="")
        }
        d <- read.csv(file=paste(fileid, ".csv", sep=""))
        
        
        # find the number of elements which are non NA
        x <- c(x, myid)
        y <- c(y, sum(complete.cases(d)))
        #B[indx,1] <- myid
        #B[indx,2] <- sum(complete.cases(d))
        indx <- indx + 1
    }
    setwd(cwd)
    data.frame(id = x, nobs= y)    
}
    