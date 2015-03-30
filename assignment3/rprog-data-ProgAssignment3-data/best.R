best <- function(state, outcome) {
    # Part of assignment of week 4 of R programming
    
    ## state is a 2 char state variable
    
    ## outcome should be one of “heart attack”, “heart failure”, or “pneumonia"
    
    ## step 1:
    ## read the data from outcome-of-care-measures.csv
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    allstates <- unique(data[, c("State")])
    if (!(state %in% allstates)) {
        stop ('invalid state')
    }
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {        
        stop("invalid outcome")
    }
    
    ## now find the best hospital in the state 
    # must find the col number to look at !
    colnum <- -1
    if (outcome == "heart attack") {
        colnum <- 11
    } else if (outcome == "heart failure") {
        colnum <- 17
    } else if (outcome == "pneumonia") {
        colnum <- 23
    } else {
        stop("invalid outcome")
    }
    
    
    # convert the desired column to numeric
    #x <- data[, 17]
    #print (x)
    #data[, 11] <- as.numeric(data[, 11], stringsAsFactors = FALSE)
    #data[, 17] <- as.numeric(x, stringsAsFactors = FALSE)
    #x <-  as.numeric(x, stringsAsFactors = FALSE)
    #data[, 23] <- as.numeric(data[, 23], stringsAsFactors = FALSE)
    #data[,colnum] <- as.numeric(data[, colnum])
    #print (x)
    
    
    # convert the desired column to numeric
    data[, colnum] <- as.numeric(data[, colnum])
    # only keep data for a particular state
    data_state <- data[with(data, State == state), ]
    # remove na values from the desired column
    
    data_state <- data_state[complete.cases(data_state[, colnum]), ]
    
    # sort the data on the desired column 
    data_state <- data_state[order(data_state[,colnum], data_state[,2]), ]
    
    bob <-  as.character(data_state[[1, 2]])
    bob
    
    #print (class(x))
    #x
}