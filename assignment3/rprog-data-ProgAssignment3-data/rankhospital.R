rankhospital <- function(state,outcome, num = "best") {
    # Part of assignment of week 4 of R programming
    
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    allstates <- unique(data[, c("State")])
    if (!(state %in% allstates)) {
        stop ('invalid state')
    }
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {        
        stop("invalid outcome")
    }
    
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
    data[, colnum] <- as.numeric(data[, colnum])
    # only keep data for a particular state
    data_state <- data[with(data, State == state), ]
    # remove na values from the desired column    
    data_state <- data_state[complete.cases(data_state[, colnum]), ]
    
    # sort the data on the desired column 
    data_state <- data_state[order(data_state[,colnum], data_state[,2]), ]
    
    rankinghospital <- data_state[,2] 
    
        
    hlen <- length(rankinghospital)
        
    if (hlen == 0) {
        return(NA)
    } else if (num == "best")
    {
        return (rankinghospital[1])
    }
    else if (num == "worst")
    {
        return (rankinghospital[hlen])
    } else {
        if (num > hlen) {
            return (NA)
        }
        else {
            return (rankinghospital[num])
        }
    }   
}