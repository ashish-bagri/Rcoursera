rankall <- function(outcome, num= "best") {
    # Part of assignment of week 4 of R programming
    # rank hospitals from each state in the data based on outcome 
    # num can be best, worst or a ranking 
    
    # read the csv data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check outcome input 
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {        
        stop("invalid outcome")
    }
    
    # select the column of the input data based on the outcome
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
    
    # convert to numeric values
    data[, colnum] <- as.numeric(data[, colnum])
    # remove NA values
    data <- data[complete.cases(data[, colnum]), ]
    
    # create a new data frame with only the select columns
    outcome_data <- data.frame(deaths = data[, colnum], State = data[, 7], HospitalName = data[, 2])
    
    # split the data based on the states
    myList <- split(outcome_data[,c("deaths", "HospitalName")], outcome_data[, c("State")])    

    # helper variable to check if the split includes all data or not 
    count <- 0
    # vectors to form the final data frame of the return value
    hospitalvector <- c()
    statevector <- c()
    
    # solve per state and get the answer here !
    for (statename in names(myList)) {
            # fill the state vector with the state name
            statevector <- c(statevector, statename)
            # state data is a data frame
            statedata <- myList[[statename]]
            
            count <- count + nrow(statedata)
            
            # get the index of  the ordering. NOTE:: THE DATA ITSELF IS NOT ORDERED SO USE INDEXING ON orderedrows
            orderedrows <- order(statedata[, 1], statedata[, 2])
            
            # get the actual value of the rankings 
            rankinghospital <- as.vector(statedata[, 2])
            
            # find the index to use of the ranking hospital 
            indx <- -1
            if (num == "best")
            {
                indx<- 1
            }
            else if (num == "worst")
            {
                indx <- length(orderedrows)
            } 
            else {
                if (num > length(orderedrows)) {
                    indx <- -1
                }
                else {
                    indx <- num
                }
            }
            if (indx == -1)
            {
                hospitalvector <- c(hospitalvector, NA)
            }
            else
            {
                hospitalvector <- c(hospitalvector, rankinghospital[orderedrows[indx]])
            }
    }
    # for the retun data frame
    res <- data.frame(hospital = hospitalvector, state = statevector)
    res
}