best <- function(state, outcome) {
    acceptableOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    if(!(state %in% data$State))
        stop("invalid state")
    
    if(!(outcome %in% acceptableOutcomes))
        stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    stateData <- getStateData(data, state)
    hospital <- findBestHospital(stateData, outcome)
    as.character(hospital[,2])
}

getStateData <- function(data, state){
    splitData <- split(data, data$State)
    stateData <- splitData[c(state)]
    stateData <- as.data.frame(stateData[[1]])
    returnData <- stateData[order(stateData[,2]),]
    returnData
}

findBestHospital <- function(dataToUse, outcome){
    if(identical(outcome, "heart attack")){
        minIndex <- which.min(as.numeric(as.character(dataToUse[,11])))
        return(dataToUse[minIndex,])
    }
    
    if(identical(outcome, "heart failure")){
        minIndex <- which.min(as.numeric(as.character(dataToUse[,17])))
        return(dataToUse[minIndex,])
    }
    
    if(identical(outcome, "pneumonia")){
        minIndex <- which.min(as.numeric(as.character(dataToUse[,23])))
        return(dataToUse[minIndex,])
    }
}