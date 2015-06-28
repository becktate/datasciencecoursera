rankall <- function(outcome, num = "best") {
    acceptableOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    data <- transform(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), 
                      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    
    ## Check that state and outcome are valid
    if(!(outcome %in% acceptableOutcomes))
        stop("invalid outcome")
    
    
    ## For each state, find the hospital of the given rank
    stateData <- split(data, data$State)
    outcomeNum <- getOutcomeNum(outcome)
    
    sortedStateData <- lapply(stateData, sortByColumn, outcomeNum)
    
    if(identical(num, "best"))
        return(getBestPerState(sortedStateData))
    if(identical(num, "worst"))
        return(getWorstPerState(sortedStateData))
     else
        return(getRankPerState(sortedStateData, num))
}

getOutcomeNum <- function(outcome){
    if(identical(outcome, "heart attack"))
        return(11)
    if(identical(outcome, "heart failure"))
        return(17)
    if(identical(outcome, "pneumonia"))
        return(23)
}

sortByColumn <- function(x, colNum){
    dataFrame <- as.data.frame(x)
    dataFrame <- dataFrame[complete.cases(dataFrame[,colNum]),]
    returnData <- dataFrame[order(dataFrame[,colNum],dataFrame[,2]),]
    returnData
}

getBestPerState <- function(sortedStateData){
    frames <- lapply(sortedStateData, returnBest)
    returnData <- do.call("rbind", frames)
    returnData
}

getWorstPerState <- function(sortedStateData){
    frames <- lapply(sortedStateData, returnWorst)
    returnData <- do.call("rbind", frames)
    returnData
}

getRankPerState <- function(sortedStateData, rank){
    frames <- lapply(sortedStateData, returnRank, rank)
    returnData <- do.call("rbind", frames)
    returnData
}

returnBest <- function(x){
    dataFrame <- as.data.frame(x)
    options(stringsAsFactors=FALSE)
    returnData <- data.frame(hospital = character(), state = character())
    returnData[1,1] = as.character(dataFrame[1,2])
    returnData[1,2] = as.character(dataFrame[1,7])
    returnData
}

returnWorst <- function(x){
    dataFrame <- as.data.frame(x)
    options(stringsAsFactors=FALSE)
    last = nrow(dataFrame)
    returnData <- data.frame(hospital = character(), state = character())
    returnData[1,1] = as.character(dataFrame[last,2])
    returnData[1,2] = as.character(dataFrame[last,7])
    returnData
}

returnRank <- function(x, rank){
    dataFrame <- as.data.frame(x)
    options(stringsAsFactors=FALSE)
    returnData <- data.frame(hospital = character(), state = character())
    returnData[1,1] = as.character(dataFrame[rank,2])
    returnData[1,2] = as.character(dataFrame[1,7])
    returnData
}