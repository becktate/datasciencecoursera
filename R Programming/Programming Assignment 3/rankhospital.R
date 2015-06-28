rankhospital <- function(state, outcome, num = "best") {
    acceptableOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    data <- transform(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), 
                      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(as.character(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    
    ## Check that state and outcome are valid
    if(!(state %in% data$State))
        stop("invalid state")
    
    if(!(outcome %in% acceptableOutcomes))
        stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stateData <- split(data, data$State)
    outcomeNum <- getOutcomeNum(outcome)
    results <- lapply(stateData, sortByColumn, outcomeNum)
    stateResults <- results[c(state)]
    dataToReturn <- stateResults[[1]]
    
    if(identical(num, "best"))
        return(as.character(dataToReturn[1,2]))
    if(identical(num, "worst"))
        return(as.character(dataToReturn[nrow(dataToReturn),2]))
    else
        return(as.character(dataToReturn[num,2]))
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