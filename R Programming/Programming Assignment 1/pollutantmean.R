pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    data <- readCsvFiles(directory, id)
    pollutantData <- getPollutantValues(data, pollutant)
    cleansedData <- removeNAValues(pollutantData)
    mean <- mean(cleansedData)
    mean
}


readCsvFiles <- function(directory, id){
    numFiles <- 0
    
    for(monitor in id){
        numFiles <- numFiles + 1
        fileName <- paste0(directory, "/", formatC(monitor, width = 3, format = "d", flag = "0"), ".csv")
        
        if(!exists("dataSet")){
            dataSet <- read.csv(fileName)
        }
        else {
            tempDataSet <- read.csv(fileName)
            dataSet <- rbind(dataSet, tempDataSet)
            rm(tempDataSet)
        }
    }
    dataSet
}


getPollutantValues <- function(data, pollutant){
    if("sulfate" %in% pollutant){
        data$sulfate
    } else {
        data$nitrate
    }
}


removeNAValues <- function(pollutantData){
    badData <- is.na(pollutantData)
    pollutantData[!badData]
}
