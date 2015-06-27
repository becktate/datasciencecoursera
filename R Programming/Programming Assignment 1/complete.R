complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    results <- data.frame(id = numeric(), nobs = numeric())
    numFiles <- 0
    
    for(monitor in id){
        numFiles <- numFiles + 1
        fileName <- paste0(directory, "/", formatC(monitor, width = 3, format = "d", flag = "0"), ".csv")
        data <- read.csv(fileName)
        nobsPerMonitor <- sum(complete.cases(data))
        results[numFiles, 1] = monitor
        results[numFiles, 2] = nobsPerMonitor
    }
    
    results
}