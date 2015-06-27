corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    results <- numeric()
    files <- list.files(paste0(directory, "/"))
    numFiles <- 0
    
    for(file in files){
        data <- read.csv(paste0(directory, "/", file))
        complete <- complete.cases(data)
        if(sum(complete) > threshold){
            numFiles <- numFiles + 1
            results[numFiles] = cor(data$sulfate[complete], data$nitrate[complete])
        }
    }
    results
}
