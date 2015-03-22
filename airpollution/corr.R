corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    files <- list.files(directory, full.names=TRUE)
    corrs <- numeric(length(files))
    nobs <- numeric(length(files))
    for (index in seq_along(files)) {
        data <- read.csv(files[index])
        nobs[index] <- length(which(!is.na(data$sulfate) & !is.na(data$nitrate)))
        corrs[index] <-
            cor(data[complete.cases(data),"nitrate"], data[complete.cases(data),"sulfate"])
        
    }
    corrs[nobs > threshold]
}