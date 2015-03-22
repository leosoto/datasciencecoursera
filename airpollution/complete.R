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
    nobs <- numeric(length(id))
    for (index in seq_along(id)) {
        filename <- paste(sprintf("%03d", id[index]), ".csv", sep="")
        csv <- file.path(directory, filename)
        data <- read.csv(csv)
        nobs[index] <- length(which(!is.na(data$sulfate) & !is.na(data$nitrate)))
    }
    data.frame(id, nobs)
}