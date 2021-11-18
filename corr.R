source("complete.R")

corr <- function(directory, threshold = 0) {
    c <- logical(0)
    dir <- paste(getwd(), directory, sep = "/") ## locate the CSV files
    for(i in 1:332){                               ## scan the files
        if (complete(directory, i)$nobs > threshold){
            if(i<10){
                name <- paste("00", i, ".csv", sep = "")
            } else if(10 <= i && i <100){
                name <- paste("0", i, ".csv", sep = "")
            } else{
                name <- paste(i, ".csv", sep = "")
            }
            oridata <- read.csv(paste(dir, name, sep = "/"))  ## read file
            good1 <- complete.cases(oridata[, "sulfate"])
            revdata1 <- oridata[good1, ]
            good2 <- complete.cases(revdata1[, "nitrate"])
            revdata2 <- revdata1[good2, ]
            x <- cor(revdata2[,"sulfate"], revdata2[, "nitrate"])
            c <- c(c,x)
        }
    }
    
    return (c)
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
}

