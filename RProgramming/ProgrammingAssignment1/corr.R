corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
  
    # use complete() to get the number of completely observed
    # observations in the dataset
    completely_observed <- complete(directory)
    
    # monitors that meet or exceed the threshold
    meet_threshold <- completely_observed[completely_observed$nobs > threshold,]

    if (nrow(meet_threshold) > 0) {
        # Build CSV filenames from the id vector, save in the csv_filenames vector
        csv_filenames <- paste(formatC(meet_threshold[, "id"], width = 3, flag = "0"), "csv", sep = ".")
        
        # build the path to the data files, and save in the data_files vector
        data_files <- paste(getwd(), directory, csv_filenames, sep = "/")
        
        # Create a vector, correlations, that is as long as 
        # the number of files that meet the threshold
        correlations <- rep(NA, nrow(meet_threshold))
        
        # determine number of observations for each ID
        for(filename in data_files) {
          csv_data <- read.csv(filename)
          nitrate <- csv_data[,"nitrate"]
          sulfate <- csv_data[,"sulfate"]
          correlation <- cor(nitrate, sulfate, use = "complete.obs")
          correlations[[match(filename, data_files)]] <- correlation
        }
    } else {
        correlations <- numeric()
    }
    
    # return the correlations vector
    correlations
    
}