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
  
#     # Build CSV filenames from the id vector, save in the csv_filenames vector
#     csv_filenames <- paste(formatC(id, width = 3, flag = "0"), "csv", sep = ".")
#   
#     # build the path to the data files, and save in the data_files vector
#     data_files <- paste(getwd(), directory, csv_filenames, sep = "/")
#   
#     # Read all of the CSV files into the data_tables vector
#     data_tables <- lapply(data_files, read.csv)
#   
#     # create the data fram of the desired files
#     pollutant_data_frame <- do.call(rbind, data_tables)
#   
#     # Number of complete observations
#     complete_pollutant_data <- pollutant_data_frame[complete.cases(pollutant_data_frame),]
# 
    # Create a vector, nobs, that is as long as id
    nobs <- rep(NA, length(id))
    
    # determine number of observations for each ID
    for(site in id) {
        csv_filename <- paste(formatC(site, width = 3, flag = "0"), "csv", sep = ".")
        data_file <- paste(getwd(), directory, csv_filename, sep = "/")
        pollutant_data_frame <- read.csv(data_file)
        complete_pollutant_data <- pollutant_data_frame[complete.cases(pollutant_data_frame),]
#         nobs[[match(site, id)]] <- nrow(complete_pollutant_data[complete_pollutant_data$ID == site,])
        nobs[[match(site, id)]] <- nrow(complete_pollutant_data)
    }

    # Create a new data frame with id and nobs
    as.data.frame(cbind(id, nobs))

}