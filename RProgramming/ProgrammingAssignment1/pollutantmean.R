pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
  
    # Build CSV filenames from the id vector, save in the csv_filenames vector
    csv_filenames <- paste(formatC(id, width = 3, flag = "0"), "csv", sep = ".")
  
    # build the path to the data files, and save in the data_files vector
    data_files <- paste(getwd(), directory, csv_filenames, sep = "/")
    
    # Read all of the CSV files into the data_tables vector
    data_tables <- lapply(data_files, read.csv)

    # create the data fram of the desired files
    pollutant_data_frame <- do.call(rbind, data_tables)
    
    # extract the raw data we want
    raw_pollutant_data <- pollutant_data_frame[pollutant]
    
    # keep just the non NA values
    good_pollutant_data <- raw_pollutant_data[!is.na(raw_pollutant_data)]
    
    # calculate the mean of the data and make it look nice
    round(mean(good_pollutant_data), digits = 3)
    
}