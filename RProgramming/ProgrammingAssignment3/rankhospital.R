rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
  
    ## Check that state and outcome are valid
  
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    # Fixed part of Column Name
    fixed_col_name_part <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    # Read in the outcome data
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character",
                             na.strings = c("NA", "N/A", "Not Available"))
    
    # Verify that the supplied state is valid
    if (!(state %in% outcome_data$State)) {
      # Invalid State
      stop("invalid state")
    }
    
    # convert the supplied "outcome" to be usable
    split_string <- strsplit(tolower(outcome), " ")[[1]]
    new_outcome <- paste(toupper(substring(split_string, 1, 1)), substring(split_string, 2),
                         sep = "", collapse = ".")
    actual_colname <- paste(fixed_col_name_part, new_outcome, sep = "", collapse = "")
    
    # Verify that the "new" outcome is valid
    if (!(actual_colname %in% colnames(outcome_data))) {
      # Invalid outcome requested
      stop("invalid outcome")
    }
    
    # Subset the data so we are only looking at the state requested
    state_subset <- subset(outcome_data, State == state,
                           select = c("Hospital.Name", actual_colname))
    
    # Sort the results first by the outcome percentage,
    # then by the Hospital Name
    sorted_state_subset <- state_subset[order(as.numeric(state_subset[, actual_colname]),
                                              state_subset[, "Hospital.Name"], na.last = NA), ]

    # Now to print out the specific hospital that was chosen
    if (tolower(num) == "first" | tolower(num) == "best") {
        sorted_state_subset[, "Hospital.Name"][1]
    } else if (tolower(num) == "last" | tolower(num) == "worst") {
        tail(sorted_state_subset[, "Hospital.Name"], n = 1)
    } else {
        sorted_state_subset[, "Hospital.Name"][num]
    }
  
}