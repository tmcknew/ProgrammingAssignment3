rankall <- function( outcome_selected, num = "best" ) {
    
    ## Read outcome data from file, keep strings for now
    outcome_data <- read.csv( "data/outcome-of-care-measures.csv", 
                             stringsAsFactors = FALSE)
    # switch State to factor
    outcome_data$State <- as.factor( outcome_data$State )
    
    # create dataframe "lookup" for selection and column reference
    outcome_options <- c( "heart attack", "heart failure", "pneumonia" )
    table_cols <- c( 11, 17, 23 )
    lookup <- data.frame( outcome_options, table_cols )
    # get column number from lookup
    found <- match( outcome_selected, outcome_options, nomatch = 0 )
    # if we didn't match text, it's time to exit
    if ( found == FALSE ) {
        stop( "invalid outcome" )
    }
    # otherwise get index of column and column name for selected outcome
    outcome_column <- lookup[ found , "table_cols" ]
    outcome_name <- colnames( outcome_data )[ outcome_column ]
    
    # convert selected outcome column to numeric for sort
    outcome_data[ , outcome_name ] <- as.numeric( outcome_data[ , outcome_name ] )
    # sort on State, outcome, name. store ordering
    ordering <- order(outcome_data[,"State"], 
                      outcome_data[ , outcome_name ], 
                      outcome_data[,"Hospital.Name"], na.last = NA)
    # drop unused 
    outcome_data <- outcome_data[ ordering, 
                                  c("State", "Hospital.Name", outcome_name)]
        
    # split on State
    state_data <- split(outcome_data, outcome_data$State)
    
    # vectors for states, hopsitals
    states <- names(state_data)
    hospitals <- vector( mode = "character", length = 0)

    ## convert num to ranking
    if (num == "best"){num <- 1}
    
    # loop over list of df, put hospital in vector
    for (this_state in state_data) {
         if (num == "worst"){
            hospitals <- c(hospitals,this_state[nrow(this_state),"Hospital.Name"])
        }
        else {
            hospitals <- c(hospitals,this_state[num,"Hospital.Name"])
        }
    }
    
    # return dataframe
    data.frame(states, hospitals)
    
}

