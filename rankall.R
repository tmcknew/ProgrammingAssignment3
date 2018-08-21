rankall <- function( outcome_selected, num = "best" ) {
    
    ## Read outcome data from file, keep strings for now
    outcome_data <- read.csv( "data/outcome-of-care-measures.csv", 
                             stringsAsFactors = FALSE)
    # switch State to factor
    outcome_data$State <- as.factor( outcome_data$State )
    
    ## Check that state is valid
    # if ( !(state %in% levels( outcome_data$State ) ) ){
    #     # error, state invalid. halt.
    #     stop( "invalid state" )
    # }

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
    
    # subset on selected state
    # state_data <- subset( outcome_data, outcome_data$State == state )
    
    # convert selected outcome column to numeric for sort
    outcome_data[ , outcome_name ] <- as.numeric( outcome_data[ , outcome_name ] )

    # sort on State, outcome, name. store ordering
    ordering <- order(outcome_data[,"State"], 
                      outcome_data[ , outcome_name ], 
                      outcome_data[,"Hospital.Name"], na.last = NA)
    
    # drop unused columns
    outcome_data <- outcome_data[ ordering, c("State", "Hospital.Name", outcome_name)]
    
    # split on State
    state_data <- split(outcome_data, "State")
    
    ## convert num to ranking
    # state_data[ ordering, ][[1,"Hospital.Name"]]
    if (num == "best"){ num <- 1}
    # if (num == "worst"){ num <- length(ordering)}

    # if( num > length(ordering)){
    #     return_value = NA;
    # }
 
    
}

