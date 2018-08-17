best <- function(state, outcome_selected) {
    ## Read outcome data
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state %in% outcome_data[,7])){
        # error, state invalid
        stop("invalid state")
    }
    # heart attack”, “heart failure”, or “pneumonia”
    outcome_options <- c("heart attack", "heart failure", "pneumonia")
    table_cols <- c(11,17,23)
    lookup <- data.frame(outcome_options, table_cols)
    column <- lookup[ match(outcome_selected, outcome_options, nomatch=0), table_cols ]
    #column <- table_cols[ which( input == lookup$options ) ]
    #selected <- match(outcome, options, nomatch=0)
    
    ## Return hospital name in that state with lowest 30-day death rate
}