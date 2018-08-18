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
    # make vector of possible selections
    table_cols <- c(11,17,23)
    # make data.frame lookup of selections matched to corresponding 
    # table cols in data
    lookup <- data.frame(outcome_options, table_cols)
    # get column number from lookup
    found <- match(outcome_selected, outcome_options, nomatch = 0)
    column <- lookup[ found , "table_cols" ]
    # if we didn't fiund a column, exit
    if (length(column) == 0) {
        stop("invalid outcome")
     }
    print(column)
    print(lookup[lookup$table_cols == column,])
    ## Return hospital name in that state with lowest 30-day death rate
}

