best <- function(state, outcome) {
    ## Read outcome data
    outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state %in% outcome[,7])){
        # error, state invalid
        stop("invalid state")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death rate
}