rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    o <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
    ## Tidy Up ##
    suppressWarnings(o$ha <- as.numeric(o[,11]))
    suppressWarnings(o$hf <- as.numeric(o[,17]))
    suppressWarnings(o$p <- as.numeric(o[,23]))
    ## Check that state and outcome are valid
    data(state)
    if(!(state %in% state.abb)){
        stop("invalid state")
    }
    validOutcome <- c("heart attack","heart failure","pneumonia")
    if(!(outcome %in% validOutcome)){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # set data to input state
    o <- o[o$State==state,]
    # figure out which column to sort on and clean
    if(outcome==validOutcome[1]){
        o <- o[order(o$ha,o$Hospital.Name,na.last=NA),]
    }
    else if(outcome==validOutcome[2]){
        o <- o[order(o$hf,o$Hospital.Name,na.last=NA),]        
    }
    else if(outcome==validOutcome[3]){
        o <- o[order(o$p,o$Hospital.Name,na.last=NA),]        
    }
    if(num=="best"){
        num<-1
    }
    if(num=="worst"){
        num<-length(o$Hospital.Name)
    }
    return(o$Hospital.Name[num])
}
