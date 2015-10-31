rankall <- function(outcome, num = "best") {
    ## Read outcome data
    o <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
    ## Tidy Up ##
    suppressWarnings(o$ha <- as.numeric(o[,11]))
    suppressWarnings(o$hf <- as.numeric(o[,17]))
    suppressWarnings(o$p <- as.numeric(o[,23]))
    ## Check that state and outcome are valid
    validOutcome <- c("heart attack","heart failure","pneumonia")
    if(!(outcome %in% validOutcome)){
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data(state)
    state.abb <- state.abb[order(state.abb)]
    results <- data.frame(hospital=character(),state=character())
    for(i in state.abb){
        oState <- o[o$State==i,]
        # figure out which column to sort on and clean
        if(outcome==validOutcome[1]){
            oState <- oState[order(oState$ha,oState$Hospital.Name,na.last=NA),]
        }
        else if(outcome==validOutcome[2]){
            oState <- oState[order(oState$hf,oState$Hospital.Name,na.last=NA),]
        }
        else if(outcome==validOutcome[3]){
            oState <- oState[order(oState$p,oState$Hospital.Name,na.last=NA),]
        }
        if(num=="best"){
            num<-1
        }
        else if(num=="worst"){
            numState<-length(oState$Hospital.Name)
        }
        else{
            numState <- num
        }
        results <- rbind(results,data.frame(hospital=oState$Hospital.Name[numState],state=i))
    }
    return(results)
}
