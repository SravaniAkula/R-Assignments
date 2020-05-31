best <- function(state, outcome) {
    ## Read outcome data
    actual<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    p<-unique(actual[[7]])
    
    ## Check that state and outcome are valid
    if (!state %in% p){
        stop('invalid state')
    } 
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    data<-subset(actual,State==state)
    if(outcome == "heart attack"){
        col <-  11
    }
    else if(outcome == "heart failure"){
        col <- 17
    }
    else if(outcome == "pneumonia"){
        col <- 23
    }
    
    newd<-which(as.numeric(data[,col])==min(as.numeric(data[,col]),na.rm=TRUE))
    names<-data[newd,2]
    names<-sort(names)
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    names[1]
}




rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    actual<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    p<-unique(actual[[7]])
    
    ## Check that state and outcome are valid
    if (!state %in% p){
        stop('invalid state')
    } 
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    data<-subset(actual,State==state)
    if(outcome == "heart attack"){
        col <-  11
    }
    else if(outcome == "heart failure"){
        col <- 17
    }
    else if(outcome == "pneumonia"){
        col <- 23
    }
    data[ ,col] <- as.numeric(data[ ,col])
    sort_names <- data[order(data[ ,col],data[,2]), ]
    sort_names <- sort_names[(!is.na(sort_names[ ,col])),]
    if(num == "best"){
        num <- 1
    }            
    else if (num == "worst"){
        num <- nrow(sort_names)
    }      
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death 
    sort_names[num,2]
    
}




rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    if(outcome == "heart attack"){
        col <-  11
    }
    else if(outcome == "heart failure"){
        col <- 17
    }
    else if(outcome == "pneumonia"){
        col <- 23
    }
    data[ ,col] <- as.numeric(data[ ,col])
    
    data<-data[!is.na(data[,col]),]
    
    states <-split(data, data$State)
    ans<-lapply(states,function(x,num){
        
        sort_names <- x[order(x[ ,col],x[,2]), ]
        if(num == "best"){
            num <- 1
        }            
        else if (num == "worst"){
            num <- nrow(sort_names)
        }      
        sort_names[num,2]
        
    },num)
    data.frame(Hospital=unlist(ans),State=names(ans))
}
