## R Programming Assignment - Data Science Coursera
## Github @ericajwashington
## April 30, 2017


## Part 1. Plot the 30-day mortality rates for heart attack

## Read the outcome data into R via the read.csv function to look at the first few rows
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

## You can see how many columns are in the dataset by typing ncol(outcome)
ncol(outcome)  ##Answer=46

## You can also see the names of each column by typing names(outcome)
names(outcome) ## Running this will have the names appear in the console
               ## You'll also see info in the Environment: 4706 obs and 46 variables

## Make a simple histogram of the 30-day death rates from heart attack
## (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])

## You may get a warning about NAs being introduced; that is okay 
## (I did get that)
hist(outcome[, 11])

## We originally read the data in as character by specifying colClasses = "character"
## so we need to coerce the column to be numeric. You may get a warning about NAs
## being introduced, but that is okay.
colClasses = "character"


##Part 3. Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best"){
  
  ## Read the file outcome
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  if(!state %in% outcome_data$State){
    stop("invalid state")
  } 
  else{
    if (outcome == "heart attack"){
      colnum <- 11
    }
    else if (outcome == "heart failure"){
      colnum <- 17
    }
    else if (outcome == "pneumonia"){
      colnum <- 23
    }
    else{
      stop("invalid outcome")
    }
    
    #Now look at outcome columns...
    outcome_data1 <- subset(outcome_data, State == state & outcome_data[,colnum]!= "Not Available")
    
    if (num == "best"){
      rank <- 1
    }
    else if (num == "worst"){
      rank <- length(outcome_data1[,colnum])
    }
    else if (num > nrow(outcome_data1)){
      return(NA)
    }
    else{
      rank <- num
    }
    hos_num <- sort(as.numeric(outcome_data1[,colnum]))
    hos_name <- subset(outcome_data1, as.numeric(outcome_data1[,colnum]) == hos_num[rank])

    return(hos_name[1,2]) ##If 2 value exist, just return the first
  }
}

## sample inputs:
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)