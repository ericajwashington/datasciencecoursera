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


## Part 4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  
  ## Read file outcome
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string ="Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = sort(unique(data[,7]))
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## For each state, find the hospital of the given rank
  hospital<-character(0)
  
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    data.state <- data[data$State==validState[i],]
    
    # Sort data by outcome
    sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.data.state)
    
    hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)
}

## sample input
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)