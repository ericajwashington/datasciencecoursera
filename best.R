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


##Part 2. Finding the best hospital in a state

## Write a function called best that takes two arguments: the 2-character abbreviated
## name of a state and an outcome name. The function reads the outcome-of-care-
## measures.csv file and returns a character vector with the name of the hospital
## that has the best (i.e. lowest) 30-day mortality for the specified outcome in that 
## state. The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". Hospitals
## that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the
## hospital names should be sorted in alphabetical order and the first hospital in that
## set should be chosen (i.e. if hospitals "b", "c", and "f" are tied for best, then 
## hospital "b" should be returned).

best <- function(state, outcome) {
  ## Read outcome data
  outcomes_DataFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  my_data <- as.data.frame(cbind(outcomes_DataFrame[, 2],   # hospital
                                 outcomes_DataFrame[, 7],   # state
                                 outcomes_DataFrame[, 11],  # heart attack
                                 outcomes_DataFrame[, 17],  # heart failure
                                 outcomes_DataFrame[, 23]), # pneumonia
                           stringsAsFactors = FALSE)
  colnames(my_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  states <- unique(my_data$state)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if ((state %in% states) == FALSE)
  {
    stop(print("invalid state"))
  }
  
  else if ((outcome %in% outcomes) == FALSE)
  {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  else
  {
    ## Get rows of user inputted state:
    rows <- which(my_data$state == state)
    tab <- my_data[rows,]
    
    ## Get info about inputted outcome:
    ## get outcome --> convert to numeric --> get the least value(omit NA's) --> print result
    get <- as.numeric(tab[,eval(outcome)])
    least <- min(get, na.rm=T)
    output <- tab$hospital[which(get==least)]
    result <- output[order(output)]
  }
  return(result)
}