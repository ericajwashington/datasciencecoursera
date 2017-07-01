## Data Science Coursea - Course 3 Week 4 Project

## You will be required to submit: 
## 1) A tidy data set as described below, 
## 2) A link to a Github repository with your script for performing the analysis, and 
## 3) A code book that describes the variables, the data, and any transformations or work 
##    that you performed to clean up the data called CodeBook.md. 
##    You should also include a README.md in the repo with your scripts. 
##    This repo explains how all of the scripts work and how they are connected.

## ASSIGNMENT: You should create one R script called run_analysis.R that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
##    of each variable for each activity and each subject.

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

## Load: activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

## Load: data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

## Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)

## Load and process X_test & y_test data.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

names(X_test) = features

## Extract only the measurements on the mean and standard deviation for each measurement.
X_test = X_test[,extract_features]

## Load activity labels
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

## Bind data
test_data <- cbind(as.data.table(subject_test), y_test, X_test)

## Load and process X_train & y_train data.
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(X_train) = features

## Extract only the measurements on the mean and standard deviation for each measurement.
X_train = X_train[,extract_features]

## Load activity data
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

## Bind data
train_data <- cbind(as.data.table(subject_train), y_train, X_train)

## Merge test and train data
data = rbind(test_data, train_data)

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
melt_data      = melt(data, id = id_labels, measure.vars = data_labels)

## Apply mean function to dataset using dcast function with averages 
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)

