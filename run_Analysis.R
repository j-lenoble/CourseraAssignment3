###Coursera Module 3 Getting and Shaping Data
##Peer Review Assignment
#Created January 2, 2019 by JLN

##Notes
#This code creates a tidy dataset from Samsung Galaxy 5 Smartphone

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy 
#data set with the average of each variable for each activity and each subject.

# Load libraries
library(reshape2)
library(dplyr)
library(quanteda)

# Create file name, download and unzip data with link provided on the Coursera page
#create file Name
filename <- "getdata_projectfiles_UCI HAR dataset.zip"
#if the file name is not located in the existing directory, download the folder
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}
#if the filename exists, unzip the folder
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

rm(filename)   #cleanup

# load the activity labels and features tables into R without factor levels
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

#create list of names of stats that are mean or standard deviation, ignoring case
stats <- grep(".*mean.*|.*std.*", features[,2])
stats.names <- features[grep(".*mean.*|.*std.*", features[,2]),2]
#use expressions to modify upper and lower case and to remove symbols or garbage
stats.names = gsub('-mean', 'Mean', stats.names)
stats.names = gsub('-std', 'Std', stats.names)
stats.names <- gsub('[-()]', '', stats.names)

rm(features)   #cleanup

##2. Load the train dataset with only mean and standard deviaiton data (i.e. stats)
train <- read.table("UCI HAR Dataset/train/X_train.txt")[stats]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

rm(trainActivities, trainSubjects)   #cleanup

##2. Load the test dataset with only mean and standard deviaiton data extracted (i.e. stats)
test <- read.table("UCI HAR Dataset/test/X_test.txt")[stats]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

rm(testActivities, testSubjects, stats)  #cleanup

##1. merge the train and test datasets using the r(ow)bind function
allDat <- rbind(train, test)

rm(test, train)  #cleanup

#4. add the column names from the nice stats.names list that was prepared so that we have desciptive variable names
colnames(allDat) <- c("subject", "activity", stats.names)

rm(stats.names)   #cleanup

##3. turn activities & subjects into factors and then replavce the activity number values
#with the activity labels for descriptive activity names
allDat$activity <- factor(allDat$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allDat$subject <- as.factor(allDat$subject)

rm(activityLabels)   #cleanup

#melt the dataset for easy data extraction
allDat.melted <- melt(allDat, id = c("subject", "activity"))
#filter out only mean values into a separate table
allDat.mean <- dcast(allDat.melted, subject + activity ~ variable, mean)

rm(allDat.melted)  #cleanup

##left without only the two tidy tables for easy saving, distribution and analysis
#write the mean data to a table
write.table(allDat.mean, "mean.txt", row.names = FALSE, quote = FALSE)
