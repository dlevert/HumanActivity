run_analysis <- function(){
  
# READ AND COMBINE "test" FILES====================================
  library(dplyr)
  library(utils)
  features <- read.csv("./UCI HAR Dataset/features.txt", sep = " ",
                       header = FALSE)
  features <- features[,2]
  xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "", dec = ".",
                      na.strings = "NA")
  ytest <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep = " ",
                    header = FALSE)
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  data <- data.frame(c("test", ytest, subject_test, xtest))
  names(data) <- c("test", "activity", "subject", features)
  rm(xtest)
  rm(ytest)
  rm(subject_test)
  
# READ AND COMBINE "train" FILES===================================
  xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "",
                       dec = ".", na.strings = "NA")
  ytrain <- read.csv("./UCI HAR Dataset/train/y_train.txt", sep = " ",
                     header = FALSE)
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  data2 <- data.frame(c("train", ytrain, subject_train, xtrain))
  names(data2) <- c("test", "activity", "subject", features)
  rm(subject_train)
  rm(xtrain)
  rm(ytrain)
  rm(features)
  
# COMBINE test AND train TABLES=====================================
  data <- rbind(data, data2)
  rm(data2)
  
# SELECT RELEVANT COLUMNS AND RENAME===============================
  data <- select(data, 1:3, contains("mean"), contains("std"))
  data <- select(data, -contains("meanFreq"))
  data <- select(data, -contains("angle"))
  names(data) <- gsub("\\()", "", names(data))
  names(data) <- gsub("^t", "Time ", names(data))
  names(data) <- gsub("^f", "FFT ", names(data))
  names(data) <- gsub("^Time est", "test", names(data))
  
#POPULATE ACTIITY==================================================
  activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
  data$activity <- as.factor(activity[data$activity,2])
  rm(activity)
  
#CREATE SUMMARIZED DATA SET========================================
  data$subject <- as.factor(data$subject)
  data$test <- as.factor(data$test)
  data <- group_by(data,activity,subject)
  suppressWarnings(sumdat <- summarise_all(data, funs(mean)))
  sumdat
}