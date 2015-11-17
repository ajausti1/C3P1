## You should create one R script called run_analysis.R that does the following.
library(data.table)
library(dplyr)
## 1. Merges the training and the test sets to create one data set.

###############################
##  GLOBAL OPS
###############################

# Column names are contained in the feautres file, with each row containing the name of the col.
colNameDS <- read.csv("UCI HAR Dataset/features.txt", header = F, sep = " ")

colNames <- vector()
for (colName in colNameDS[,2]) {
  colNames <- append(colNames, colName)
}

# Activity ID to Name mapping is contained in another file
activityMap <- read.csv("UCI HAR Dataset/activity_labels.txt", header = F, sep = " ")

###############################
##  TEST-SET OPS
###############################
# Read in both test and train sets; each row has a corresponding activity type and subject ID
testSubjectIds <- read.csv("UCI HAR Dataset/test/subject_test.txt", col.names = c("subjectID"), header = F)
testActivityIds <- read.csv("UCI HAR Dataset/test/y_test.txt", header = F)

# Read in the testDS using colNames calculated above
testDS <- fread(input = "UCI HAR Dataset/test/X_test.txt", header = F, sep = " ", col.names = colNames)
# Turn the IDs into their factor representation and append it to the testDS
testDS$activity <- merge(testActivityIds, activityMap, by.x="V1")[ , 2]

# Append the subject ID to each row in the dataset
testDS$subjectID <- testSubjectIds

###############################
##  TRAIN-SET OPS
###############################
# Each row has a corresponding activity type and subject ID
trainSubjectIds <- read.csv("UCI HAR Dataset/train/subject_train.txt", col.names = c("subjectID"), header = F)
trainActivityIds <- read.csv("UCI HAR Dataset/train/y_train.txt", header = F)

# Read in the trainDS using colNames calculated above
trainDS <- fread(input = "UCI HAR Dataset/train/X_train.txt", header = F, sep = " ", col.names =colNames)

# Turn the IDs into their factor representation and append it to the trainDS
trainDS$activity <- merge(trainActivityIds, activityMap, by.x="V1")[ , 2]

# Append the subject ID to each row in the dataset
trainDS$subjectID <- trainSubjectIds

###############################
##  COMBINATION / EXTRACTION OPS
###############################
# Combine the DS' using rbind
combinedDS <- rbind(testDS, trainDS)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Grep header names to only select those dealing with the mean and standard deviation into a new
# dataset.  Also select subjectID and activity

# Make a vector of the columns we are interested in and select those columns from the combined DS
colList <-  append(c("subjectID", "activity"), 
                  append(grep("mean", colnames(combinedDS), value = TRUE),
                  grep("std", colnames(combinedDS), value = TRUE)))

# It's a dataframe, so use subset()
extractedDS <- subset(combinedDS, select = colList)

## 3. Uses descriptive activity names to name the activities in the data set
# We add descriptive activity names during our inital data load and merge above

## 4. Appropriately labels the data set with descriptive variable names.
# We use incoing data columns to set meaningful / descriptive variable names

## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.
###############################
##  TIDY DS GENERATION
###############################
tidyDS <- summarize_each(group_by(extractedDS, activity, subjectID), funs(mean))
write.table(x = tidyDS, file = "tidyDS.txt", row.names = FALSE)