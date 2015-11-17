---
title: "CodeBook.md"
author: "A. Austin"
date: "November 16, 2015"
output: html_document
---
# Code Book

## Raw Data
The raw data used was described by the initial researchers as following:  

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The full description can be found here:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

And the raw data can be downloaded here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Processing and Transformations
Once the contents of the downloaded zip file are extracted into their own directories, the run_analysis.R script uses 'features.txt' to import column names and 'activity_labels.txt' to resolve activity name factor values (e.g. WALKING).  The script then reads in subjectIDs, activityIDs and data rows for both the train and test sets, using the column header names imported above and using the activity labels resolved above as well.  The datasets are combined before a the subject, activity and columns containing means and standard deviations are selected into a smaller dataset.  The smaller dataset is then operated on to produce our tidy dataset containing averages of averages and standard deviations, grouped by activity and subject.

## Tidy Variables Selected
The tidy dataset contains two 'grouping' columns of activity (e.g. WALKING) and subjectID followed by the average value contained in the column of the same name in the original dataset, where the column is either representing a mean or standard deviation of a measured value.  

### Columns Selected
 [1] "activity"                       
 [2] "subjectID"                      
 [3] "tBodyAcc-mean()-X"              
 [4] "tBodyAcc-mean()-Y"              
 [5] "tBodyAcc-mean()-Z"              
 [6] "tGravityAcc-mean()-X"           
 [7] "tGravityAcc-mean()-Y"           
 [8] "tGravityAcc-mean()-Z"           
 [9] "tBodyAccJerk-mean()-X"          
[10] "tBodyAccJerk-mean()-Y"          
[11] "tBodyAccJerk-mean()-Z"          
[12] "tBodyGyro-mean()-X"             
[13] "tBodyGyro-mean()-Y"             
[14] "tBodyGyro-mean()-Z"             
[15] "tBodyGyroJerk-mean()-X"         
[16] "tBodyGyroJerk-mean()-Y"         
[17] "tBodyGyroJerk-mean()-Z"         
[18] "tBodyAccMag-mean()"             
[19] "tGravityAccMag-mean()"          
[20] "tBodyAccJerkMag-mean()"         
[21] "tBodyGyroMag-mean()"            
[22] "tBodyGyroJerkMag-mean()"        
[23] "fBodyAcc-mean()-X"              
[24] "fBodyAcc-mean()-Y"              
[25] "fBodyAcc-mean()-Z"              
[26] "fBodyAcc-meanFreq()-X"          
[27] "fBodyAcc-meanFreq()-Y"          
[28] "fBodyAcc-meanFreq()-Z"          
[29] "fBodyAccJerk-mean()-X"          
[30] "fBodyAccJerk-mean()-Y"          
[31] "fBodyAccJerk-mean()-Z"          
[32] "fBodyAccJerk-meanFreq()-X"      
[33] "fBodyAccJerk-meanFreq()-Y"      
[34] "fBodyAccJerk-meanFreq()-Z"      
[35] "fBodyGyro-mean()-X"             
[36] "fBodyGyro-mean()-Y"             
[37] "fBodyGyro-mean()-Z"             
[38] "fBodyGyro-meanFreq()-X"         
[39] "fBodyGyro-meanFreq()-Y"         
[40] "fBodyGyro-meanFreq()-Z"         
[41] "fBodyAccMag-mean()"             
[42] "fBodyAccMag-meanFreq()"         
[43] "fBodyBodyAccJerkMag-mean()"     
[44] "fBodyBodyAccJerkMag-meanFreq()" 
[45] "fBodyBodyGyroMag-mean()"        
[46] "fBodyBodyGyroMag-meanFreq()"    
[47] "fBodyBodyGyroJerkMag-mean()"    
[48] "fBodyBodyGyroJerkMag-meanFreq()"
[49] "tBodyAcc-std()-X"               
[50] "tBodyAcc-std()-Y"               
[51] "tBodyAcc-std()-Z"               
[52] "tGravityAcc-std()-X"            
[53] "tGravityAcc-std()-Y"            
[54] "tGravityAcc-std()-Z"            
[55] "tBodyAccJerk-std()-X"           
[56] "tBodyAccJerk-std()-Y"           
[57] "tBodyAccJerk-std()-Z"           
[58] "tBodyGyro-std()-X"              
[59] "tBodyGyro-std()-Y"              
[60] "tBodyGyro-std()-Z"              
[61] "tBodyGyroJerk-std()-X"          
[62] "tBodyGyroJerk-std()-Y"          
[63] "tBodyGyroJerk-std()-Z"          
[64] "tBodyAccMag-std()"              
[65] "tGravityAccMag-std()"           
[66] "tBodyAccJerkMag-std()"          
[67] "tBodyGyroMag-std()"             
[68] "tBodyGyroJerkMag-std()"         
[69] "fBodyAcc-std()-X"               
[70] "fBodyAcc-std()-Y"               
[71] "fBodyAcc-std()-Z"               
[72] "fBodyAccJerk-std()-X"           
[73] "fBodyAccJerk-std()-Y"           
[74] "fBodyAccJerk-std()-Z"           
[75] "fBodyGyro-std()-X"              
[76] "fBodyGyro-std()-Y"              
[77] "fBodyGyro-std()-Z"              
[78] "fBodyAccMag-std()"              
[79] "fBodyBodyAccJerkMag-std()"      
[80] "fBodyBodyGyroMag-std()"         
[81] "fBodyBodyGyroJerkMag-std()"     
