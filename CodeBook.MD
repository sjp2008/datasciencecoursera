## Getting and Cleaning Data Course Project
June 22, 2014
Sheila Poteat

### Description
This code book file contains additional information about the variables, data and transformations used in the R script for the course project.

### Data Source
This project serves to demonstrate the downloading and cleaning of a tidy data set that can be used for subsequent analysis. A full description of the data used in this project can be found at:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The source data for this project can be found here -- (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments were carried out with a group of 30 volunteers, age 19-48. Each person performed six activities (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING) while wearing a smartphone (Samsung Galaxy S II) on their waist. Using its embedded accelerometer and gyroscope, researchers captured acceleration and velocity at a constant rate of 50Hz.

The experiments were video-recorded and the data was labeled manually. The obtained dataset has been randomly split into two sets where 70% of the volunteers was selected for generating the "training" data and 30% the "test" data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width windows of 2.56 sec and 50% overlap (128 readings/window).

The sensor acceleration signal, which has gravitational and body motion components, was separated using a low-pass filter into body acceleration and gravity. 

The gravitational force is assumed to have only low frequency components so a filter with a 0.3 Hz cutoff frequency was used.

### Attributes recorded for each subject in the dataset 
- An identifier for the experiment subject
- Total acceleration and estimated body acceleration 
- Velocity from the gyroscope 
- A vector with time, frequency and activity label


### Section 1. Merge the training and the test sets to create one data set.
After setting the working directory for the files, read into R the following text files the data is located in:
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

Assign column names and merge to create one data set.

## Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 
Create a vector for the ID, mean and std dev columns.
Subset this data to keep only the necessary columns.

## Section 3. Use descriptive activity names to name the activities in the data set.
Merge data subset with the activityType table to include the new descriptive activity names.

## Section 4. Appropriately label the data set with descriptive activity names.
Can use gsub function for pattern replacement to clean up the data labels.

changed "-std$" to "StdDev"
changed "-mean" to "Mean"
changed "^(t)" to "time"
changed "^(f)" to "freq"
changed "[Gg]ravity)" to "Gravity"
changed "([Bb]ody[Bb]ody|[Bb]ody)" to "Body"
changed "[Gg]yro"to "Gyro"
changed "AccMag" to "AccMagnitude"
changed "([Bb]odyaccjerkmag)"to "BodyAccJerkMagnitude"
changed "JerkMag" to JerkMagnitude"
changed "GyroMag" to "GyroMagnitude"

## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
The result is a rather wide data set with numerous activity columns for each subject ID.  Because of the width, the best way to view the data is to write it to an Excel spreadsheet but you can also write it to a text file and reformat it.  See R code script file (run_analysis.R) for specific commands and syntax used for this project.
