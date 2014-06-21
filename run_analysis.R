# Download and unzip data from website to working directory on local hard drive
# Set working directory to location of unzipped UCI HAR dataset
setwd('C:/Users/Sheila/Desktop/Coursera/UCI_HAR_Dataset/')

# Read train datasets into R
features = read.table('./features.txt',header=F)
activityType = read.table('./activity_labels.txt',header=F)
subjectTrain = read.table('./train/subject_train.txt',header=F)
xTrain = read.table('./train/x_train.txt',header=F)
yTrain = read.table('./train/y_train.txt',header=F)

# Assign column names to train datasets
colnames(activityType) = c("activityID","activityType")
colnames(subjectTrain) = "subjectID"
colnames(xTrain) = features[,2] 
colnames(yTrain) = "activityID"

# Create the final dataset for train by merging yTrain, subjectTrain, and xTrain
trainData = cbind(yTrain, subjectTrain, xTrain)

# Read test datasets into R
subjectTest = read.table('./test/subject_test.txt',header=F)
xTest = read.table('./test/x_test.txt',header=F)
yTest = read.table('./test/y_test.txt',header=F)

# Assign column names to test data
colnames(subjectTest) = "subjectID"
colnames(xTest) = features[,2] 
colnames(yTest) = "activityID"


# Create the final dataset for test by merging the xTest, yTest and subjectTest data
testData = cbind(yTest, subjectTest, xTest)


# 1. Merge training and test datasets to create one dataset
finalData = rbind(trainData,testData)
 

# 2. Extract only mean and standard deviation for each measurement. 

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean, std dev columns
colNames  = colnames(finalData)

# Create a vector that contains values for the ID, mean, std dev columns
IDvector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the vector in order to keep only the desired data
finalData = finalData[IDvector==T]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the activityType table to include descriptive activity names
finalData = merge(finalData, activityType, by='activityID', all.x=T)

# Update the colNames vector to include the new column names after merge
colNames = colnames(finalData) 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalData2 without the activityType column
finalData2 = finalData[,names(finalData) != 'activityType']

# Summarize the finalData2 table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalData2[,names(finalData2) != c('activityID','subjectID')],by=list(activityID=finalData2$activityID, subjectID = finalData2$subjectID),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityID',all.x=T)

# Export the tidyData set to text file on hard drive
write.table(tidyData, './tidyData.txt', row.names=T, sep='\t')