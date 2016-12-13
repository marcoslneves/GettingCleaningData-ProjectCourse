##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Marcos Neves

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##
## Some considerations after analysing the datasets
##
# The data values for Activity come from Y_train.txt and Y_test.txt
# The data values for Subject come from subject_train.txt and subject_test.txt
# The data values for Features come from  from X_train.txt and X_test.txt
# The data names for Features come from features.txt
# The data levels for Activity come from activity_labels.txt


##########################################################################################################
library(dplyr)


#Download the zip 
if(!file.exists("./data")){
    dir.create("./data")
}

zip_path <- "./data/UCI_HAR_Dataset.zip"
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists(zip_path)){
    download.file(file_url, destfile=zip_path, method="curl")
} 

if (!file.exists("UCI HAR Dataset")) { 
    ## unzip the file
    unzip(zipfile=zip_path, exdir="./data")
}


## get the data path and the list of files
data_path = "data/UCI HAR Dataset"
files<-list.files(data_path, recursive=TRUE)
# files


## Read Features names
data_features_names <- tbl_df(read.table(file = "data/UCI HAR Dataset/features.txt"))

##Read data values for Features 
data_features_train <- tbl_df(read.table(file = "data/UCI HAR Dataset/train/X_train.txt"))
data_features_test <- tbl_df(read.table(file = "data/UCI HAR Dataset/test/X_test.txt"))

##Read data values for Activity values and labels
data_activity_train <- tbl_df(read.table(file = "data/UCI HAR Dataset/train/Y_train.txt"))
data_activity_test <- tbl_df(read.table(file = "data/UCI HAR Dataset/test/Y_test.txt"))
data_activity_labels <- tbl_df(read.table(file = "data/UCI HAR Dataset/activity_labels.txt"))

##Read data values for Subject Identifier ( Who)
data_subject_train <- tbl_df(read.table(file = "data/UCI HAR Dataset/train/subject_train.txt"))
data_subject_test <- tbl_df(read.table(file = "data/UCI HAR Dataset/test/subject_test.txt"))

##### 2 - Merges train and test datasets
data_features <- rbind(data_features_train, data_features_test)
## Assign a vector of names from data_features_names to variable names(colnames) in data_features to have more descriptive names for each Feature
names(data_features) = data_features_names$V2

## Merging the Activity train and test datasets and merges the result dataset with the Activity Labels dataset to get more descriptive labels  
data_activity <- rbind(data_activity_train, data_activity_test)
data_activity <- merge(data_activity, data_activity_labels, by = "V1")
data_activity$V1 <- NULL
names(data_activity) = "activity"

## Merging the train and test Subject identifier data
data_subject <- rbind(data_subject_train, data_subject_test)
names(data_subject) = "subject"


##### 3 - Extract only the mean and standard deviation measurements from data_features
data_features_filtered <- data_features[, grepl("mean\\(\\)|std\\(\\)", names(data_features))]


## Merges the subject, activity and features datasets
data_all_clean <- tbl_df(cbind(data_subject, data_activity, data_features_filtered))

## Descriptive variable names
names_vec <- names(data_all_clean)
names_vec <- gsub("\\(", "", names_vec)
names_vec <- gsub("\\)", "", names_vec)
names_vec <- gsub("\\-", "_", names_vec)
names_vec <- gsub(",", "_", names_vec)
names_vec <- gsub("^t", "time", names_vec)
names_vec <- gsub("^f", "frequency", names_vec)
names_vec <- gsub("Acc", "Acceleration", names_vec)
names_vec <- gsub("Gyro", "Gyroscope", names_vec)
names_vec <- gsub("Mag", "Magnitude", names_vec)
names_vec <- gsub("BodyBody", "Body", names_vec)
names_vec <- gsub("mean", "Mean", names_vec)
names_vec <- gsub("std", "StandardDeviation", names_vec)
names(data_all_clean) <- names_vec


###########
# Create a second, independent tidy data set with the average of each variable for each activity and each subject
data_tidy <- aggregate(. ~ subject + activity, data_all_clean, mean)
data_tidy <- arrange(data_tidy, subject, activity)

## Write to txt file
write.table(data_tidy, './data/UCI_HAR_tidy_data.txt', row.names=TRUE)

## Check the final data frame
dim(data_all_clean)
head(data_all_clean)

print("PROCESS COMPLETE")
