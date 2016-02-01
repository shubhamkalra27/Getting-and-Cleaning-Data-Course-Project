library(tidyr)
library(dplyr)


##################################################################
########### merges the training and test sets to create one data set.
###################################################################
# training data
train_x <- read.table("UCI HAR Dataset//train/X_train.txt",  quote="\"", stringsAsFactors=FALSE)
train_sub <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names=c("subject"))
train_y <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity"))
train_data <- cbind(train_x, train_sub, train_y)

# test data
test_x <- read.table("UCI HAR Dataset//test/X_test.txt",  quote="\"", stringsAsFactors=FALSE)
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_y <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity"))
test_data <- cbind(test_x, test_sub, test_y)

HARCombinedData <- rbind(train_data, test_data)

##################################################################
########### Extracts only the measurements on the mean and standard deviation for each measurement.
###################################################################

feature_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "name"))

features <- c(as.vector(feature_list[, "name"]), "subject", "activity")

# filter only features that has mean or std in the name

featureindex <- grepl("-mean[^F]()|-std()|subject|activity", features)

HARCombinedData_with_mean_std <- HARCombinedData[,featureindex]


##################################################################
########### Uses descriptive activity names to name the activities in the data set
###################################################################



activity_labels <- read.table("UCI HAR Dataset//activity_labels.txt", quote="\"", stringsAsFactors=FALSE)

# renaming column name to perform inner join
colnames(activity_labels)[which(names(activity_labels) == "V1")] <- "activity"

colnames(activity_labels)[which(names(activity_labels) == "V2")] <- "activity2"

#Inner join
HARCombinedData_with_mean_std_activity <- merge(HARCombinedData_with_mean_std, activity_labels, by = "activity")
HARCombinedData_with_mean_std_activity$activity <- NULL

##################################################################
########### Appropriately labels the data set with descriptive variable names.
###################################################################

#removing unrequired featues
features <- features[featureindex]

# removing brackets and Dashes
features_1 <- gsub("\\(\\)", "", features)

# replacing dashes with spaces
features_1 <- gsub("-", " ", features_1)

names(HARCombinedData_with_mean_std_activity) <- features_1

##################################################################
########### From the data set in step 4, creates a second, 
########### independent tidy data set with the average of each variable for each activity and each subject.
###################################################################


tidy <- aggregate(. ~ activity + subject, data = HARCombinedData_with_mean_std_activity , FUN = mean)

write.table(tidy, "tidy.txt", row.names=FALSE)
