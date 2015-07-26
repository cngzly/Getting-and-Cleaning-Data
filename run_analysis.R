library(reshape2)

# 1. Merges the training and the test sets to create one data set.

# Read data
features <- read.table("UCI HAR Dataset/features.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names=c("subject_id"))
X_train <- read.table("UCI HAR Dataset/train/X_train.txt",col.names=features[,2])
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt",col.names=c("activity_id"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names=c("subject_id"))
X_test <- read.table("UCI HAR Dataset/test/X_test.txt",col.names=features[,2])
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt",col.names=c("activity_id"))

# Merge data
training_set <- cbind(subject_train,Y_train, X_train)
test_set <- cbind(subject_test,Y_test, X_test)
combin_data <- rbind(training_set,test_set)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Look for mean and standard deviation and extract
features_mean_dev <- features[grepl("mean\\(\\)", features$V2) | grepl("std\\(\\)", features$V2), ]
dataset_mean_dev <- combin_data[,c(c(1,2),features_mean_dev$V1+2)]


# 3. Uses descriptive activity names to name the activities in the data set

# read activity lables from file, merge lable and delet id to rename the activities
activity_labels <- read.table("activity_labels.txt",col.names=c("activity_id","activity_labels"))
dataset_mean_dev <- subset(merge(activity_labels,dataset_mean_dev),select = -c(activity_id))


# 4. Appropriately labels the data set with descriptive variable names.

dataset_labels <- colnames(dataset_mean_dev)
dataset_labels <- gsub("\\.std","_Std", dataset_labels)
dataset_labels <- gsub("\\.mean","_Mean", dataset_labels)
dataset_labels <- gsub("\\...","_", dataset_labels)
dataset_labels <- gsub("\\..","", dataset_labels)
colnames(dataset_mean_dev) <- dataset_labels

# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

meltdata <-  melt(dataset_mean_dev, id.var = c("subject_id", "activity_labels"))
meandata <-  dcast(meltdata , subject_id + activity_labels ~ variable, mean)
write.table(meandata, file="tidy_data.txt",row.name=FALSE)
