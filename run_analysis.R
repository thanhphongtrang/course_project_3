library(dplyr)
#Read data into frames
features=read.table("./UCI HAR Dataset/features.txt",col.names = c("n","functions"))
activities=read.table("./UCI HAR Dataset/activity_labels.txt",col.names = c("code","activities"))
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt",col.names = features$functions)
x_train <-read.table("./UCI HAR Dataset/train/X_train.txt",col.names = features$functions)
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt",col.names = "code")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt",col.names = "code")

#Merges the training and the test sets to create one data set.
subject=rbind(subject_test,subject_train)
X=rbind(x_test,x_train)
 Y=rbind(y_test,y_train)
rawdata=cbind(subject,X,Y)

#Extracts only the measurements on the mean and standard deviation for each measurement.
OnlyMeanAndStd <- rawdata %>% select(subject,code,contains("mean"),contains("std"))

#Uses descriptive activity names to name the activities in the data set.
OnlyMeanAndStd$code = activities[OnlyMeanAndStd$code,2]

#Appropriately labels the data set with descriptive variable names. 
names(OnlyMeanAndStd)[2] = "activity"
names(OnlyMeanAndStd)<-gsub("Acc", "Accelerometer", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("Gyro", "Gyroscope", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("BodyBody", "Body", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("Mag", "Magnitude", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("^t", "Time", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("^f", "Frequency", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("tBody", "TimeBody", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("-mean()", "Mean", names(OnlyMeanAndStd), ignore.case = TRUE)
names(OnlyMeanAndStd)<-gsub("-std()", "STD", names(OnlyMeanAndStd), ignore.case = TRUE)
names(OnlyMeanAndStd)<-gsub("-freq()", "Frequency", names(OnlyMeanAndStd), ignore.case = TRUE)
names(OnlyMeanAndStd)<-gsub("angle", "Angle", names(OnlyMeanAndStd))
names(OnlyMeanAndStd)<-gsub("gravity", "Gravity", names(OnlyMeanAndStd))

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
SubmitData = group_by(OnlyMeanAndStd,subject,activity) %>% summarise_all(funs(mean))

#export the clean data
write.table(SubmitData,"cleandata.txt",row.names = FALSE)