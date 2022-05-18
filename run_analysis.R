#Merges the training and the test sets to create one data set
##read data
rm(list=ls())
library(dplyr)
feature<-read_table2("data/features.txt",col_names = F)
X_train <- read_table2("data/train/X_train.txt",col_names = feature$X2,)
Y_train <- read_table2("data/train/y_train.txt",col_names = F)
subject_train<-read_table2("data/train/subject_train.txt",col_names = F)
X_test <- read_table2("data/test/X_test.txt",col_names = feature$X2)
Y_test <- read_table2("data/test/y_test.txt",col_names = F)
subject_test<-read_table2("data/test/subject_test.txt",col_names = F)

###merge data
names(Y_train)<-"activity"
names(subject_train)<-"subject"
alltrain<-cbind(subject_train,Y_train,X_train)
names(Y_test)<-"activity"
names(subject_test)<-"subject"
alltest<-cbind(subject_test,Y_test,X_test)
allData<-rbind(alltrain,alltest)

#Extracts only the measurements on the mean and standard deviation for each measurement. 

allData_mean_sd<-select(allData,grep("mean|std",names(allData)))
allData_mean_sd2<-cbind(allData[,c(1,2)],allData_mean_sd)

#Uses descriptive activity names to name the activities in the data set

activity_name<-read_table2("data/activity_labels.txt",col_names = F)
names(activity_name)<-c("activity","activity_name")
allData_mean_sd_named<-merge(activity_name,allData_mean_sd2,"activity",all=T)


#Appropriately labels the data set with descriptive variable names
names(allData_mean_sd_named)<-gsub("Acc", "Accelerometer", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("Gyro", "Gyroscope", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("BodyBody", "Body", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("Mag", "Magnitude", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("^t", "Time", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("^f", "Frequency", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("tBody", "TimeBody", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("-mean", "Mean", names(allData_mean_sd_named), ignore.case = TRUE)
names(allData_mean_sd_named)<-gsub("-std", "STD", names(allData_mean_sd_named), ignore.case = TRUE)
names(allData_mean_sd_named)<-gsub("-freq", "Frequency", names(allData_mean_sd_named), ignore.case = TRUE)
names(allData_mean_sd_named)<-gsub("angle", "Angle", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("gravity", "Gravity", names(allData_mean_sd_named))
names(allData_mean_sd_named)<-gsub("[()-]", "", names(allData_mean_sd_named))

###From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
list<-list(allData_mean_sd_named$activity_name,allData_mean_sd_named$subject)
secondTidyData<-aggregate(allData_mean_sd_named[,-c(1:3)],by=list,mean)
names(secondTidyData)<-c("activity_name","subject",names(secondTidyData)[-c(1,2)])
write.table(secondTidyData,"output/secondTidyData.txt",row.names = F,sep = "\t")
