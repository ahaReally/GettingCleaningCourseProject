########################################################################################################
#	Getting and Cleaning Data											                                                       #
#	Course Project                                                                                       #
#	Human Activity Recognition Using Smartphones Dataset                                                 #
#                                                                                                      #
# You should create one R script called run_analysis.R that does the following.                        #
# 1.	Merges the training and the test sets to create one data set.                                    #
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.          #
# 3.	Uses descriptive activity names to name the activities in the data set                           #
# 4.	Appropriately labels the data set with descriptive variable names.                               #
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of     #
# eachvariable for each activity and each subject.                                                     #
########################################################################################################

setwd("C:/Users/owner/Documents/Data Science/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

##install.packages("dplyr")
##install.packages("plyr")
##install.packages("Hmisc")
##install.packages("data.table")
##install.packages("reshape2")
##install.packages("rmarkdown")

library(curl)
library(data.table)
library(dplyr)
library(plyr)
library(Hmisc)
library(reshape2)
library(rmarkdown)

###########################################################################
######1.Merges the training and the test sets to create one data set.######

###Read in data###

#Features
features<-read.table('./features.txt',header=FALSE)
head(features,20)
dim(features)#561   2
features[,2]

#Train
sub_Train<- read.table('./train/subject_train.txt',header=FALSE); 
x_Train<- read.table('./train/x_train.txt',header=FALSE);
y_Train<-read.table('./train/y_train.txt',header=FALSE); 

names(x_Train)<-features[,2]
head(x_Train)
names(y_Train)<-"activity"
names(sub_Train)<-"subject"

train <- cbind(sub_Train, y_Train, x_Train)
head(train,2)
names(train)
dim(train)#7352  563

#Test
sub_Test<- read.table('./test/subject_test.txt',header=FALSE); 
x_Test<- read.table('./test/x_test.txt',header=FALSE); 
y_Test<- read.table('./test/y_test.txt',header=FALSE); 

names(x_Test)<-features[,2]
head(x_Test)
names(y_Test)<-"activity"
names(sub_Test)<-"subject"

test <- cbind(sub_Test, y_Test, x_Test)
head(test,2)
dim(test)#2947  563
names(test)

###merge train and test###
both <- rbind(train, test)
dim(both)#10299   563
vars<-names(both)



###########################################################################
###### 2.	Extracts only the measurements on the mean and standard  ######
######      deviation for each measurement.                   	   ######


### seperate the wanted vars such as activity, subject, mean and std 
tokeep <- grepl("activity|subject|.*mean()|.*std.*", vars)
table(tokeep)

### keep the vars of interest
keep<- both[tokeep==T]
dim(keep)
names(keep)


###########################################################################
###### 3.	Uses descriptive activity names to name the activities   ######

actLabels<- read.table('./activity_labels.txt',header=FALSE);
head(actLabels)

keep$activity<-gsub("1", "WALKING", keep$activity)
keep$activity<-gsub("2", "WALKING_UPSTAIRS", keep$activity)
keep$activity<-gsub("3", "WALKING_DOWNSTAIRS", keep$activity)
keep$activity<-gsub("4", "SITTING", keep$activity)
keep$activity<-gsub("5", "STANDING", keep$activity)
keep$activity<-gsub("6", "LAYING", keep$activity)
table(keep$activity)


###########################################################################
###4.	Appropriately labels the data set with descriptive variable names.###

names(keep)

names(keep)<-gsub("[Mm]ean()", "mean", names(keep))
names(keep)<-gsub("std()", "stdev", names(keep))
names(keep)<-gsub("^t", "time_", names(keep))
names(keep)<-gsub("^f", "frequency_", names(keep))
names(keep)<-gsub("^angle", "angle_", names(keep))
names(keep)<-gsub("Acc", "Accelerometer", names(keep))
names(keep)<-gsub("Gyro", "Gyroscope", names(keep))
names(keep)<-gsub("Mag", "Magnitude", names(keep))

names(keep)
dim(keep)
length(names(keep))



###########################################################################
###   5.From the data set in step 4, creates a second, independent      ###
###     tidy data data set with the average of each variable for        ###
###     each activity and each subject.                                 ###

melted <- melt(keep, id = c("subject", "activity")) 
dim(melted)
head(melted)
 
avg_keep <- dcast(melted, subject + activity ~ variable, mean)
dim(avg_keep)
head(avg_keep)
names(avg_keep)

##could use ddply from dply as an alternative

write.table(avg_keep, "tidy.txt", row.name=FALSE)






