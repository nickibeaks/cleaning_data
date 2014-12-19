if(!file.exists("dataset.zip")){
  dir.create("dataset.zip")
}
library(downloader)
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download(url, "dataset.zip", mode="wb", method = "curl")
unzip("dataset.zip")
unlink(url)

# reading data into R variables
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt") 
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# combining training and test data
data <- rbind(x_train,x_test) 
activity <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

#labeling columns using features labels
columns <- features[,2]
colnames(data) <- columns

#selecting mean and std data
library(dplyr)
unduplicate <- data[,!duplicated(names(data))]
extract <- select(unduplicate,contains('mean()'), contains('std()'))

# labelling the activity column
activity <- activity[,1]
activity <- replace(activity, activity==1, "WALKING")
activity <- replace(activity, activity==2, "WALKING_UPSTAIRS")
activity <- replace(activity, activity==3, "WALKING_DOWNSTAIRS")
activity <- replace(activity, activity==4, "SITTING")
activity <- replace(activity, activity==5, "STANDING")
activity <- replace(activity, activity==6, "LAYING")
activity <- as.data.frame(activity)

#binding the subject and activity columns together to organize the data set
activity_subject <- cbind(subject, activity)

# first data set that is the result of steps 1-4
data1 <- cbind(activity_subject, extract)
colnames(data1)[1] <- "subject"

#my mean function removes the first two columns of the data set, which are factors, and then does colmean
library(plyr)

myfun <- function (x) {
  data <- x[,c(-1,-2)]
  colMeans(data, na.rm=TRUE)
}

#use ddply to split the data by activity and subject then apply my function
tidy_data<- ddply(data1, .(activity, subject), myfun)
