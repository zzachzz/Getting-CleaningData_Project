library(dplyr)
library(data.table)

#download file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#file destination
destfile <- "C:/Users/Zach/Desktop/URI HAR Dataset.zip"

#download zip file
download.file(url, destfile)

#unzip
unzip("C:/Users/Zach/Desktop/URI HAR Dataset.zip",list=TRUE)

#filepaths assigned to variables
trainFile <- "C:/Users/Zach/Desktop/UCI HAR Dataset/train/X_train.txt"
testFile <- "C:/Users/Zach/Desktop/UCI HAR Dataset/test/X_test.txt"
testLabel <- "C:/Users/Zach/Desktop/UCI HAR Dataset/test/y_test.txt"
trainLabel <- "C:/Users/Zach/Desktop/UCI HAR Dataset/train/y_train.txt"
activLabel <- "C:/Users/Zach/Desktop/UCI HAR Dataset/activity_labels.txt"
features <- "C:/Users/Zach/Desktop/UCI HAR Dataset/features.txt"
subjectTrain <- "C:/Users/Zach/Desktop/UCI HAR Dataset/train/subject_train.txt"
subjectTest <- "C:/Users/Zach/Desktop/UCI HAR Dataset/test/subject_test.txt"

#read in datasets from each text file
train <- read.table(trainFile) #train values
test <- read.table(testFile) #test values
testLabelTable <- read.table(testLabel) #corresponding factor values
trainLabelTable <- read.table(trainLabel) #corresponding factor values
activLabelTable <- read.table(activLabel) #factor representations
featuresTable <- read.table(features) #column headers
subjectTrainTable <- read.table(subjectTrain) #subject samples
subjectTestTable <- read.table(subjectTest) #subject samples


#combine train and test subjects
allSubjects <- rbind(subjectTrainTable, subjectTestTable)


#train and test datasets combined
trainTest <- rbind(train,test)
head(trainTest)

#add activity value column to dataset
allLabels <- rbind(trainLabelTable,testLabelTable) #combine label tables
trainTestWLabels <- cbind(trainTest,allLabels) #combine labels with data
head(trainTestWLabels)

##transpose features measurement descriptions
transp <- t(featuresTable$V2)
transp <- data.frame(transp)

##self-check if column names are assigned properly with the following 2 instructions 
colnames(transp) <- transp[,1:561]
names(transp)

#assign accurate column names for dataset
colnames(trainTestWLabels) <- transp[,1:561]
names(trainTestWLabels)[is.na(names(trainTestWLabels))] <- "activity"
head(trainTestWLabels)
names(trainTestWLabels)


#replace activity numbers with verb activities
trainTestWLabels$activity <- gsub(pattern="^[1-6]",trainTestWLabels$activity,activLabelTable[trainTestWLabels$activity,2])
head(trainTestWLabels$activity)
colnames(trainTestWLabels)

#add subject column to dataset
trainTestWLabels <- cbind(trainTestWLabels,allSubjects)
colnames(trainTestWLabels)[(names(trainTestWLabels)) == "V1"] <- "subject"
head(trainTestWLabels)

#select columns measuring mean (meanFreq() will need to be removed from dataset)
meanCols <- grep(pattern="-mean()",colnames(trainTestWLabels))
meanFreq <- grep(pattern="-meanFreq()",colnames(trainTestWLabels))

#remove columns with meanFreq
meanCols <- meanCols[!(meanCols %in% meanFreq)]

#select columns measuring std
stdCols <- grep(pattern="-std()",colnames(trainTestWLabels))


#move mean and std values together into new dataframe
meanStdVals <- cbind(trainTestWLabels[meanCols],trainTestWLabels[stdCols])
head(meanStdVals)
names(meanStdVals)


#create new dataset to compute avg of each var for each activity and each subject
meanStdVals <- cbind(meanStdVals, trainTestWLabels$activity)
meanStdVals <- cbind(meanStdVals, trainTestWLabels$subject)
names(meanStdVals)


#compute the means across dataset
tidyMeansStd <- meanStdVals %>%
  group_by(trainTestWLabels$activity,trainTestWLabels$subject) %>%
  summarize_all(mean)

head(tidyMeansStd)

#write out tidy dataset
write.table(tidyMeansStd,"C:/Users/Zach/Desktop/neat.txt",row.names = FALSE)





