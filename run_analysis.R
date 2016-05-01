library(plyr)
library(dplyr)

#read files with list of subjects
Subjecttest<-read.table("./test/subject_test.txt")
SubjectTrain<-read.table("./train/subject_train.txt")
#rename the column with "subject"
names(Subjecttest)<-"subject"
names(SubjectTrain)<-"subject"

#read files with test and training data
Xtest<-read.table("./test/X_test.txt")
XTrain<-read.table("./train/X_train.txt")
#read files with activity codes corresponding to test and training data
Ytest<-read.table("./test/Y_test.txt")
YTrain<-read.table("./train/Y_train.txt")
#read file with activity names corresponding to activity codes
ActivityLables<-read.table("activity_labels.txt")

#read file containing descriptive variable names for corresponding columns of test and training data
Features<-read.table("features.txt")
#Create list of descriptive variable names
NewColNames<-Features[,2]
#Rename column names of both test and training data
names(Xtest)<-NewColNames
names(XTrain)<-NewColNames
#Reduce test and training data to columns containing only Mean- and Standard-Deviations-measures
toMatch <- c("*mean*", "*Mean*", "*std*")
RelevantColumns<-grep(paste(toMatch,collapse="|"), NewColNames, value=TRUE)
XtestRelevant<-Xtest[,RelevantColumns]
XtrainRelevant<-XTrain[,RelevantColumns]
#Replace activity codes with activity labels
ActivityTest<-merge(Ytest, ActivityLables, by = "V1")[,2]
ActivityTrain<-merge(YTrain, ActivityLables, by = "V1")[,2]
#Bring together subjects and descriptive activity labels
dfTest<-cbind(Subjecttest,ActivityTest)
dfTrain<-cbind(SubjectTrain,ActivityTrain)
dfTest<-rename(dfTest,ActivityName=ActivityTest)
dfTrain<-rename(dfTrain,ActivityName=ActivityTrain)
#Bring together subjects, activity labels and data 
dfTest <-cbind(dfTest,XtestRelevant)
dfTrain <-cbind(dfTrain,XtrainRelevant)
#Create complete table containing both test and training data
dfTotal<-rbind(dfTest,dfTrain)

#Task 5: Calculating mean values of all measure columns
loopcolumns<-ncol(dfTotal)
columns = names(dfTotal)[1:2]
for (i in 3:loopcolumns)
  { dfCalc<-dfTotal[,c(1:2,i)]
    AllNamesDF<-names(dfCalc)
    AllNamesDF[3]<-"Value"
    names(dfCalc)<-AllNamesDF
    dfCalcMean<-ddply(dfCalc, columns, summarize, Value=mean(Value))
    AllNamesDF[3]<-names(dfTotal)[i]
    names(dfCalcMean)<-AllNamesDF
    dfTotalMean <- if (i>3) {merge(dfTotalMean,dfCalcMean,by=c("subject","ActivityName"),all=TRUE)} else {dfCalcMean}
    }
write.table(dfTotalMean,file="Task5DataTable.txt",row.name=FALSE)