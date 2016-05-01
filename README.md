# GettingCleaningData

The script for writing a datatable to a file containing mean values of all measure columns that represent mean and standard deviation values uses two additional libraries:

library(plyr)
library(dplyr)

The code first reads all the base data files:

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


In order to meet a requirement of tidy datasets (=using descriptive variable names) the following steps are carried out for the measure columns:

#Create list of descriptive variable names for the measure columns
	NewColNames<-Features[,2]

#Rename column names of both test and training data
	names(Xtest)<-NewColNames
	names(XTrain)<-NewColNames


In the next step, measure columns are reduced to those which represent only mean and standard-deviation measures:

#Reduce test and training data to columns containing only Mean- and Standard-Deviations-measures
	toMatch <- c("*mean*", "*Mean*", "*std*")
	RelevantColumns<-grep(paste(toMatch,collapse="|"), NewColNames, value=TRUE)
	XtestRelevant<-Xtest[,RelevantColumns]
	XtrainRelevant<-XTrain[,RelevantColumns]


In order to meet a requirement of tidy datasets (=using descriptive variable names) the following steps are carried out for the Activity column:

#Replace activity codes with activity labels
	ActivityTest<-merge(Ytest, ActivityLables, by = "V1")[,2]
	ActivityTrain<-merge(YTrain, ActivityLables, by = "V1")[,2]


In a last step to create a combined dataset of test and training data (=dfTotal), subject, activity and measure columns are bound together:

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


The workflow to create a table containing mean values over subjects and activities takes a column by column appraoch for the measure columns:

#Task 5: Calculating mean values of all measure columns

#Find out the limit of the loop variable for the column by column loop:	
	loopcolumns<-ncol(dfTotal)

#Define the columns relevant for summarisation: 
	columns = names(dfTotal)[1:2]


	for (i in 3:loopcolumns)
  		{ 
#Create an interim dataframe containing the summarisation columns and one measure column	 
	 	 dfCalc<-dfTotal[,c(1:2,i)]
    
#Rename the measure column to "Value"
	 	 AllNamesDF<-names(dfCalc)
	 	 AllNamesDF[3]<-"Value"
		 names(dfCalc)<-AllNamesDF

#Calculate the summarised mean value in the interim dataframe
    		 dfCalcMean<-ddply(dfCalc, columns, summarize, Value=mean(Value))
    
#Rename the measure column to its original name
		 AllNamesDF[3]<-names(dfTotal)[i]
    		 names(dfCalcMean)<-AllNamesDF
    
#Append the new calculated measure column to the final data frame "dfTotalMean"
		 dfTotalMean <- if (i>3) {merge(dfTotalMean,dfCalcMean,by=c("subject","ActivityName"),all=TRUE)} else {dfCalcMean}
    		}

#Write the dataframe of all summarised measures to a text file
	write.table(dfTotalMean,file="Task5DataTable.txt",row.name=FALSE)
