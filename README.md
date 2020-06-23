---
title: "README.md"
author: "Pablo Rueda"
date: "6/23/2020"
output: pdf_document
---

#### FINAL PROJECT COURSE 3:

library(dplyr)

##----------------------------------------------------------------------------------------------

## QUESTION 1:Merges the training and the test sets to create one data set.

#COL NAMES FOR RAW DATA OF TEST AND TRAIN SUBSETS:

features<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/features.txt")
t_features<-as.data.frame(t(features))
clean_features<-t_features[2,]

### TEST SUBSET: IF YOU WANT TO RUN THE SCRIPT IN YOUR PC, PLEASE CHANGE THE PATH DIRECTORY

subject_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/subject_test.txt")

x_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/X_test.txt")

y_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/y_test.txt")


### BINDING ROWS AND COLUMNS FOR THE TEST DATAFRAME:
test_bind<-cbind(subject_test,y_test)
test_named<-setNames(test_bind,c("Subject ID", "Activity ID"))

##BINDING RAW DATA+FEATURES
x_test<-sapply(x_test,as.numeric)

test_bindes_row<-rbind(clean_features,x_test)


colnames(test_bindes_row) <- test_bindes_row[1,]
test_bindes_row <- test_bindes_row[-1, ]

## BINDING ALL DATAFRAMES IN 1 DF CALLED 'TEST'
test<-cbind(test_named,test_bindes_row)



### TRAIN SUBSETI:F YOU WANT TO RUN THE SCRIPT IN YOUR PC, PLEASE CHANGE THE PATH DIRECTORY
subject_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/subject_train.txt")

x_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/X_train.txt")

y_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/y_train.txt")


### BINDING ROWS AND COLUMNS FOR THE TRAIN DATAFRAME:
train_bind<-cbind(subject_train,y_train)
train_named<-setNames(train_bind,c("Subject ID", "Activity ID"))


##BINDING RAW DATA+FEATURES

x_train<-sapply(x_train,as.numeric)

train_bindes_row<-rbind(clean_features,x_train)


colnames(train_bindes_row) <- train_bindes_row[1,]
train_bindes_row <- train_bindes_row[-1, ]

## BINDING ALL DATAFRAMES IN 1 DF CALLED 'TRAIN'
train<-cbind(train_named,train_bindes_row)




DF_merged<-rbind(test,train)

#--------------------------------------------------------------------------------------------------------

## QUESTION 2: Extracts only the measurements on the mean and standard deviation for each measurement. 

columns_match<-grep("mean()|std()",names(DF_merged))
DF_match<-DF_merged[,columns_match]
#-----------------------------------------------------------------------------------------------------------

## QUESTION 3:Use descriptive activity names to name the activities in the data set

# SUBSETTING THE FIRST TWO COLUMNS OF THE MERGED DF 

col_split<- select(DF_merged,c("Subject ID", "Activity ID"))

# CHANGING THE ACTIVITY NUMBERS TO DESCRIPTIVE ACTIVITY NAMES

col_split$`Activity ID`[col_split$`Activity ID`==1]<-"Walking"
col_split$`Activity ID`[col_split$`Activity ID`==2]<-"Walking Upstairs"
col_split$`Activity ID`[col_split$`Activity ID`==3]<-"Walking Downstairs"
col_split$`Activity ID`[col_split$`Activity ID`==4]<-"Sitting"
col_split$`Activity ID`[col_split$`Activity ID`==5]<-"Standing"
col_split$`Activity ID`[col_split$`Activity ID`==6]<-"Laying"

# BINDING THE LAST DF WITH THE DF OBTAINED IN THE LAST QUESTION (2)

DF_match_act<-cbind(col_split,DF_match)


##------------------------------------------------------------------------------------------

## QUESTION 4: Appropriately labels the data set with descriptive variable names. 


#ADDING A NEW COLUMN TO THE LAST DF CALLED GROUP WHICH REPRESENTS THE SET (TEST/TRAIN) OF THE CORRESPONDING SUBJECT ID


        
Group<-as.data.frame(ifelse(DF_match_act$`Subject ID`==2|DF_match_act$`Subject ID`==4|DF_match_act$`Subject ID`==9|
                      DF_match_act$`Subject ID`==10|DF_match_act$`Subject ID`==12|DF_match_act$`Subject ID`==13|
                      DF_match_act$`Subject ID`==18|DF_match_act$`Subject ID`==20|DF_match_act$`Subject ID`==24,"Test","Train"))

Group<-setNames(Group,"Group")
DF_labeled<-cbind(Group,DF_match_act)

##-----------------------------------------------------------------------


## QUESTION 5:From the data set in step 4, creates a second, independent tidy 
#             data set with the average of each variable for each activity and each subject.

#CHANGING VARIABLES FROM CHARACTER TO NUMERIC

DF_labeled1<-DF_labeled[,c(4:82)]
DF_labeled1<-sapply(DF_labeled1,as.numeric)
DF_labeled2<-DF_labeled[,c(1:3)]

DF_labeled3<-cbind(DF_labeled2,DF_labeled1)

#SUMMARIZING DATA

DF_sum<- DF_labeled3 %>% 
        group_by(`Subject ID`,`Activity ID`) %>% 
        select(-starts_with("Group")) %>% 
        summarize_all(funs(mean))

        
#WRITING THE TXT FILE 'TIDY.DATA' TO BE UPLOADED ON GITHUB REPO


write.csv(DF_sum, file="tidy.data.csv",row.names = TRUE, col.names = TRUE)



                     