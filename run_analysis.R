


library(dplyr)


features<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/features.txt")
t_features<-as.data.frame(t(features))
clean_features<-t_features[2,]


subject_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/subject_test.txt")

x_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/X_test.txt")

y_test<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/test/y_test.txt")


test_bind<-cbind(subject_test,y_test)
test_named<-setNames(test_bind,c("Subject ID", "Activity ID"))


x_test<-sapply(x_test,as.numeric)

test_bindes_row<-rbind(clean_features,x_test)


colnames(test_bindes_row) <- test_bindes_row[1,]
test_bindes_row <- test_bindes_row[-1, ]


test<-cbind(test_named,test_bindes_row)


subject_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/subject_train.txt")

x_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/X_train.txt")

y_train<-read.table(file="C:/Users/pdrs8/Desktop/Coursera/Ejercicios cleanning data/final project/UCI HAR Dataset/train/y_train.txt")


train_bind<-cbind(subject_train,y_train)
train_named<-setNames(train_bind,c("Subject ID", "Activity ID"))



x_train<-sapply(x_train,as.numeric)

train_bindes_row<-rbind(clean_features,x_train)


colnames(train_bindes_row) <- train_bindes_row[1,]
train_bindes_row <- train_bindes_row[-1, ]

train<-cbind(train_named,train_bindes_row)

DF_merged<-rbind(test,train)


columns_match<-grep("mean()|std()",names(DF_merged))
DF_match<-DF_merged[,columns_match]

col_split<- select(DF_merged,c("Subject ID", "Activity ID"))

col_split$`Activity ID`[col_split$`Activity ID`==1]<-"Walking"
col_split$`Activity ID`[col_split$`Activity ID`==2]<-"Walking Upstairs"
col_split$`Activity ID`[col_split$`Activity ID`==3]<-"Walking Downstairs"
col_split$`Activity ID`[col_split$`Activity ID`==4]<-"Sitting"
col_split$`Activity ID`[col_split$`Activity ID`==5]<-"Standing"
col_split$`Activity ID`[col_split$`Activity ID`==6]<-"Laying"


DF_match_act<-cbind(col_split,DF_match)



Group<-as.data.frame(ifelse(DF_match_act$`Subject ID`==2|DF_match_act$`Subject ID`==4|DF_match_act$`Subject ID`==9|
                      DF_match_act$`Subject ID`==10|DF_match_act$`Subject ID`==12|DF_match_act$`Subject ID`==13|
                      DF_match_act$`Subject ID`==18|DF_match_act$`Subject ID`==20|DF_match_act$`Subject ID`==24,"Test","Train"))

Group<-setNames(Group,"Group")
DF_labeled<-cbind(Group,DF_match_act)


DF_labeled1<-DF_labeled[,c(4:82)]
DF_labeled1<-sapply(DF_labeled1,as.numeric)
DF_labeled2<-DF_labeled[,c(1:3)]

DF_labeled3<-cbind(DF_labeled2,DF_labeled1)



DF_sum<- DF_labeled3 %>% 
        group_by(`Subject ID`,`Activity ID`) %>% 
        select(-starts_with("Group")) %>% 
        summarize_all(funs(mean))

View(DF_sum) 




                     