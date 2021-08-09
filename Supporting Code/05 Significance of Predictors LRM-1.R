suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(glmnet))
suppressMessages(library(ranger))


#######################################################################################

#### Datasets are available in OneDrive (You can use the Link specified in readme File)

#######################################################################################

raw_data_4<-readRDS('./Data/dataset_version_2/input_datasetFinal.rds', refhook = NULL)
raw_data_5<-readRDS('./Data/dataset_version_2/Average_SD_Data.rds', refhook = NULL)

##########################################################################################################################################
########## LOGISTIC REGRESSION MODEL -1 
####### DATASET USED: DataSet 3 
#####
####

copy_data<-raw_data_4
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(300) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)
## Scaling the data

cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 217, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 217, byrow = FALSE)

## Normalizing the TEST DATASET
cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))

for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross

## Logistic Regression Model 1

### BS as 1 and all other events as 0

## Removing dirn_cross and creating new variable `new_trainData`.

new_trainData<-dplyr::select(cassini.train_scaled,-dirn_cross)
new_testData<-dplyr::select(cassini.test_scaled,-c(dirn_cross))

#### Training Data

cassini.logTrain<-new_trainData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTrain$event_occured<-as.factor(cassini.logTrain$event_occured)
cassini.logTrain <- dplyr::select(cassini.logTrain,-c(type_cross))

#### Test Data

cassini.logTest<-new_testData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTest$event_occured<-as.factor(cassini.logTest$event_occured)
cassini.logTest <- dplyr::select(cassini.logTest,-c(type_cross))

#### Model 1 Training and Confusion Matrix

LOGModel1<- glm(event_occured ~ ., family = "binomial", data = cassini.logTrain)
cassini.logTest$prob=predict(LOGModel1,cassini.logTest,type="response")
cassini.logTest$pred <- factor(ifelse(cassini.logTest$prob < .8, 0,1))
conf_mat_log1<-confusionMatrix(cassini.logTest$pred, cassini.logTest$event_occured,positive='1')
conf_mat_log1



#######################################################################################################################################
########## LOGISTIC REGRESSION MODEL -1 
####### DATASET USED: DataSet 4 
#####
####

data_avg<-raw_data_5
copy_data<-data_avg
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(100) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)

## Scaling the data
cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 23, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 23, byrow = FALSE)


## Normalizing the TEST DATASET
cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross


## Logistic Regression Model (Average)
new_trainData<-cassini.train_scaled
new_testData<-cassini.test_scaled

#### Training Data
cassini.logTrain<-new_trainData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTrain$event_occured<-as.factor(cassini.logTrain$event_occured)
cassini.logTrain <- dplyr::select(cassini.logTrain,-c(type_cross))

#### Test Data
cassini.logTest<-new_testData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTest$event_occured<-as.factor(cassini.logTest$event_occured)
cassini.logTest <- dplyr::select(cassini.logTest,-c(type_cross))

#### Model 1 Training and Confusion Matrix
LOGModel2<- glm(event_occured ~ ., family = "binomial", data = cassini.logTrain)
cassini.logTest$prob=predict(LOGModel2,cassini.logTest,type="response")
cassini.logTest$pred <- factor(ifelse(cassini.logTest$prob < .8, 0,1))
conf_mat_log2<-confusionMatrix(cassini.logTest$pred, cassini.logTest$event_occured,positive='1')
conf_mat_log2

#######################################################################################################################################
########## LOGISTIC REGRESSION MODEL -1 
####### DATASET USED: DataSet 3 (without Lead Variables) 
#####
####

copy_data<-raw_data_4[,1:107]
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(100) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)

## Scaling the data
cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 107, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 107, byrow = FALSE)


## Normalizing the TEST DATASET
cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross


## Logistic Regression Model 1 (Without Lead Variables)
new_trainData<-cassini.train_scaled
new_testData<-cassini.test_scaled

### BS as 1 and all other events as 0
#### Training Data
cassini.logTrain<-new_trainData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTrain$event_occured<-as.factor(cassini.logTrain$event_occured)
cassini.logTrain <- dplyr::select(cassini.logTrain,-c(type_cross))

#### Test Data
cassini.logTest<-new_testData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTest$event_occured<-as.factor(cassini.logTest$event_occured)
cassini.logTest <- dplyr::select(cassini.logTest,-c(type_cross))

#### Model 1 Training and Confusion Matrix
LOGModel2<- glm(event_occured ~ ., family = "binomial", data = cassini.logTrain)
cassini.logTest$prob=predict(LOGModel2,cassini.logTest,type="response")
cassini.logTest$pred <- factor(ifelse(cassini.logTest$prob < .8, 0,1))
conf_mat_log3<-confusionMatrix(cassini.logTest$pred, cassini.logTest$event_occured,positive='1')
conf_mat_log3