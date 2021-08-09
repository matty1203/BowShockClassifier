suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(glmnet))
suppressMessages(library(ranger))
suppressMessages(library(caret))

#######################################################################################

#### Datasets are available in OneDrive (You can use the Link specified in readme File)

#######################################################################################

raw_data_4<-readRDS('./Data/dataset_version_2/input_datasetFinal.rds', refhook = NULL)
raw_data_5<-readRDS('./Data/dataset_version_2/Average_SD_Data.rds', refhook = NULL)

##########################################################################################################################################
########## Random Forest Model 
####### DATASET USED: DataSet 3 
#####
####  


copy_data<-raw_data_4
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t[!is.na(t$X_KSM16), ]
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

### Sampling from Dataset to create a balanced train dataset
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(300) %>%
  rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
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

## Removing dirn_cross and creating new variable `new_trainData`.
new_trainData<-dplyr::select(cassini.train_scaled,-dirn_cross)
new_testData<-cassini.test_scaled

#### Random Forrest Model Training and Confusion Matrix
cassini.rfmodel1 <- ranger(type_cross ~ ., data = new_trainData)
new_testData$pred<-predict(cassini.rfmodel1,data = new_testData)$predictions
conf_mat.rfmodel1<-confusionMatrix(new_testData$pred, new_testData$type_cross)
conf_mat.rfmodel1

##########################################################################################################################################
########## Random Forest Model 
####### DATASET USED: DataSet 4 (Average and SD Dataset) 
#####
#### 

data_avg<-raw_data_5
copy_data<-data_avg
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t[!is.na(t$Avg_Lead_Bz), ]
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

### Sampling from Dataset to create a balanced train dataset
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(300) %>%
  rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
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
new_trainData<-dplyr::select(cassini.train_scaled,-dirn_cross)
new_testData<-cassini.test_scaled

#### Random Forrest Model Training and Confusion Matrix
cassini.rfmodel2 <- ranger(type_cross ~ ., data = new_trainData)
new_testData$pred<-predict(cassini.rfmodel2,data = new_testData)$predictions
conf_mat.rfmodel2<-confusionMatrix(new_testData$pred, new_testData$type_cross)
conf_mat.rfmodel2

##########################################################################################################################################
########## Random Forest Model 
####### DATASET USED: DataSet 3 (Without Lead Variables) 
#####
#### 

copy_data<-raw_data_4[,1:107]
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

### Sampling from Dataset to create a balanced train dataset
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
new_trainData<-dplyr::select(cassini.train_scaled,-dirn_cross)
new_testData<-dplyr::select(cassini.test_scaled,-dirn_cross)

#### Random Forest Model Training and Confusion Matrix
cassini.rfmodel3 <- ranger(type_cross ~ ., data = new_trainData,num.trees = 500,importance = 'impurity')
new_testData$pred<-predict(cassini.rfmodel3,data = new_testData)$predictions
conf_mat.rfmodel3<-confusionMatrix(new_testData$pred, new_testData$type_cross)
conf_mat.rfmodel3
