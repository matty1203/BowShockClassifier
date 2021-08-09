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
########## Lasso Regression Model 
####### DATASET USED: DataSet 3 
#####
####  

copy_data<-raw_data_4
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t[!is.na(t$X_KSM16), ]
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(150) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)

## Scaling the data
cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 218, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 218, byrow = FALSE)

## Normalizing the TEST DATASET
cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross

#### Converting all categories into numerical variables NE=0, BS=1 and MP=2
cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.train_scaled$event_occured<-as.factor(cassini.train_scaled$event_occured)
cassini.train_scaled <- dplyr::select(cassini.train_scaled,-c(type_cross,dirn_cross))

##Test Dataset
cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.test_scaled$event_occured<-as.factor(cassini.test_scaled$event_occured)
cassini.test_scaled <- dplyr::select(cassini.test_scaled,-c(type_cross,dirn_cross))


# Setting alpha = 1 implements lasso regression
x_tr<-as.matrix(dplyr::select(cassini.train_scaled,-event_occured))
y_tr<-as.matrix(dplyr::select(cassini.train_scaled,event_occured))
x_te<-as.matrix(dplyr::select(cassini.test_scaled,-event_occured))
y_te<-as.matrix(dplyr::select(cassini.test_scaled,event_occured))

lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- lasso_reg <- cv.glmnet(x_tr, y_tr, alpha = 1, lambda = lambdas, nfolds = 5, family="multinomial")

# Best 
lambda_best <- lasso_reg$lambda.min 

###Lasso Model
lasso_model <- glmnet(x_tr, y_tr, alpha = 1, lambda = lambda_best, family="multinomial")
predictions_train <- predict(lasso_model, s = lambda_best, newx = x_tr,type="class")
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_te,type="class")
conf_mat.lsmodel1<-confusionMatrix(table(predictions_test,y_te))
conf_mat.lsmodel1

##########################################################################################################################################
########## Lasso Regression Model 
####### DATASET USED: DataSet 4 (Average and Standard Deviation Dataset) 
#####
####  

copy_data<-raw_data_5
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t[!is.na(t$Avg_Lead_Bz), ]
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(150)%>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)

## Scaling the data
cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 218, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 218, byrow = FALSE)

## Normalizing the TEST DATASET
cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross

#### Converting all categories into numerical variables NE=0, BS=1 and MP=2
cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.train_scaled$event_occured<-as.factor(cassini.train_scaled$event_occured)
cassini.train_scaled <- dplyr::select(cassini.train_scaled,-c(type_cross,dirn_cross))
cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.test_scaled$event_occured<-as.factor(cassini.test_scaled$event_occured)
cassini.test_scaled <- dplyr::select(cassini.test_scaled,-c(type_cross,dirn_cross))

# Setting alpha = 1 implements lasso regression
x_tr<-as.matrix(dplyr::select(cassini.train_scaled,-event_occured))
y_tr<-as.matrix(dplyr::select(cassini.train_scaled,event_occured))
x_te<-as.matrix(dplyr::select(cassini.test_scaled,-event_occured))
y_te<-as.matrix(dplyr::select(cassini.test_scaled,event_occured))

lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- lasso_reg <- cv.glmnet(x_tr, y_tr, alpha = 1, lambda = lambdas, nfolds = 5, family="multinomial")

# Best 
lambda_best <- lasso_reg$lambda.min 

###Lasso
lasso_model <- glmnet(x_tr, y_tr, alpha = 1, lambda = lambda_best, family="multinomial")
predictions_train <- predict(lasso_model, s = lambda_best, newx = x_tr,type="class")
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_te,type="class")
conf_mat.lsmodel2<-confusionMatrix(table(predictions_test,y_te))
conf_mat.lsmodel2

##########################################################################################################################################
########## Lasso Regression Model 
####### DATASET USED: Dataset 3 (Without Lead Variables) 
#####
####  

copy_data<-raw_data_4[,1:107]
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))

##sampling data from the dataset to make train dataset balanced
cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(150) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
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

#### Converting all categories into numerical variables NE=0, BS=1 and MP=2
cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.train_scaled$event_occured<-as.factor(cassini.train_scaled$event_occured)
cassini.train_scaled <- dplyr::select(cassini.train_scaled,-c(type_cross,dirn_cross))
cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.test_scaled$event_occured<-as.factor(cassini.test_scaled$event_occured)
cassini.test_scaled <- dplyr::select(cassini.test_scaled,-c(type_cross,dirn_cross))


# Setting alpha = 1 implements lasso regression
x_tr<-as.matrix(dplyr::select(cassini.train_scaled,-event_occured))
y_tr<-as.matrix(dplyr::select(cassini.train_scaled,event_occured))
x_te<-as.matrix(dplyr::select(cassini.test_scaled,-event_occured))
y_te<-as.matrix(dplyr::select(cassini.test_scaled,event_occured))

lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- lasso_reg <- cv.glmnet(x_tr, y_tr, alpha = 1, lambda = lambdas, nfolds = 5, family="multinomial")

# Best 
lambda_best <- lasso_reg$lambda.min 

###Lasso
lasso_model <- glmnet(x_tr, y_tr, alpha = 1, lambda = lambda_best, family="multinomial")
predictions_train <- predict(lasso_model, s = lambda_best, newx = x_tr,type="class")
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_te,type="class")
conf_mat.lsmodel3<-confusionMatrix(table(predictions_test,y_te))
conf_mat.lsmodel3

