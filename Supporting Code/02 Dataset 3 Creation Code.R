suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))

###### READ THIS BEFORE RUNNING THIS CODE
######
######   IF YOU RUN THIS CODE IT WILL TAKES 18-36 HOURS TO COMPLETE BECAUSE OF 494,000 DATAPOINTS IN THE DATASET. 
######
######

#######################################################################################

#### Datasets are available in OneDrive (You can use the Link specified in readme File)

#######################################################################################


raw_data_3<-readRDS('./Data/dataset_version_0/joined_data.rds', refhook = NULL)

### Creating the Dataset 3

copy_data<-raw_data_3%>%select(-c(X,DOY.UTC.,SCET.s.,deltaTime.s.,date,hour_cross,minute_cross,year_cross,doy_cross,time,xcrosslist,ycrosslist,zcrosslist,doyfrac_cross));

## A new function created which takes dataset as input and saves the final processed dataset (It takes 1-2 days to completely process the dataset) 
wideData_creator<-function(data_ip){
  data_created<-data.frame();
  i<-0;
  total<-(dim(data_ip)[1]-16)
  for(data_ind in c(16:total )){
    #count restarts for each column in new row
    diff_min<-1;
    out <- paste0("Completed ",data_ind, "/",total)
    print(out)
    data_created<-dplyr::bind_rows(data_created,data_ip[data_ind,])
    
    for(time_ind in c((data_ind-15):(data_ind+15))){
      if(time_ind!=data_ind){
        
        data_created[i,paste("X_KSM",diff_min,sep = "")]<-data_ip[time_ind,'X_KSM.km.']
        data_created[i,paste("Y_KSM",diff_min,sep = "")]<-data_ip[time_ind,'Y_KSM.km.']
        data_created[i,paste("Z_KSM",diff_min,sep = "")]<-data_ip[time_ind,'Z_KSM.km.']
        data_created[i,paste("BX_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BX_KSM.nT.']
        data_created[i,paste("BY_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BY_KSM.nT.']
        data_created[i,paste("BZ_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BZ_KSM.nT.']
        data_created[i,paste("BTot_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BTotal.nT.']
        
      }
      diff_min=diff_min+1;
    }
    i=i+1; 
    # New Data Index
  }
  
  print("Saving Data as a RDS File.....")
  saveRDS(data_created,"Full_Data_Wider.rds")
  print("Function Completed.....")
  return(data_created)
}

wideData_creator(copy_data)
raw_widerDataset <-readRDS('Full_Data_Wider.rds', refhook = NULL)

### Removing all na values in the dataset and imputing the NA values in direction of cross and Type of cross by 'UD' and 'NE' respectively. 

raw_widerDataset$type_cross<-replace_na(raw_widerDataset$type_cross,"NE")
raw_widerDataset$dirn_cross<-replace_na(raw_widerDataset$dirn_cross,"UD")
raw_widerDataset<-na.omit(raw_widerDataset)
df <- subset(raw_widerDataset, select = -c(TimeStamp,Timestamp.UTC.) )
df$type_cross=as.factor(df$type_cross)
saveRDS(df, "input_dataset.rds")