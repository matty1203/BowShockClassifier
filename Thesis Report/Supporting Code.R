suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(caret))
suppressMessages(library(glmnet))
suppressMessages(library(ranger))
suppressMessages(library(MASS))

### Reading the Raw Datasets from local

raw_data_mag<-readRDS('./Data/dataset_version_0/Full_Cassini_Master_MP_BS_CMJ_revised2005.rds', refhook = NULL)
raw_data_all<-readRDS('./Data/dataset_version_0/Cass_data2005.rds', refhook = NULL)

## Joining the two datasets
copy_data1<-raw_data_mag
copy_data2<-raw_data_all

###################### Formatting Date in raw_data_mag

copy_data1$date<-as.Date(copy_data1$doy_cross,origin="2005-01-01")
head(copy_data1)
copy_data1$time<-paste(copy_data1$hour_cross,copy_data1$minute_cross,"00",sep=":")

###################### Extracting Date, hour cross and minute cross from the timestamp

copy_data2$TimeStamp<-as.POSIXct(copy_data2$Timestamp.UTC., format="%d/%m/%Y %H:%M")
copy_data2$date<-as.Date(format(copy_data2$TimeStamp, "%Y-%m-%d" ))
copy_data2$hour_cross<-as.integer(format(copy_data2$TimeStamp, "%H" )) 
copy_data2$minute_cross<-as.integer(format(copy_data2$TimeStamp, "%M" )) 

######### Merging with time variables  and saving the 

leftJoinDf <- left_join(copy_data2,copy_data1,by=c('date','hour_cross','minute_cross'))
leftJoinDf<-leftJoinDf%>%select(-c(xcrosslist,ycrosslist,zcrosslist));
saveRDS(leftJoinDf,"joined_data.rds")

#### Reading the saved dataset

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

#### Creating Dataset 4



dat<-readRDS('input_datasetFinal.rds', refhook = NULL)
cpy_data<-dat

###Using Row means and rowSDS for finding the mean and standard deviation

#### Below are the indexes of each columns which we us to take average and SD in the dataset

lag_col_index<-c(10,17,24,31,38,45,52,59,63,70,77,84,91,98,105)
lead_col_index<-c(112,119,126,133,140,147,154,161,168,175,183,190,197,204)
shortSD_Data<-data.frame(X_KSM=cpy_data[,1],Y_KSM=cpy_data[,2],
                         Z_KSM=cpy_data[,3],
                         B_Tot=cpy_data[,4],
                         BX=cpy_data[,5],
                         BY=cpy_data[,6],
                         BZ=cpy_data[,7], 
                         type_cross=cpy_data[,8],
                         dirn_cross=cpy_data[,9],
                         Avg_Lag_Bx=rowMeans(cpy_data[,lag_col_index]),
                         Avg_Lag_By=rowMeans(cpy_data[,lag_col_index+1]),
                         Avg_Lag_Bz=rowMeans(cpy_data[,lag_col_index+2]),
                         Avg_Lag_BTot=rowMeans(cpy_data[,lag_col_index+3]),
                         
                         SD_Lag_Bx=rowSds(as.matrix(cpy_data[,lag_col_index])),
                         SD_Lag_By=rowSds(as.matrix(cpy_data[,lag_col_index+1])),
                         SD_Lag_Bz=rowSds(as.matrix(cpy_data[,lag_col_index+2])),
                         SD_Lag_BTot=rowSds(as.matrix(cpy_data[,lag_col_index+3])),
                         
                         Avg_Lead_Bx=rowMeans(cpy_data[,lead_col_index]),
                         Avg_Lead_By=rowMeans(cpy_data[,lead_col_index+1]),
                         Avg_Lead_Bz=rowMeans(cpy_data[,lead_col_index+2]),
                         Avg_Lead_BTot=rowMeans(cpy_data[,lead_col_index+3]),
                         
                         SD_Lead_Bx=rowSds(as.matrix(cpy_data[,lead_col_index])),
                         SD_Lead_By=rowSds(as.matrix(cpy_data[,lead_col_index+1])),
                         SD_Lead_Bz=rowSds(as.matrix(cpy_data[,lead_col_index+2])),
                         SD_Lead_BTot=rowSds(as.matrix(cpy_data[,lead_col_index+3]))
                         
)

saveRDS(shortSD_Data,"Average_SD_Data.rds")


####Reading all the created datasets for future use


raw_data_4<-readRDS('./Data/dataset_version_2/input_datasetFinal.rds', refhook = NULL)
raw_data_5<-readRDS('./Data/dataset_version_2/Average_SD_Data.rds', refhook = NULL)


### Data Exploration

####Line Graph of Total Magnetic Field Strength between 72 and 74th day of the year 2005
d1<-as.Date(72,origin="2005-01-01")
d2<-as.Date(73,origin="2005-01-01")
temp<-filter(raw_data_3,between(date,d1,d2))
bowShock<-filter(temp,type_cross=="BS")$TimeStamp
MagP<-filter(temp,type_cross=="MP")$TimeStamp

ggplot(data=temp, aes(x=TimeStamp , y=abs(BTotal.nT.))) +
  geom_line()+
  ylab("|B|(nT)")+
  xlab("Timestamp")+
  geom_vline(xintercept=bowShock, linetype="dotted",colour="blue",size=1.3)+
  geom_vline(xintercept=MagP, linetype="dotted",colour="red",size=1.3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + theme(legend.position = "none") + 
  ggtitle("Magnetic Field Strength between 72 and 74 day in the year 2005 (Outbound) ") +
  xlab("BTot")

###Line Graph of Total Magnetic Field Strength between 136 and 138th day of the year 2005

d1<-as.Date(136,origin="2005-01-01")
d2<-as.Date(138,origin="2005-01-01")
temp<-filter(raw_data_3,between(date,d1,d2))
bowShock<-filter(temp,type_cross=="BS")$TimeStamp
MagP<-filter(temp,type_cross=="MP")$TimeStamp

ggplot(data=temp, aes(x=TimeStamp , y=abs(BTotal.nT.))) +
  geom_line()+
  ylab("|B|(nT)")+
  xlab("Timestamp")+
  geom_vline(xintercept=bowShock, linetype="dotted",colour="blue",size=1.3)+
  geom_vline(xintercept=MagP, linetype="dotted",colour="red",size=1.3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

###Boundary Crossing points on the Trajectory of Cassini Space craft 

ggplot(raw_widerDataset, aes(x=X_KSM.km. , y=Y_KSM.km.))+
  geom_point(alpha=0.3)+
  geom_point(events_data, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross,shape=dirn_cross))

### Proportion of Direction of Cross in Different Classes

ggplot(data = raw_data_4) + geom_bar(mapping = aes(x = type_cross, fill=dirn_cross) , position = "fill")

###

