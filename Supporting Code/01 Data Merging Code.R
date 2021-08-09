suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))



#######################################################################################

#### Datasets are available in OneDrive (You can use the Link specified in readme File)

#######################################################################################

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

######### Merging with time variables  and saving the File as joined_data.rds

leftJoinDf <- left_join(copy_data2,copy_data1,by=c('date','hour_cross','minute_cross'))
leftJoinDf<-leftJoinDf%>%select(-c(xcrosslist,ycrosslist,zcrosslist));
saveRDS(leftJoinDf,"joined_data.rds")

### Removing all na values in the dataset and imputing the NA values in direction of cross and Type of cross by 'UD' and 'NE' respectively. 

raw_widerDataset$type_cross<-replace_na(raw_widerDataset$type_cross,"NE")
raw_widerDataset$dirn_cross<-replace_na(raw_widerDataset$dirn_cross,"UD")
raw_widerDataset<-na.omit(raw_widerDataset)
df <- subset(raw_widerDataset, select = -c(TimeStamp,Timestamp.UTC.) )
df$type_cross=as.factor(df$type_cross)
saveRDS(df, "input_dataset.rds")
