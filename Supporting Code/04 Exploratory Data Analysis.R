suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))

#######################################################################################

#### Datasets are available in OneDrive (You can use the Link specified in readme File)

#######################################################################################

### Data Exploration

raw_data_4<-readRDS('./Data/dataset_version_2/input_datasetFinal.rds', refhook = NULL)
raw_data_5<-readRDS('./Data/dataset_version_2/Average_SD_Data.rds', refhook = NULL)

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
### 
###THIS CODE ALSO TAKES SOME TIME TO COMPILE BECAUSE WE ARE TRYING TO PLOT 494000 DATAPOINTS IN A GRAPH
####

events_data<-subset(raw_data_4,type_cross!='NE')
ggplot(raw_data_4, aes(x=X_KSM.km. , y=Y_KSM.km.))+
  geom_point(alpha=0.3)+
  geom_point(events_data, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross,shape=dirn_cross))

### Proportion of Direction of Cross in Different Classes

ggplot(data = raw_data_4) + geom_bar(mapping = aes(x = type_cross, fill=dirn_cross) , position = "fill")

###
