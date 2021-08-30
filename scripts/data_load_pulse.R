library(dplyr)
library(purrr)
library(ggplot2)
library(ggforce)
library(ggpubr)
library(tidyr)
library(lubridate)
library(scales)

data_load_pulse<-function(file_name){
  file_path=paste("raw_data/",file_name,sep="")
  
  data = list.files(path = file_path, full.names = T) %>%
    map(read.csv, col.names=c("time","ID","stimu_pos","trial_num","delay","delay_num","rating_pain","rating_unp",
                               "simultaneity","Q1","Q2","Q3","Q4"),colClasses = "character", header = F, sep = ",",na.strings="na")%>%
    reduce(rbind)%>%
    mutate(time=as.character(time), ID=factor(ID),stimu_pos=factor(stimu_pos),trial_num=as.integer(trial_num),
           delay=as.numeric(delay),delay_num=factor(delay_num),rating_pain=as.numeric(rating_pain),rating_unp=as.numeric(rating_unp),
           simultaneity=as.numeric(simultaneity),Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4))%>%
    mutate(time=as.POSIXct(time,format="%H:%M"))
  
  
  
  
  assign("data",data,env=.GlobalEnv)
  
  
}

#,Q1=V10,Q2=V11,Q3=V12,Q4=V13
#,Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4)