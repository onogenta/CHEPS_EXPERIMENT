library(dplyr)
library(purrr)
library(ggplot2)
library(ggforce)
library(ggpubr)
library(tidyr)
library(lubridate)
library(scales)
library(rstatix)
library(lmerTest)
library(lme4)
library(report)
library(MuMIn)
library(ggpmisc)
library(ICC)
library(corrr)

data_load_pulse_self<-function(file_name){
  file_path=paste("raw_data/",file_name,sep="")
  
  data = list.files(path = file_path, full.names = T) %>%
    map(read.csv, col.names=c("time","ID","mode","trial_num","stimu_pos","delay","sound_end_time","trigger_time","knife_contact_time","time_diff","rating_pain","rating_unp",
                              "simultaneity","Q1","Q2","Q3","Q4","Q5"),colClasses = "character", header = F, sep = ",",na.strings="na")%>%
    reduce(rbind)%>%
    mutate(time=as.character(time), ID=factor(ID),mode=factor(mode,levels = c("pain_only","naive","test")),stimu_pos=factor(stimu_pos),trial_num=as.integer(trial_num),
           delay=as.numeric(delay),sound_end_time=as.numeric(sound_end_time),trigger_time=as.numeric(trigger_time),knife_contact_time=as.numeric(knife_contact_time),
           time_diff=as.numeric(time_diff),rating_pain=as.numeric(rating_pain),rating_unp=as.numeric(rating_unp),
           simultaneity=as.numeric(simultaneity),Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4),Q5=as.numeric(Q5))%>%
    mutate_at("simultaneity",~replace(.,is.na(.),1))%>%
    mutate(time=as.POSIXct(time,format="%H:%M"),
           bin=cut(time_diff,breaks=c(-10000,-1250,-1000,-750,-500,-250,0,250,500,750,1000,1250,10000)*0.001,right = FALSE,
                   labels = c("~","-1250~","-1000~","-750~","-500~","-250~","0~","250~","500~","750~","1000~","1250~")),
           bin_delay=cut(delay,breaks=c(-2000,-1250,-1000,-750,-500,-250,0,250,500,750,1000,1250,2000)*0.001,right = FALSE,
                   labels = c("~","-1250~","-1000~","-750~","-500~","-250~","0~","250~","500~","750~","1000~","1250~")),
           simul=factor(if_else(simultaneity==1,true="simul",
                                false = "non_simul"))
    )
  
  
  data$simul<- factor(data$simul,levels = c("simul","non_simul"))
  
  data<-data%>%
    group_by(ID)%>%
    mutate(pain_cwc=rating_pain-mean(rating_pain,na.rm=TRUE),unp_cwc=rating_unp-mean(rating_unp,na.rm=TRUE),
           Q1_cwc=Q1-mean(Q1,na.rm=TRUE),Q2_cwc=Q2-mean(Q2,na.rm=TRUE),Q3_cwc=Q3-mean(Q3,na.rm=TRUE))
  
  data<-data%>%
    group_by(ID,mode)%>%
    mutate(pain_cwc_mode=rating_pain-mean(rating_pain,na.rm=TRUE),unp_cwc_mode=rating_unp-mean(rating_unp,na.rm=TRUE))%>%
    ungroup()
  
 
  
  assign("data_self",data,env=.GlobalEnv)
  
  
}


#,Q1=V10,Q2=V11,Q3=V12,Q4=V13
#,Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4)