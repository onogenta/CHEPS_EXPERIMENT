data_relation<-data%>%
  
  mutate(simultaneity=if_else(simultaneity==1,true="simul",false="non_simul"),
         stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "forearm"))%>%
  mutate(simultaneity=factor(simultaneity),stimu_pos=factor(stimu_pos))

data_relation$stimu_pos<- factor(data_relation$stimu_pos,levels = c("wrist","forearm"))

data_relation_wrist<-data_relation%>%
  filter(stimu_pos=="wrist")

data_relation_forearm<-data_relation%>%
  filter(stimu_pos=="forearm")

data_relation_simul<-data_relation%>%
  filter(simultaneity=="simul")

data_relation_nonsimul<-data_relation%>%
  filter(simultaneity=="non_simul")




pain_question_plot<-ggplot(data=data_relation,aes(x=Q1,y=rating_pain))+
  geom_point(stat = 'identity',aes(shape=simultaneity,color=stimu_pos))+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,100))+

  stat_smooth(data=data_relation_wrist,aes(x=Q1,y=rating_pain),method = "lm",se=FALSE,color="#F8766D",size=1)+
  stat_smooth(data=data_relation_forearm,aes(x=Q1,y=rating_pain),method = "lm",se=FALSE,color="#00BFC4",size=1)+

  theme_classic()



pain_question_plot2<-ggplot(data=data_relation,aes(x=Q1,y=rating_pain))+
  geom_point(stat = 'identity',aes(shape=stimu_pos,color=simultaneity))+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,100))+
  scale_color_manual(values = c("firebrick1","dodgerblue"))+
  
  stat_smooth(data=data_relation_simul,aes(x=Q1,y=rating_pain),method = "lm",se=FALSE,color="dodgerblue",size=1)+
  stat_smooth(data=data_relation_nonsimul,aes(x=Q1,y=rating_pain),method = "lm",se=FALSE,color="firebrick1",size=1)+
  
  theme_classic()


file_pass=paste("result/",data$ID[1],"/",sep="")
file_name=paste(file_pass,data$ID[1],"relation.png",sep="")
file_name2=paste(file_pass,data$ID[1],"relation2.png",sep="")

ggsave(file = file_name, plot = pain_question_plot, dpi = 100, width = 8.27, height = 11.69)
ggsave(file = file_name2, plot = pain_question_plot2, dpi = 100, width = 8.27, height = 11.69)