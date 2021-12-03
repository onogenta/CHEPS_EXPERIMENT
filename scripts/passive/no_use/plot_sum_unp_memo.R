data_all<-data%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm"))


data_sum<-data%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm"))%>%
  group_by(stimu_pos,delay)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),simul_prob=mean(simultaneity),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp))

data_all$stimu_pos<- factor(data_all$stimu_pos,levels = c("wrist","upper_arm"))
data_sum$stimu_pos<- factor(data_sum$stimu_pos,levels = c("wrist","upper_arm"))                                                                                                  


data_pos_1<-data%>%
  filter(stimu_pos==1)%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm")
         ,simultaneity=if_else(simultaneity==1,true="simul",
                               false = "non_simul"),simultaneity=factor(simultaneity))

data_pos_2<-data%>%
  filter(stimu_pos==2)%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm")
         ,simultaneity=if_else(simultaneity==1,true="simul",
                               false = "non_simul"),simultaneity=factor(simultaneity))

data_sum_pos1<-data%>%
  filter(stimu_pos==1)%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm"))%>%
  
  group_by(stimu_pos,delay)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),simul_prob=mean(simultaneity),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp))


data_sum_pos2<-data%>%
  filter(stimu_pos==2)%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm"))%>%
  
  group_by(stimu_pos,delay)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),simul_prob=mean(simultaneity),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp))






x.lim  <- c(-1,1)
y1.lim <- c(0, 100)
y2.lim <- c(0, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=delay,y=unp_avg,fill=stimu_pos))

p_2<-p_1+
  labs(x="delay",y="pain_rating")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.1,position = position_dodge(0.1))+
  # geom_jitter(
  #   data=data_all,
  #   aes(x=delay,y=rating_pain,shape=stimu_pos),
  #   stat = "identity",
  #   size=1.5,
  #   position = position_jitterdodge(jitter.width = 0.01,jitter.height = 0,dodge.width = 0.1,seed = NA)
  # )+
  geom_errorbar(
    data=data_sum,
    aes(x=delay,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd),
    position = position_dodge(width=0.1),
    width=.05
  )

p_4<-p_3+
  geom_line(aes(x=delay,y=simul_prob*scaler, colour = stimu_pos), size=1)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_unp\n", color = "",
       title='\n\n', 
       subtitle='')



p_1_pos_1<-ggplot(data=data_sum_pos1,aes(x=delay,y=unp_avg))+
  labs(x="delay",y="unp_rating")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()+
  geom_bar(stat='identity', width=0.1,position = position_dodge(0.1),fill='#F8766D')+
  geom_jitter(
    data=data_pos_1,
    aes(x=delay,y=rating_unp,shape=simultaneity),
    stat = "identity",
    size=1.5,
    position = position_jitterdodge(jitter.width = 0.01,jitter.height = 0,dodge.width = 0.1,seed = NA)
  )+
  geom_errorbar(
    data=data_sum_pos1,
    aes(x=delay,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd),
    position = position_dodge(width=0.1),
    width=.05
  )+
  #geom_hline(linetype="dashed",yintercept = 30,col="black")+
  geom_line(aes(x=delay,y=simul_prob*scaler, colour = "simul_prob"), size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_unp\n", color = "",
       title='\n\n', 
       subtitle='wrist')


p_1_pos_2<-ggplot(data=data_sum_pos2,aes(x=delay,y=unp_avg))+
  labs(x="delay",y="unp_rating")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()+
  geom_bar(stat='identity', width=0.1,position = position_dodge(0.1),fill='#00BFC4')+
  geom_jitter(
    data=data_pos_2,
    aes(x=delay,y=rating_unp,shape=simultaneity),
    stat = "identity",
    size=1.5,
    position = position_jitterdodge(jitter.width = 0.01,jitter.height = 0,dodge.width = 0.1,seed = NA)
  )+
  geom_errorbar(
    data=data_sum_pos2,
    aes(x=delay,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd),
    position = position_dodge(width=0.1),
    width=.05
  )+
  #geom_hline(linetype="dashed",yintercept = 30,col="black")+
  geom_line(aes(x=delay,y=simul_prob*scaler, colour = "simul_prob"), size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_pain\n", color = "",
       title='\n\n', 
       subtitle='upper_arm')

p<-ggarrange(p_4,p_1_pos_1,p_1_pos_2, nrow = 3, ncol = 1)


file_pass=paste("result/",data$ID[1],"/",sep="")
file_name=paste(file_pass,data$ID[1],"unp_all.png",sep="")

ggsave(file = file_name, plot = p, dpi = 100, width = 16, height = 22)







