data_load_pulse("all",load_multi_file = TRUE)

data_avg<-data%>%
  group_by(ID,delay,stimu_pos)%>%
  summarise(pain_avg=mean(rating_pain,na.rm = TRUE),unp_avg=mean(rating_unp,na.rm = TRUE),simul_avg=mean(simultaneity,na.rm=TRUE),Q1_avg=mean(Q1,na.rm = TRUE))%>%
  ungroup()

data_diff<-data_avg%>%
  group_by(ID)%>%
  mutate(pain_diff=pain_avg/pain_avg[delay==0&stimu_pos==1],Q1_diff=Q1_avg/Q1_avg[delay==0&stimu_pos==1])%>%
  ungroup()

data_diff<-data_diff%>%
  mutate(stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "fore_arm"))

data_diff$stimu_pos<- factor(data_diff$stimu_pos,levels = c("wrist","fore_arm"))

data_diff_sum<-data_diff%>%
  group_by(delay,stimu_pos)%>%
  summarise(pain_avg=mean(pain_diff),simul_avg=mean(simul_avg),pain_sd=sd(pain_diff))

data_pos_1<-data_diff%>%
  filter(stimu_pos=="wrist")

data_pos_2<-data_diff%>%
  filter(stimu_pos=="fore_arm")

data_sum_pos1<-data_pos_1%>%
  group_by(delay)%>%
  summarise(pain_avg=mean(pain_diff),simul_avg=mean(simul_avg),pain_sd=sd(pain_diff))

data_sum_pos2<-data_pos_2%>%
  group_by(delay)%>%
  summarise(pain_avg=mean(pain_diff),simul_avg=mean(simul_avg),pain_sd=sd(pain_diff))



x.lim  <- c(-1,1)
y1.lim <- c(0, 1.5)
y2.lim <- c(0, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_diff_sum,aes(x=delay,y=pain_avg,fill=stimu_pos))

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
    data=data_diff_sum,
    aes(x=delay,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=0.1),
    width=.05
  )+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_4<-p_3+
  geom_line(aes(x=delay,y=simul_avg*scaler, colour = stimu_pos), size=1)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_pain\n", color = "",
       title='\n\n', 
       subtitle='')



p_1_pos_1<-ggplot(data=data_sum_pos1,aes(x=delay,y=pain_avg))+
  labs(x="delay",y="pain_rating")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()+
  geom_bar(stat='identity', width=0.1,position = position_dodge(0.1),fill='#F8766D')+
  # geom_jitter(
  #   data=data_pos_1,
  #   aes(x=delay,y=rating_pain,shape=simultaneity),
  #   stat = "identity",
  #   size=1.5,
  #   position = position_jitterdodge(jitter.width = 0.01,jitter.height = 0,dodge.width = 0.1,seed = NA)
  # )+
  geom_errorbar(
    data=data_sum_pos1,
    aes(x=delay,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=0.1),
    width=.05
  )+
  geom_hline(linetype="dashed",yintercept = 30,col="black")+
  geom_line(aes(x=delay,y=simul_avg*scaler, colour = "simul_prob"), size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_pain\n", color = "",
       title='\n\n', 
       subtitle='wrist')


p_1_pos_2<-ggplot(data=data_sum_pos2,aes(x=delay,y=pain_avg))+
  labs(x="delay",y="pain_rating")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()+
  geom_bar(stat='identity', width=0.1,position = position_dodge(0.1),fill='#00BFC4')+
  # geom_jitter(
  #   data=data_pos_2,
  #   aes(x=delay,y=rating_pain,shape=simultaneity),
  #   stat = "identity",
  #   size=1.5,
  #   position = position_jitterdodge(jitter.width = 0.01,jitter.height = 0,dodge.width = 0.1,seed = NA)
  # )+
  geom_errorbar(
    data=data_sum_pos2,
    aes(x=delay,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=0.1),
    width=.05
  )+
  #geom_hline(linetype="dashed",yintercept = 30,col="black")+
  geom_line(aes(x=delay,y=simul_avg*scaler, colour = "simul_prob"), size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")
  theme(text=element_text(size=12, family="MS Gothic",color="black"),
        axis.text=element_text(size=10, family="MS Gothic",color="black"),
        legend.position="top",
        plot.subtitle=element_text(size=10, color="#666666")) + 
  labs(x="\ndelay\n", y="\nrating_pain\n", color = "",
       title='\n\n', 
       subtitle='upper_arm')

p<-ggarrange(p_4,p_1_pos_1,p_1_pos_2, nrow = 3, ncol = 1)
ggsave(file = "pain_all.png", plot = p, dpi = 100, width = 16, height = 22)








