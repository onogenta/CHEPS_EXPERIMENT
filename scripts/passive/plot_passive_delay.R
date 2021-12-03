#手首刺激のデータセット
data_passive_cwc<-data%>%
  filter(stimu_pos==1)%>%
  na.omit(Q1)%>%
  group_by(ID)%>%
  mutate(delay=as.factor(delay),pain_diff=rating_pain-mean(rating_pain,na.rm=TRUE),unp_diff=rating_unp-mean(rating_unp,na.rm=TRUE),Q1_diff=Q1-mean(Q1,na.rm=TRUE))%>%
  ungroup()

data_passive_sum<-data_passive_cwc%>%
  group_by(delay,ID)%>%
  summarise(pain_avg=mean(pain_diff),unp_avg=mean(unp_diff),Q1_avg=mean(Q1_diff))%>%
  ungroup()

data_passive_prob<-data%>%
  filter(stimu_pos==1)%>%
  na.omit(Q1)%>%
  group_by(delay)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  mutate(delay=as.factor(delay))%>%
  ungroup()



#前腕刺激のデータセット 前腕条件だけでだけでcwc
data_passive_cwc_pos2<-data%>%
  filter(stimu_pos==2)%>%
  na.omit(Q1)%>%
  group_by(ID)%>%
  mutate(delay=as.factor(delay),pain_diff=rating_pain-mean(rating_pain,na.rm=TRUE),unp_diff=rating_unp-mean(rating_unp,na.rm=TRUE),Q1_diff=Q1-mean(Q1,na.rm=TRUE))%>%
  ungroup()

data_passive_sum_pos2<-data_passive_cwc_pos2%>%
  group_by(delay,ID)%>%
  summarise(pain_avg=mean(pain_diff),unp_avg=mean(unp_diff),Q1_avg=mean(Q1_diff))%>%
  ungroup()

data_passive_prob_pos2<-data%>%
  filter(stimu_pos==2)%>%
  na.omit(Q1)%>%
  group_by(delay)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  mutate(delay=as.factor(delay))%>%
  ungroup()



##手首刺激のプロット
y1.lim <- c(-25, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum,aes(x=delay,y=pain_avg))

p_2<-p_1+
  labs(x="delay",y="pain_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#ff4b00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_pain<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\npain_cwc\n", color = "",
       title='\npain\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_pain

file_name="result/passive_all/plot_passive_ref.png"
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)


##unp
y1.lim <- c(-25, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum,aes(x=delay,y=unp_avg))

p_2<-p_1+
  labs(x="delay",y="unp_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#990099")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_unp<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\nunp_cwc\n", color = "",
       title='\nunp\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_unp

file_name="result/passive_all/plot_passive_ref_unp.png"
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)


##Q1
y1.lim <- c(-40, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum,aes(x=delay,y=Q1_avg))

p_2<-p_1+
  labs(x="delay",y="Q1_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#03af7a")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\nQ1_cwc\n", color = "",
       title='\nQ1\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q1

file_name="result/passive_all/plot_passive_ref_Q1.png"
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)

##前腕刺激のプロット
y1.lim <- c(-25, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum_pos2,aes(x=delay,y=pain_avg))

p_2<-p_1+
  labs(x="delay",y="pain_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#ff4b00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_pain<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob_pos2)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\npain_cwc\n", color = "",
       title='\npain\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_pain

file_name="result/passive_all/plot_passive_arm_ref.png"
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)


##unp
y1.lim <- c(-25, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum_pos2,aes(x=delay,y=unp_avg))

p_2<-p_1+
  labs(x="delay",y="unp_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#990099")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_unp<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob_pos2)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\nunp_cwc\n", color = "",
       title='\nunp\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_unp

file_name="result/passive_all/plot_passive_arm_ref_unp.png"
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)


##Q1
y1.lim <- c(-40, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_passive_sum_pos2,aes(x=delay,y=Q1_avg))

p_2<-p_1+
  labs(x="delay",y="Q1_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                     ), 
                     name="\nsimul_prob\n")+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#03af7a")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler),size=1,col="grey40",data = data_passive_prob_pos2)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\ndelay\n", y="\nQ1_cwc\n", color = "",
       title='\nQ1\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q1

file_name="result/passive_all/plot_passive_arm_ref_Q1.png"
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)


