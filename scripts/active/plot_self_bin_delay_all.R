##bin=ナイフ接触‐トリガーごとの「同時」回答確率と各測定値のプロット
data_load_pulse_self("all_self")
file_pass=paste("result/active_all/","/",sep="")
#テスト試行のみの全体平均で中心化
data_sum<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin_delay,ID)%>%
  summarise(pain_cwc_avg=mean(pain_cwc_mode),unp_cwc_avg=mean(unp_cwc_mode),Q1_cwc_avg=mean(Q1_cwc),Q2_cwc_avg=mean(Q2_cwc),Q3_cwc_avg=mean(Q3_cwc))%>%
  ungroup()


data_sum_prob<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin_delay)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  ungroup()


template<-theme(axis.title.x = element_text(size = 40),
                axis.text.x = element_text(size=30,face = "bold"),
                axis.title.y = element_text(size = 40),
                axis.text.y = element_text(size=40),
                legend.text = element_text(size=40),
                legend.title = element_text(size = 30,face="bold"),
                legend.position=c(0,1),
                legend.justification = c(-0.3,1),
                plot.title = element_text(size=60,hjust = 0.5))



pain.max=max(data_sum$pain_cwc_avg)
pain.min=min(data_sum$pain_cwc_avg)

y1.lim <- c(ceiling(pain.min*1.1), ceiling(pain.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=pain_cwc_avg))

p_2<-p_1+
  labs(x="Delay(ms)",y="Pain(cwc)",title="Pain")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#ff4b00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_pain<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=4,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template

file_name=paste(file_pass,"plot_bin_delay_pain_all.png",sep="")
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 20,height = 13)


##不快感

unp.max=max(data_sum$unp_cwc_avg)
unp.min=min(data_sum$unp_cwc_avg)

y1.lim <- c(ceiling(unp.min*1.1), ceiling(unp.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=unp_cwc_avg))

p_2<-p_1+
  labs(x="Delay(ms)",y="Pain unpleassantness(cwc)",title="Unpleassantness")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#990099")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_unp<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=4,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template

file_name=paste(file_pass,"plot_bin_delay_delay_unp_all.png",sep="")
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 20,height = 13)


##Q1

Q1.max=max(data_sum$Q1_cwc_avg)
Q1.min=min(data_sum$Q1_cwc_avg)

y1.lim <- c(ceiling(Q1.min*1.1), ceiling(Q1.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q1_cwc_avg))

p_2<-p_1+
  labs(x="Delay(ms)",y="Embodiment(cwc)",title="Embodiment")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#03af7a")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=4,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template


file_name=paste(file_pass,"plot_bin_delay_Q1_all.png",sep="")
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 20,height = 13)


##Q2
Q2.max=max(data_sum$Q2_cwc_avg)
Q2.min=min(data_sum$Q2_cwc_avg)

y1.lim <- c(ceiling(Q2.min*1.1), ceiling(Q2.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q2_cwc_avg))

p_2<-p_1+
  labs(x="Delay(ms)",y="Left arm agency(cwc)",title="Left arm agency")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()
p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#005aff")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q2<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=4,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template

file_name=paste(file_pass,"plot_bin_delay_Q2_all.png",sep="")
ggsave(file = file_name, plot = p_Q2, dpi = 100, width = 20,height = 13)

##Q3

Q3.max=max(data_sum$Q3_cwc_avg)
Q3.min=min(data_sum$Q3_cwc_avg)

y1.lim <- c(ceiling(Q3.min*1.1), ceiling(Q3.max*1.1))
y2.lim <- c(-1.0, 1.5)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q3_cwc_avg))

p_2<-p_1+
  labs(x="Delay(ms)",y="Causality(cwc)",title="Causality")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#f6aa00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q3<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=4,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template


file_name=paste(file_pass,"plot_bin_delay_Q3_all.png",sep="")
ggsave(file = file_name, plot = p_Q3, dpi = 100, width = 20,height = 13)
# p_bin_diff<-ggarrange(p_pain,p_unp,p_Q1,p_Q2,p_Q3,nrow = 5,ncol=1)
# p_bin_diff


#file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_diff.png",sep="")

#ggsave(file = file_name, plot = p_bin_delay_diff, dpi = 300, width = 12,height = 58.45,limitsize = FALSE)







