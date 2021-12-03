##bin=ナイフ接触‐トリガーごとの「同時」回答確率と各測定値のプロット
data_load_pulse_self("all_self")
file_pass=paste("result/active_all/","/",sep="")
#テスト試行のみの全体平均で中心化
data_sum<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin,ID)%>%
  summarise(pain_cwc_avg=mean(pain_cwc_mode),unp_cwc_avg=mean(unp_cwc_mode),Q1_cwc_avg=mean(Q1_cwc),Q2_cwc_avg=mean(Q2_cwc),Q3_cwc_avg=mean(Q3_cwc))%>%
  ungroup()


data_sum_prob<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  ungroup()



##面積比用のデータフレーム
data_sum_comp<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin)%>%
  summarise(pain_cwc_avg=mean(pain_cwc_mode),unp_cwc_avg=mean(unp_cwc_mode),Q1_cwc_avg=mean(Q1_cwc),Q2_cwc_avg=mean(Q2_cwc),Q3_cwc_avg=mean(Q3_cwc))%>%
  ungroup()




y1.lim <- c(-25, 30)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin,y=pain_cwc_avg))

p_2<-p_1+
  #labs(x="delay",y="pain_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler
                                        ,name="\nsimul_prob\n") 
                                        )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#ff4b00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_pain<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\npain_cwc_avg\n", color = "",
       title='\npain\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

file_name=paste(file_pass,"plot_bin_diff_pain_all.png",sep="")
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)


##不快感
p_1<-ggplot(data=data_sum,aes(x=bin,y=unp_cwc_avg))

p_2<-p_1+
  labs(x="delay",y="unp_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nsimul_prob\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#990099")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_unp<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\nunp_cwc_avg\n", color = "",
       title='\nunp\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))


file_name=paste(file_pass,"plot_bin_diff_unp_all.png",sep="")
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)


##Q1

y1.lim <- c(-50, 50)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])
p_1<-ggplot(data=data_sum,aes(x=bin,y=Q1_cwc_avg))

p_2<-p_1+
  labs(x="delay",y="unp_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nsimul_prob\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#03af7a")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\nQ1_cwc_avg\n", color = "",
       title='\nembodiment\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))


file_name=paste(file_pass,"plot_bin_diff_Q1_all.png",sep="")
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)


##Q2
p_1<-ggplot(data=data_sum,aes(x=bin,y=Q2_cwc_avg))

p_2<-p_1+
  labs(x="delay",y="Q2_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nsimul_prob\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#005aff")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q2<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\nQ2_cwc_avg\n", color = "",
       title='\nagency(leftarm)\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

file_name=paste(file_pass,"plot_bin_diff_Q2_all.png",sep="")
ggsave(file = file_name, plot = p_Q2, dpi = 100, width = 8.27,height = 11.69)

##Q3

y1.lim <- c(-75, 75)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])
p_1<-ggplot(data=data_sum,aes(x=bin,y=Q3_cwc_avg))

p_2<-p_1+
  labs(x="delay",y="Q3_cwc")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        name="\nsimul_prob\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1),fill="#f6aa00")+
  geom_jitter(size=3,position = position_dodge(width = 1))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q3<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40",data = data_sum_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\ncQ3_cwc\n", color = "",
       title='\ncausality\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))


file_name=paste(file_pass,"plot_bin_diff_Q3_all.png",sep="")
ggsave(file = file_name, plot = p_Q3, dpi = 100, width = 8.27,height = 11.69)
# p_bin_diff<-ggarrange(p_pain,p_unp,p_Q1,p_Q2,p_Q3,nrow = 5,ncol=1)
# p_bin_diff


#file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_diff.png",sep="")

#ggsave(file = file_name, plot = p_bin_delay_diff, dpi = 300, width = 12,height = 58.45,limitsize = FALSE)







