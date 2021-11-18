##bin=delayごとの「同時」回答確率と各測定値のプロット

data_sum<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin_delay)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),simul_prob=mean(simultaneity),Q1_avg=mean(Q1),Q2_avg=mean(Q2),Q3_avg=mean(Q3),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp),
            Q1_sd=sd(Q1),Q2_sd=sd(Q2),Q3_sd=sd(Q3))%>%
  ungroup()

y1.lim <- c(-10, 100)
y2.lim <- c(0, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=pain_avg))

p_2<-p_1+
  labs(x="delay",y="pain")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="#ff4b00")+
  geom_errorbar(
    aes(ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=0.1),
    width=.3
  )+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_pain<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin_delay\n", y="\npain_rating\n", color = "",
       title='\npain\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))



##不快感
p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=unp_avg))

p_2<-p_1+
  labs(x="delay",y="unp")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="#990099")+
  geom_errorbar(
    aes(ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd),
    position = position_dodge(width=0.1),
    width=.3
  )

p_unp<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin_delay\n", y="\nunp_rating\n", color = "",
       title='\nunp\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

##Q1
p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q1_avg))

p_2<-p_1+
  labs(x="delay",y="Q1")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="#03af7a")+
  geom_errorbar(
    aes(ymin=Q1_avg-Q1_sd,ymax=Q1_avg+Q1_sd),
    position = position_dodge(width=0.1),
    width=.3
  )+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin_delay\n", y="\nQ1_rating\n", color = "",
       title='\nQ1:embodiment\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q1


##Q2
p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q2_avg))

p_2<-p_1+
  labs(x="delay",y="Q2")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="#005aff")+
  geom_errorbar(
    aes(ymin=Q2_avg-Q2_sd,ymax=Q2_avg+Q2_sd),
    position = position_dodge(width=0.1),
    width=.3
  )+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q2<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin_delay\n", y="\nQ2_rating\n", color = "",
       title='\nQ2:agency(leftarm)\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))


##Q3
p_1<-ggplot(data=data_sum,aes(x=bin_delay,y=Q3_avg))

p_2<-p_1+
  labs(x="delay",y="Q3")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="#f6aa00")+
  geom_errorbar(
    aes(ymin=Q3_avg-Q3_sd,ymax=Q3_avg+Q3_sd),
    position = position_dodge(width=0.1),
    width=.3
  )+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q3<-p_3+
  geom_line(aes(x=as.numeric(bin_delay),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin_delay\n", y="\nQ3_rating\n", color = "",
       title='\nQ3:causality\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))

#p_bin_delay_diff<-ggarrange(p_pain,p_unp,p_Q1,p_Q2,p_Q3,nrow = 5,ncol=1)
#p_bin_delay_diff

file_pass=paste("result/",data_self$ID[1],"/",sep="")
#file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_diff.png",sep="")

#ggsave(file = file_name, plot = p_bin_delay_diff, dpi = 300, width = 12,height = 58.45,limitsize = FALSE)


file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_pain.png",sep="")
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)

file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_unp.png",sep="")
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)

file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_Q1.png",sep="")
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)

file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_Q2.png",sep="")
ggsave(file = file_name, plot = p_Q2, dpi = 100, width = 8.27,height = 11.69)

file_name=paste(file_pass,data_self$ID[1],"plot_bin_delay_Q3.png",sep="")
ggsave(file = file_name, plot = p_Q3, dpi = 100, width = 8.27,height = 11.69)





