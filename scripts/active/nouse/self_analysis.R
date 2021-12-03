##active実験の結果プロット
##データの加工，simultaneity=1-->simul,0 -->non_simulへ


##3条件：熱刺激のみ，最初の3回，本実験での痛み評価値と不快感評価値の平均プロット


plot_cond_avg<-ggplot(aes(x=mode,y=rating_pain,fill=simul),data = data_self)+
  labs(x="",y="pain_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme_classic()+
  geom_bar(stat="summary",fun="mean",width=0.5,position = position_dodge(0.5))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=.2,position = position_dodge(width = .5))+
  geom_point(size=1,stroke=1,position = position_dodge(width = .5))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")



#


plot_cond_avg_unp<-ggplot(aes(x=mode,y=rating_unp,fill=simul),data = data_self)+
  labs(x="",y="unp_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme_classic()+
  geom_bar(stat="summary",fun="mean",width=0.5,position = position_dodge(0.5))+
  stat_summary(fun.data = "mean_se",geom = "errorbar",width=.2,position = position_dodge(width = .5))+
  geom_point(size=1,stroke=1,position = position_dodge(width = .5))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))


#plot_cond_avg_unp

p_condition<-ggarrange(plot_cond_avg,plot_cond_avg_unp,nrow = 2,ncol=1)
p_condition
file_pass=paste("result/",data_self$ID[1],"/",sep="")
file_name=paste(file_pass,data_self$ID[1],"plot_condition.png",sep="")

ggsave(file = file_name, plot = p_condition, dpi = 300, width = 8.27,height = 23.38)



##binごとの「同時」回答確率と各測定値のプロット

data_sum<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),simul_prob=mean(simultaneity),Q1_avg=mean(Q1),Q2_avg=mean(Q2),Q3_avg=mean(Q3),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp),
            Q1_sd=sd(Q1),Q2_sd=sd(Q2),Q3_sd=sd(Q3))%>%
  ungroup()

y1.lim <- c(-10, 100)
y2.lim <- c(0, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_sum,aes(x=bin,y=pain_avg))

p_2<-p_1+
  labs(x="delay",y="pain")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 
                                        breaks=seq(from=y2.lim[1], to=y2.lim[2], by=0.2), 
                                        name="\nsimul_prob\n"))+
  theme_classic()

p_3<-p_2+
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.1),fill="brown3")+
  geom_errorbar(
    aes(ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=0.1),
    width=.3
  )+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_4<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler),size=1,col="grey40")+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  labs(x="\nbin\n", y="\npain_rating\n", color = "",
       title='\npain\n', 
       subtitle='')+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=15,face = "bold"),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30),
        plot.title = element_text(size=30,hjust = 0.5))


p_4
