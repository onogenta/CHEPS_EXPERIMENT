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



