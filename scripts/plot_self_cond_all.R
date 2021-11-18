
data_cwc_mean<-data_self%>%
  group_by(ID,mode,simul)%>%
  summarise(pain_cwc_mean=mean(pain_cwc),unp_cwc_mean=mean(unp_cwc))%>%
  ungroup()


plot_cond_avg<-ggplot(aes(x=mode,y=pain_cwc_mean,fill=simul),data = data_cwc_mean)+
  labs(x="",y="pain_cwc_mean")+
  scale_y_continuous(expand = c(0,10),limits = c(-10,15))+
  theme_classic()+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1))+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

plot_cond_avg

#


plot_cond_avg_unp<-ggplot(aes(x=mode,y=unp_cwc_mean,fill=simul),data = data_cwc_mean)+
  labs(x="",y="unp_cwc_mean")+
  scale_y_continuous(expand = c(0,10),limits = c(-20,30))+
  theme_classic()+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(1))+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

plot_cond_avg_unp
#plot_cond_avg_unp


file_name="result/plot_condition_pain_all.png"

ggsave(file = file_name, plot = plot_cond_avg, dpi = 300, width = 8.27,height = 11.68)

file_name="result/plot_condition_unp_all.png"

ggsave(file = file_name, plot = plot_cond_avg_unp, dpi = 300, width = 8.27,height = 11.68)


