data_con<-mutate(data,stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "upper_arm"),simultaneity=if_else(simultaneity==1,true="simul",
                                                                                                              false = "non_simul"))
data_con$stimu_pos<- factor(data_con$stimu_pos,levels = c("wrist","upper_arm"))
data_pos_simul$stimu_pos<- factor(data_pos_simul$stimu_pos,levels = c("wrist","upper_arm"))

data_pos_simul<-data_con%>%
  mutate(simultaneity=factor(simultaneity))%>%
  group_by(stimu_pos,simultaneity)%>%
  summarise(pain_avg=mean(rating_pain),unp_avg=mean(rating_unp),pain_sd=sd(rating_pain),unp_sd=sd(rating_unp))



p_con_1<-ggplot(data=data_pos_simul,aes(x=simultaneity,y=pain_avg,fill=stimu_pos))

p_con_2<-p_con_1+
  labs(x="",y="pain_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme_classic()



p_con_3<-p_con_2+
  geom_bar(
           stat = 'identity',
           position = position_dodge(1)
           )

p_con_4<-p_con_3
#   geom_jitter(
#     data=data_con,
#     aes(x=simultaneity,y=rating_pain,shape=stimu_pos),
#     stat = "identity",
#     size=2,
#     position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA)
#   )

p_con_5<-p_con_4+
  geom_errorbar(
    data=data_pos_simul,
    aes(x=simultaneity,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd),
    position = position_dodge(width=1),
    width=.2
  )+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

ggsave(file = "pain_con.png", plot = p_con_5, dpi = 100, width = 8.27,height = 11.69)