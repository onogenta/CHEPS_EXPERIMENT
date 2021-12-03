data_question<-data%>%
  
  mutate(simultaneity=if_else(simultaneity==1,true="simul",false="non_simul"))%>%
  mutate(simultaneity=factor(simultaneity))%>%
  tidyr::gather(key=Question,value=rating,Q1,Q2,Q3,Q4)%>%
  filter(trial_num%%10==0)%>%
  group_by(simultaneity)
  
data_question_simul<-data_question%>%
  group_by(simultaneity,Question)%>%
  summarise(Q_avg=mean(rating),Q_sd=sd(rating))


data_question_Q1<-data_question%>%
  filter(Question=="Q1")

data_question_simul_Q1<-
  data_question%>%
  group_by(simultaneity,Question,stimu_pos)%>%
  summarise(Q_avg=mean(rating),Q_sd=sd(rating))%>%
  filter(Question=="Q1")





p_q1<-ggplot(data_question_simul,aes(x=Question,y=Q_avg,fill=simultaneity))+
  
  geom_bar(stat='identity', width=0.5,position = position_dodge(0.5))+
  
  
  geom_jitter(
    data=data_question,
    aes(x=Question,y=rating),
    stat = "identity",
    size=1,
    position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA)
  )+
  
  
  geom_errorbar(
    data=data_question_simul,
    aes(x=Question,ymin=Q_avg-Q_sd,ymax=Q_avg+Q_sd),
    position = position_dodge(width=0.5),
    width=.2
  )+

  geom_hline(linetype="dashed",yintercept = 50,col="black")+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_brewer(palette ="Set1")+
  theme_classic()





p_q2<-ggplot(data_question_simul_Q1,aes(x=simultaneity,y=Q_avg,fill=stimu_pos))+
  
  labs(x="",y="Q1_score")+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme_classic()+
  
  geom_bar(
    stat = 'identity',
    position = position_dodge(1)
  )+
  
  geom_jitter(
    data=data_question_Q1,
    aes(x=simultaneity,y=rating,shape=stimu_pos),
    stat = "identity",
    size=2,
    position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA)
  )+
  geom_errorbar(
    data=data_question_simul_Q1,
    aes(x=simultaneity,ymin=Q_avg-Q_sd,ymax=Q_avg+Q_sd),
    position = position_dodge(width=1),
    width=.2
  )+
  geom_hline(linetype="dashed",yintercept = 50,col="black")



# p_question<-ggplot(data_question,aes(x=Question,y=rating))+
#   
#   geom_jitter(aes(color=Question,shape=simultaneity),width=0.1,height = 0,show.legend = TRUE,size=3)+
#   geom_hline(linetype="dashed",yintercept = 50,col="black")+
#   scale_y_continuous(limits = c(0,100))+
#   theme_classic()

ggsave(file = "question_all.png", plot = p_q1, dpi = 100, width = 8.27, height = 11.69)
ggsave(file = "question_Q1.png", plot = p_q2, dpi = 100, width = 8.27, height = 11.69)


