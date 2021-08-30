data_question<-data%>%
  tidyr::gather(key=Question,value=rating,Q1,Q2,Q3,Q4)%>%
  filter(trial_num%%10==0)
  

p_question<-ggplot(data_question,aes(x=Question,y=rating))+
  geom_jitter(aes(color=Question),width=0.1,height = 0,show.legend = FALSE,size=3)+
  geom_hline(linetype="dashed",yintercept = 50,col="black")+
  scale_y_continuous(limits = c(0,100))+
  theme_classic()

ggsave(file = "question.png", plot = p_question, dpi = 100, width = 8.27, height = 11.69)


