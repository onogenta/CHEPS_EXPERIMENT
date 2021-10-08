data_load_pulse("all",load_multi_file = TRUE)

data_avg<-data%>%
  mutate(simultaneity=if_else(simultaneity==1,true="simul",false="non_simul"),
         stimu_pos=if_else(stimu_pos==1,true = "wrist",false = "forearm"))%>%
  mutate(simultaneity=factor(simultaneity),stimu_pos=factor(stimu_pos))%>%
  group_by(ID,stimu_pos,simultaneity)%>%
  summarise(pain_avg=mean(rating_pain,na.rm = TRUE),unp_avg=mean(rating_unp,na.rm = TRUE),Q1_avg=mean(Q1,na.rm = TRUE))%>%
  ungroup()


data_avg=data_avg[order(data_avg$ID,data_avg$stimu_pos,data_avg$simultaneity,decreasing = T),]

data_diff<-data_avg%>%
  group_by(ID)%>%
  mutate(pain_diff=pain_avg-pain_avg[1],Q1_diff=Q1_avg-Q1_avg[1])%>%
  ungroup()%>%
  mutate(condition=case_when(
    
    simultaneity=="simul" & stimu_pos=="forearm" ~ "simul_forearm",
    simultaneity=="simul" & stimu_pos=="wrist" ~ "simul_wrist",
    simultaneity=="non_simul" & stimu_pos=="forearm" ~ "nonsimul_forearm",
    simultaneity=="non_simul" & stimu_pos=="wrist" ~ "nonsimul_wrist"
    
  ))

data_diff$condition<-
  factor(data_diff$condition,levels = c("nonsimul_wrist","nonsimul_forearm","simul_wrist","simul_forearm"))

plot_avg_pain<-ggplot(data_diff)+
  geom_boxplot(aes(x=condition,y=pain_diff,fill=condition),width=0.5,outlier.colour = "red",outlier.size = 3,outlier.shape = 8)+
  geom_jitter(aes(x=condition,y=pain_diff),width=0.05)+
  stat_summary(aes(x=condition,y=pain_diff),fun=mean,geom="point",size=3,shape=4,col="white")+
  scale_y_continuous(limits = c(-70,10))+
  theme_classic()

plot_avg_pain

plot_avg_Q1<-ggplot(data_diff)+
  geom_boxplot(aes(x=condition,y=Q1_diff,fill=condition),width=0.5,outlier.colour = "red",outlier.size = 3,outlier.shape = 8)+
  geom_jitter(aes(x=condition,y=Q1_diff),width=0.05)+
  stat_summary(aes(x=condition,y=Q1_diff),fun=mean,geom="point",size=3,shape=4,col="white")+
  scale_y_continuous(limits = c(-70,10))+
  theme_classic()

plot_avg_Q1

ggsave(file = "result/pain_diff.png", plot = plot_avg_pain, dpi = 100, width = 8.27, height = 11.69)
ggsave(file = "result/Q1_diff.png", plot = plot_avg_Q1, dpi = 100, width = 8.27, height = 11.69)

#こっから1群のｔ検定(片側)

test_n_w=data_diff[data_diff$condition=="nonsimul_wrist",]$pain_diff
test_n_f=data_diff[data_diff$condition=="nonsimul_forearm",]$pain_diff
test_s_f=data_diff[data_diff$condition=="simul_forearm",]$pain_diff


t.test(test_n_w,mu=0,alternative = "less")
t.test(test_n_f,mu=0,alternative = "less")
t.test(test_s_f,mu=0,alternative = "less")

#こっから多重比較と分散分析？
library(multcomp)
summary(aov(pain_avg~simultaneity+stimu_pos+Error(ID+ID:simultaneity+ID:stimu_pos+ID:simultaneity:stimu_pos),data = data_diff))

bartlett.test(pain_diff~condition,data = data_diff)
data_test_tukey<-data_diff%>%
  filter(condition!="simul_wrist")

TukeyHSD(aov(pain_diff~condition,data = data_diff))

