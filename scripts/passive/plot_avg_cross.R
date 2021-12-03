data_load_pulse("all",load_multi_file = TRUE)
# 
# data_avg<-data%>%
#   group_by(ID,delay,stimu_pos)%>%
#   summarise(pain_avg=mean(rating_pain,na.rm = TRUE),unp_avg=mean(rating_unp,na.rm = TRUE),simul_avg=mean(simultaneity,na.rm=TRUE),Q1_avg=mean(Q1,na.rm = TRUE))%>%
#   ungroup()

data_diff<-data%>%
  group_by(ID)%>%
  mutate(pain_diff=rating_pain-mean(rating_pain,na.rm=TRUE),unp_diff=rating_unp-mean(rating_unp,na.rm=TRUE),Q1_diff=Q1-mean(Q1,na.rm=TRUE),delay_abs=abs(delay))%>%
  ungroup()

data_diff<-data_diff%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm")),simultaneity=factor(if_else(simultaneity==1,true="simul",
                                                                                                false = "non_simul")))

data_diff$stimu_pos<- factor(data_diff$stimu_pos,levels = c("wrist","fore_arm"))


data_diff_avg<-data_diff%>%
  group_by(stimu_pos,simultaneity,ID)%>%
  summarise(pain_diff_avg=mean(pain_diff,na.rm=TRUE),unp_diff_avg=mean(unp_diff,na.rm=TRUE),Q1_diff_avg=mean(Q1_diff,na.rm=TRUE))%>%
  ungroup()



data_diff_sum<-data_diff%>%
  group_by(delay,stimu_pos,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))%>%
  mutate(delay=factor(delay))

#delay絶対値プロット用のデータフレーム
data_diff_sum_abs<-data_diff%>%
  group_by(delay_abs,stimu_pos,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))%>%
  mutate(delay_abs=factor(delay_abs))


data_diff_sum_pos<-data_diff%>%
  group_by(stimu_pos,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))

data_diff_sum_simul<-data_diff%>%
  group_by(simultaneity,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))




# p_con_1<-ggplot(data=data_diff_avg,aes(x=simultaneity,y=pain_diff_avg,fill=stimu_pos))
# 
# p_con_2<-p_con_1+
#   labs(x="",y="pain_rating")+
#   scale_y_continuous(expand = c(0,0),limits = c(-20,20))+
#   theme_classic()
# 
# p_con_3<-p_con_2+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 3,outlier.shape = 8,position = position_dodge(1))+
#   geom_jitter(size=1,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=3,shape=4,col="white",position = position_dodge(width = 1))

p_con_pain<-ggplot(data=data_diff_avg,aes(x=simultaneity,y=pain_diff_avg,fill=stimu_pos))+
  labs(x="",y="pain_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(-20,20))+
  theme_classic()+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 5,outlier.shape = 8,position = position_dodge(1))+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face="bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))


p_con_unp<-ggplot(data=data_diff_avg,aes(x=simultaneity,y=unp_diff_avg,fill=stimu_pos))+
  labs(x="",y="unp_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(-20,20))+
  theme_classic()+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 5,outlier.shape = 8,position = position_dodge(1))+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face="bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))

p_con_Q1<-ggplot(data=data_diff_avg,aes(x=simultaneity,y=Q1_diff_avg,fill=stimu_pos))+
  labs(x="",y="Q1_rating")+
  scale_y_continuous(expand = c(0,0),limits = c(-50,50))+
  theme_classic()+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 5,outlier.shape = 8,position = position_dodge(1))+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 1,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face="bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))

ggsave(file = "result/passive_all/pain_cond_all_passive.png", plot = p_con_pain, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/unp_cond_all_passive.png", plot = p_con_unp, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/Q1_cond_all_passive.png", plot = p_con_Q1, dpi = 100, width = 16, height = 22)


p_test<-ggplot(data = data_diff_sum,aes(x=delay,y=pain_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))








p_test_unp<-ggplot(data = data_diff_sum,aes(x=delay,y=unp_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))

p_test_Q1<-ggplot(data = data_diff_sum,aes(x=delay,y=Q1_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))




ggsave(file = "result/passive_all/result/passive_all/pain_diff_all.png", plot = p_test, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/unp_diff_all.png", plot = p_test_unp, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/Q1_diff_all.png", plot = p_test_Q1, dpi = 100, width = 16, height = 22)



##絶対値プロット
p_test_abs<-ggplot(data = data_diff_sum_abs,aes(x=delay_abs,y=pain_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))

p_test_unp_abs<-ggplot(data = data_diff_sum_abs,aes(x=delay_abs,y=unp_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))

p_test_Q1_abs<-ggplot(data = data_diff_sum_abs,aes(x=delay_abs,y=Q1_avg,fill=stimu_pos))+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  theme_classic()+
  geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
  stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")+
  
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size=20,face = "bold"),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size=20),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 30))


ggsave(file = "result/passive_all/pain_diff_abs_all.png", plot = p_test_abs, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/unp_diff_abs_all.png", plot = p_test_unp_abs, dpi = 100, width = 16, height = 22)
ggsave(file = "result/passive_all/Q1_diff_abs_all.png", plot = p_test_Q1_abs, dpi = 100, width = 16, height = 22)







# ##刺激部位，同時性ごとのプロット
# 
# p_test_pos<-ggplot(data = data_diff_sum_pos,aes(x=stimu_pos,y=pain_avg,fill=stimu_pos))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))
# 
# 
# 
# p_test_simul<-ggplot(data = data_diff_sum_simul,aes(x=simultaneity,y=pain_avg,fill=simultaneity))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))+
#   scale_fill_brewer(palette ="Set1")
# 
# 
# 
# p_test_pos_unp<-ggplot(data = data_diff_sum_pos,aes(x=stimu_pos,y=unp_avg,fill=stimu_pos))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))
# 
# 
# 
# p_test_simul_unp<-ggplot(data = data_diff_sum_simul,aes(x=simultaneity,y=unp_avg,fill=simultaneity))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))+
#   scale_fill_brewer(palette ="Set1")
# 
# p_test_pos_Q1<-ggplot(data = data_diff_sum_pos,aes(x=stimu_pos,y=Q1_avg,fill=stimu_pos))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))
# 
# 
# 
# p_test_simul_Q1<-ggplot(data = data_diff_sum_simul,aes(x=simultaneity,y=Q1_avg,fill=simultaneity))+
#   geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
#   theme_classic()+
#   geom_jitter(size=3,position = position_jitterdodge(jitter.width = 0.1,jitter.height = 0,dodge.width = 0.5,seed = NA))+
#   stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 0.5))+
#   geom_hline(linetype="dashed",yintercept = 0,col="black")+
#   
#   theme(axis.title.x = element_text(size = 30),
#         axis.text.x = element_text(size=20,face = "bold"),
#         axis.title.y = element_text(size = 30),
#         axis.text.y = element_text(size=20),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size = 30))+
#   scale_fill_brewer(palette ="Set1")
# 
# 
# ggsave(file = "pain_diff_pos.png", plot = p_test_pos, dpi = 100, width = 16, height = 22)
# ggsave(file = "pain_diff_simul.png", plot = p_test_simul, dpi = 100, width = 16, height = 22)
# 
# ggsave(file = "unp_diff_pos.png", plot = p_test_pos_unp, dpi = 100, width = 16, height = 22)
# ggsave(file = "unp_diff_simul.png", plot = p_test_simul_unp, dpi = 100, width = 16, height = 22)
# 
# ggsave(file = "Q1_diff_pos.png", plot = p_test_pos_Q1, dpi = 100, width = 16, height = 22)
# ggsave(file = "Q1_diff_simul.png", plot = p_test_simul_Q1, dpi = 100, width = 16, height = 22)
# 


