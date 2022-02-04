data_load_pulse("all",load_multi_file = TRUE)　#passive実験の各被験者のデータを読み込み

# 
# data_avg<-data%>%
#   group_by(ID,delay,stimu_pos)%>%
#   summarise(pain_avg=mean(rating_pain,na.rm = TRUE),unp_avg=mean(rating_unp,na.rm = TRUE),simul_avg=mean(simultaneity,na.rm=TRUE),Q1_avg=mean(Q1,na.rm = TRUE))%>%
#   ungroup()

data_diff<-data%>%
  na.omit(Q1)%>%　　#Q1 が欠損しているデータを除外(※passive実験では，前半実施分の被験者は全てのトライアルでアンケートが出現していない)
  group_by(ID)%>%   #被験者毎に，各回答値を中心化(それぞれの値を被験者毎の平均値で引く)
  mutate(pain_diff=rating_pain-mean(rating_pain,na.rm=TRUE),unp_diff=rating_unp-mean(rating_unp,na.rm=TRUE),Q1_diff=Q1-mean(Q1,na.rm=TRUE),
         Q2_diff=Q2-mean(Q2,na.rm=TRUE),Q3_diff=Q3-mean(Q3,na.rm=TRUE),Q4_diff=Q4-mean(Q4,na.rm=TRUE),delay_abs=abs(delay))%>%
  ungroup()

data_diff<-data_diff%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm")),simultaneity=factor(if_else(simultaneity==1,true="simul",
                                                                                                               false = "non_simul")))

data_diff$stimu_pos<- factor(data_diff$stimu_pos,levels = c("wrist","fore_arm"))

data_diff_sum<-data_diff%>%
  mutate(delay_abs=factor(delay_abs))%>%
  group_by(ID,stimu_pos,delay)%>%
  summarise(pain_avg=mean(pain_diff),unp_avg=mean(unp_diff),Q1_avg=mean(Q1_diff),Q1_avg2=mean(Q1),Q2_avg=mean(Q2),Q3_avg=mean(Q3),Q4_avg=mean(Q4))%>%
  ungroup()

data_diff_sum_sum<-data_diff_sum%>%
  group_by(delay,stimu_pos)%>%
  summarise(Q1_sum=mean(Q1_avg2),Q1_sd=sd(Q1_avg2),Q2_sum=mean(Q2_avg),Q2_sd=sd(Q2_avg),Q3_sum=mean(Q3_avg),Q3_sd=sd(Q3_avg),Q4_sum=mean(Q4_avg),Q4_sd=sd(Q4_avg))


data_passive_prob<-data%>%
  na.omit(Q1)%>%
  group_by(delay,stimu_pos)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  mutate(delay=as.factor(delay),stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm")))%>%
  ungroup()

data_passive_prob_abs<-data%>%
  na.omit(Q1)%>%
  group_by(delay,stimu_pos)%>%
  summarise(simul_prob=mean(simultaneity))%>%
  mutate(delay_abs=as.factor(delay_abs),stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm")))%>%
  ungroup()




#コントロール質問によるチェック
data_check<-data%>%
  na.omit(Q1)%>%
  group_by(ID)%>%
  summarise(Q1_mean=mean(Q1),Q2_mean=mean(Q2),Q3_mean=mean(Q3),Q4_mean=mean(Q4))%>%
  ungroup()

expect_sub<-data_check%>%
  filter((Q1_mean>=50||Q2_mean>=50)&&(Q3_mean<=50&&Q4_mean<=50))


##プロットテーマのテンプレ

template<-theme(axis.title.x = element_text(size = 40),
                axis.text.x = element_text(size=30,face = "bold"),
                axis.title.y = element_text(size = 40),
                axis.text.y = element_text(size=40),
                legend.text = element_text(size=40),
                legend.title = element_text(size = 30,face="bold"),
                legend.position=c(0,1),
                legend.justification = c(-0.3,1),
                plot.title = element_text(size=60,hjust = 0.5))




##痛み測定値のプロット

pain.max=max(data_diff_sum$pain_avg)
pain.min=min(data_diff_sum$pain_avg)

y1.lim <- c(ceiling(pain.min*1.1), ceiling(pain.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_diff_sum,aes(x=delay,y=pain_avg,fill=stimu_pos))

p_2<-p_1+
  labs(x="Delay",y="Pain(cwc)",title = "Pain")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler,
                      name="\nProbability of \"Synchronous\" response\n"), 
                    )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.8))+
  geom_jitter(size=4,position = position_dodge(width = 0.8))+
  #stat_summary(fun=mean,geom="point",size=5,shape=16,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_pain<-p_3+
  #geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler,colour=stimu_pos),size=4,data = data_passive_prob_abs)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  # labs(x="\ndelay\n", y="\nPain intensity(cwc)\n", color = "",
  #      title='\nPain\n', 
  #      subtitle='')+
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)

p_pain

file_name="result/passive_all/plot_passive_pain.png"
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 20,height = 13)


##不快感測定値のプロット
unp.max=max(data_diff_sum$unp_avg)
unp.min=min(data_diff_sum$unp_avg)

y1.lim <- c(ceiling(unp.min*1.1), ceiling(unp.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_diff_sum,aes(x=delay,y=unp_avg,fill=stimu_pos))

p_2<-p_1+
  labs(x="Delay(s)",y="Pain unpleassantness(cwc)",title = "Unpleassantness")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler,
                                        name="\nProbability of \"Synchronous\" response\n"), 
  )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.8))+
  geom_jitter(size=4,position = position_dodge(width = 0.8))+
  #stat_summary(fun=mean,geom="point",size=5,shape=16,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_unp<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler,colour=stimu_pos),size=4,data = data_passive_prob)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  # labs(x="\ndelay\n", y="\nPain intensity(cwc)\n", color = "",
  #      title='\nPain\n', 
  #      subtitle='')+
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)



p_unp

file_name="result/passive_all/plot_passive_unp.png"
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 20,height = 13)



#embodimentの測定値

Q1.max=max(data_diff_sum$Q1_avg)
Q1.min=min(data_diff_sum$Q1_avg)

y1.lim <- c(ceiling(Q1.min*1.1), ceiling(Q1.max*1.1))
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_diff_sum,aes(x=delay_abs,y=Q1_avg,fill=stimu_pos))

p_2<-p_1+
  labs(x="Delay(s)",y="Ownership(cwc)",title = "Ownership")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler,
                                        name="\nProbability of \"Synchronous\" response\n"), 
  )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.8,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.8))+
  geom_jitter(size=4,position = position_dodge(width = 0.8))+
  #stat_summary(fun=mean,geom="point",size=5,shape=16,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 0,col="black")

p_Q1<-p_3+
  geom_line(aes(x=as.numeric(delay),y=simul_prob*scaler,colour=stimu_pos),size=4,data = data_passive_prob_abs)+
  # geom_point(aes(x=delay,y=simul_prob*scaler),col="black")+
  # labs(x="\ndelay\n", y="\nPain intensity(cwc)\n", color = "",
  #      title='\nPain\n', 
  #      subtitle='')+
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)

p_Q1

file_name="result/passive_all/plot_passive_Q1.png"
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 20,height = 13)







#Q1embodimentの測定値2 barplot


p_1<-ggplot()

p_2<-p_1+
  
  scale_y_continuous(limits = c(0,120))+
  labs(title = "Embodiment",x="Delay(s)",y="Embodiment(VAS)")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_diff_sum_sum,aes(x=delay,y=Q1_sum,fill=stimu_pos),stat='identity',width=0.8,position = position_dodge(0.8))+
  geom_jitter(data = data_diff_sum,aes(x=delay,y=Q1_avg2,group=stimu_pos),size=4,position = position_dodge(width = 0.8))+
  geom_errorbar(data=data_diff_sum_sum,aes(x=delay,ymin=Q1_sum-Q1_sd,ymax=Q1_sum+Q1_sd,group=stimu_pos,width=0.5),position = position_dodge(0.8))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q1_2<-p_3+
  
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)


p_Q1_2

file_name="result/passive_all/plot_passive_Q1_2.png"
ggsave(file = file_name, plot = p_Q1_2, dpi = 100, width = 20,height = 13)



#Q2の測定値2 barplot


p_1<-ggplot()

p_2<-p_1+
  labs(x="Delay(s)",y="Causality of visuo-tactile stimuli(VAS)",title = "Causality(visuo-tactile)")+
  scale_y_continuous(limits = c(0,120))+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_diff_sum_sum,aes(x=delay,y=Q2_sum,fill=stimu_pos),stat='identity',width=0.8,position = position_dodge(0.8))+
  geom_jitter(data = data_diff_sum,aes(x=delay,y=Q2_avg,group=stimu_pos),size=4,position = position_dodge(width = 0.8))+
  geom_errorbar(data=data_diff_sum_sum,aes(x=delay,ymin=Q2_sum-Q2_sd,ymax=Q2_sum+Q2_sd,group=stimu_pos,width=0.5),position = position_dodge(0.8))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q2<-p_3+
  
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)


p_Q2

file_name="result/passive_all/plot_passive_Q2.png"
ggsave(file = file_name, plot = p_Q2, dpi = 100, width = 20,height = 13)

#Q3の測定値2 barplot


p_1<-ggplot()

p_2<-p_1+
  labs(x="Delay(s)",y="Control-1(VAS)",title = "Control-1")+
  scale_y_continuous(limits = c(-20,100))+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_diff_sum_sum,aes(x=delay,y=Q3_sum,fill=stimu_pos),stat='identity',width=0.8,position = position_dodge(0.8))+
  geom_jitter(data = data_diff_sum,aes(x=delay,y=Q3_avg,group=stimu_pos),size=4,position = position_dodge(width = 0.8))+
  geom_errorbar(data=data_diff_sum_sum,aes(x=delay,ymin=Q3_sum-Q3_sd,ymax=Q3_sum+Q3_sd,group=stimu_pos,width=0.5),position = position_dodge(0.8))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q3<-p_3+
  
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)


p_Q3

file_name="result/passive_all/plot_passive_Q3.png"
ggsave(file = file_name, plot = p_Q3, dpi = 100, width = 20,height = 13)


#Q4の測定値2 barplot


p_1<-ggplot()

p_2<-p_1+
  labs(x="Delay(s)",y="Control-2(VAS)",title = "Control-2")+
  scale_y_continuous(limits = c(-20,100))+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_diff_sum_sum,aes(x=delay,y=Q4_sum,fill=stimu_pos),stat='identity',width=0.8,position = position_dodge(0.8))+
  geom_jitter(data = data_diff_sum,aes(x=delay,y=Q4_avg,group=stimu_pos),size=4,position = position_dodge(width = 0.8))+
  geom_errorbar(data=data_diff_sum_sum,aes(x=delay,ymin=Q4_sum-Q4_sd,ymax=Q4_sum+Q4_sd,group=stimu_pos,width=0.5),position = position_dodge(0.8))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q4<-p_3+
  
  template+
  scale_fill_brewer(palette="Set2")+
  guides(colour=FALSE)

p_Q4

file_name="result/passive_all/plot_passive_Q4.png"
ggsave(file = file_name, plot = p_Q4, dpi = 100, width = 20,height = 13)



##plot並べ
