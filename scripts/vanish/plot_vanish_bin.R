#実験データのプロット(個人)
data_vanish<-data_vanish%>%　#練習データを除外
  filter(mode=="test_vanish")



data_sum<-data_vanish%>%
  filter(mode=="test_vanish")%>%
  group_by(bin,vanish)%>%　　　　#binとナイフ消失条件でグループ化
  summarise(pain_avg=mean(rating_pain),
            unp_avg=mean(rating_unp),
            simul_prob=mean(simultaneity),
            Q1_avg=mean(Q1),
            Q2_avg=mean(Q2),
            Q3_avg=mean(Q3),
            pain_sd=sd(rating_pain),
            unp_sd=sd(rating_unp),
            Q1_sd=sd(Q1),
            Q2_sd=sd(Q2),
            Q3_sd=sd(Q3))%>%
  ungroup()

#プロットの書式テンプレートの定義
template<-theme(axis.title.x = element_text(size = 40),
                axis.text.x = element_text(size=30,face = "bold"),
                axis.title.y = element_text(size = 40),
                axis.text.y = element_text(size=40),
                legend.text = element_text(size=40),
                legend.title = element_text(size = 30,face="bold"),
                legend.position=c(0,1),
                legend.justification = c(-0.3,1),
                plot.title = element_text(size=60,hjust = 0.5))


#各測定値の2軸プロットの描画　※1軸目：アンケートの回答値，2軸目：同時性の回答値
#痛み回答値のみ2軸でプロットする

y1.lim <- c(0,100)　#1軸目の範囲
y2.lim <- c(0, 1)　 #2軸目の範囲　

scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])　#1軸目と2軸目のスケール比

p_1<-ggplot(data=data_vanish,aes(x=bin,y=rating_pain,fill=vanish))#ggplotのx，y軸とグループ分けの定義

p_2<-p_1+
  labs(x="bin(delay(ms))",y="Pain",title="Pain")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0), 
                     sec.axis=sec_axis( ~ ./scaler, 　　　　#2軸目の追加
                                        name="\nProbability of \"Synchronous\" response\n"
                     ))+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+　#箱ひげ図のプロット
  geom_jitter(size=3,position = position_dodge(width = 0.5))+　　　　　　　　　　　　　　　　　　　　　　　　　　　　#生データの追加
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")　　　　　　　　　　　　　　　　　　　　　　　　　　　　　#基準線の追加(痛みの場合は値=30の点)

p_pain<-p_3+
  geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler,colour=vanish),size=4,data = data_sum)+　　　　　　　　　　　　#同時性の回答確率の線プロット
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template


#Embodiment

y1.lim <- c(0,100)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_vanish,aes(x=bin,y=Q1,fill=vanish))

p_2<-p_1+
  labs(x="bin(delay(ms))",y="Embodiment",title="Embodiment")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0),
                     # sec.axis=sec_axis( ~ ./scaler,
                     #                    name="\nProbability of \"Synchronous\" response\n"
                     # )
                     )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  geom_jitter(size=3,position = position_dodge(width = 0.5))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q1<-p_3+
  #geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler,colour=vanish),size=4,data = data_sum)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template


#Agency

y1.lim <- c(0,100)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_vanish,aes(x=bin,y=Q2,fill=vanish))

p_2<-p_1+
  labs(x="bin(delay(ms))",y="Agency",title="Agency(leftarm)")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0),
                     # sec.axis=sec_axis( ~ ./scaler,
                     #                    name="\nProbability of \"Synchronous\" response\n"
                     # )
  )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  geom_jitter(size=3,position = position_dodge(width = 0.5))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q2<-p_3+
  #geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler,colour=vanish),size=4,data = data_sum)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template

#Causality

y1.lim <- c(0,100)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_vanish,aes(x=bin,y=Q3,fill=vanish))

p_2<-p_1+
  labs(x="bin(delay(ms))",y="Causality",title="Causality")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0),
                     # sec.axis=sec_axis( ~ ./scaler,
                     #                    name="\nProbability of \"Synchronous\" response\n"
                     # )
  )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  geom_jitter(size=3,position = position_dodge(width = 0.5))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q3<-p_3+
  #geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler,colour=vanish),size=4,data = data_sum)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template

#Unp

y1.lim <- c(0,100)
y2.lim <- c(-1, 1.2)
scaler <- (y1.lim[2] - y1.lim[1])/(y2.lim[2] - y2.lim[1])

p_1<-ggplot(data=data_vanish,aes(x=bin,y=rating_unp,fill=vanish))

p_2<-p_1+
  labs(x="bin(delay(ms))",y="Unpleasantness",title="Unpleasantness")+
  
  scale_y_continuous(limit=y1.lim, expand = c(0.1, 0),
                     # sec.axis=sec_axis( ~ ./scaler,
                     #                    name="\nProbability of \"Synchronous\" response\n"
                     # )
  )+
  theme_classic()

p_3<-p_2+
  geom_boxplot(width=0.5,outlier.colour = "red",outlier.size = 1,outlier.shape = 8,position = position_dodge(0.5))+
  geom_jitter(size=3,position = position_dodge(width = 0.5))+
  #stat_summary(fun=mean,geom="point",size=6,shape=4,col="white",position = position_dodge(width = 1))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_unp<-p_3+
  #geom_line(aes(x=as.numeric(bin),y=simul_prob*scaler,colour=vanish),size=4,data = data_sum)+
  # geom_point(aes(x=Delay(ms),y=simul_prob*scaler),col="black")+
  template



file_pass=paste("result/",data_vanish$ID[1],"/",sep="")　#保存先のパスを設定，data_vanishのIDと同じ名前のファイルへ


file_name=paste(file_pass,data_vanish$ID[1],"plot_vanish_pain.png",sep="")
ggsave(file = file_name, plot = p_pain, dpi = 100, width = 20,height = 13)

file_name=paste(file_pass,data_vanish$ID[1],"plot_vanish_emb.png",sep="")
ggsave(file = file_name, plot = p_Q1, dpi = 100, width = 20,height = 13)

file_name=paste(file_pass,data_vanish$ID[1],"plot_vanish_agc.png",sep="")
ggsave(file = file_name, plot = p_Q2, dpi = 100, width = 20,height = 13)

file_name=paste(file_pass,data_vanish$ID[1],"plot_vanish_cas.png",sep="")
ggsave(file = file_name, plot = p_Q3, dpi = 100, width = 20,height = 13)

file_name=paste(file_pass,data_vanish$ID[1],"plot_vanish_unp.png",sep="")
ggsave(file = file_name, plot = p_unp, dpi = 100, width = 20,height = 13)


plot_time<-ggplot(data=data_vanish,aes(x=trial_num,y=time_diff))+
  geom_point()+geom_line()+
  scale_y_continuous(limits = c(-2,2))
plot_time

