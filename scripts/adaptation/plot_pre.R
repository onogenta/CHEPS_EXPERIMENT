#preデータのプロット

data_pre<-data_adapt%>%
  filter(mode=="test_pre")%>%
  mutate(
    condition=factor(
      case_when(
        (arm_delay==0&delay==0)~"arm_delay=0,stimu_delay=0",
        (arm_delay==0&delay==0.5)~"arm_delay=0,stimu_delay=500",
        (arm_delay==0&delay==-0.5)~"arm_delay=0,stimu_delay=-500",
        arm_delay!=0~"arm_delay=500,stimu_delay=0"
      ),
    levels = c("arm_delay=0,stimu_delay=-500","arm_delay=0,stimu_delay=0","arm_delay=0,stimu_delay=500","arm_delay=500,stimu_delay=0"))
  )

data_pre_sum<-data_pre%>%
  group_by(condition)%>%
  summarise(pain_avg=mean(rating_pain),pain_sd=sd(rating_pain),
            unp_avg=mean(rating_unp),unp_sd=sd(rating_unp),
            Q1_avg=mean(Q1),Q1_sd=sd(Q1),
            Q2_avg=mean(Q2),Q2_sd=sd(Q2),
            Q3_avg=mean(Q3),Q3_sd=sd(Q3))
##痛みのプロット

p_1<-ggplot()

p_2<-p_1+
  labs(x="condition",y="pain")+
  scale_y_continuous(limits = c(0,120))+
  labs(title = "\npain\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_pre_sum,aes(x=condition,y=pain_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_pre,aes(x=condition,y=rating_pain,group=condition),size=2,width = 0.1)+
  geom_errorbar(data=data_pre_sum,aes(x=condition,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd,group=condition,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_pain<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_pain




##不快間のプロット
p_1<-ggplot()

p_2<-p_1+
  labs(x="condition",y="unp")+
  scale_y_continuous(limits = c(0,120))+
  labs(title = "\nunp\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_pre_sum,aes(x=condition,y=unp_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_pre,aes(x=condition,y=rating_unp,group=condition),size=2,width = 0.1)+
  geom_errorbar(data=data_pre_sum,aes(x=condition,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd,group=condition,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_unp<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_unp


##Q1のプロット
p_1<-ggplot()

p_2<-p_1+
  labs(x="condition",y="embodiment")+
  scale_y_continuous(limits = c(0,120))+
  labs(title = "\nQ1\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_pre_sum,aes(x=condition,y=Q1_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_pre,aes(x=condition,y=Q1,group=condition),size=2,width=0.1)+
  geom_errorbar(data=data_pre_sum,aes(x=condition,ymin=Q1_avg-Q1_sd,ymax=Q1_avg+Q1_sd,group=condition,width=0.2),position = position_dodge(0.5))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q1<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q1

##Q2のぷロット
p_1<-ggplot()

p_2<-p_1+
  labs(x="condition",y="Q2")+
  scale_y_continuous(limits = c(0,120))+
  labs(title = "\nagency\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_pre_sum,aes(x=condition,y=Q2_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_pre,aes(x=condition,y=Q2,group=condition),size=2,width=0.1)+
  geom_errorbar(data=data_pre_sum,aes(x=condition,ymin=Q2_avg-Q2_sd,ymax=Q2_avg+Q2_sd,group=condition,width=0.2),position = position_dodge(0.5))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q2<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q2


##Q3のプロット
p_1<-ggplot()

p_2<-p_1+
  labs(x="condition",y="Q3")+
  scale_y_continuous(limits = c(0,120))+
  labs(title = "\ncausality\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_pre_sum,aes(x=condition,y=Q3_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_pre,aes(x=condition,y=Q3,group=condition),size=2,width=0.1)+
  geom_errorbar(data=data_pre_sum,aes(x=condition,ymin=Q3_avg-Q3_sd,ymax=Q3_avg+Q3_sd,group=condition,width=0.2),position = position_dodge(0.5))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q3<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q3


##ファイルセーブ
file_pass=paste("result/",data_adapt$ID[1],"/",sep="")
file_name_pain=paste(file_pass,data_adapt$ID[1],"plot_pain_pre.png",sep="")

ggsave(file = file_name_pain, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)


file_name_unp=paste(file_pass,data_adapt$ID[1],"plot_unp_pre.png",sep="")
ggsave(file = file_name_unp, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)


file_name_Q1=paste(file_pass,data_adapt$ID[1],"plot_Q1_pre.png",sep="")
ggsave(file = file_name_Q1, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)



file_name_Q2=paste(file_pass,data_adapt$ID[1],"plot_Q2_pre.png",sep="")
ggsave(file = file_name_Q2, plot = p_Q2, dpi = 100, width = 8.27,height = 11.69)


file_name_Q3=paste(file_pass,data_adapt$ID[1],"plot_Q3_pre.png",sep="")
ggsave(file = file_name_Q3, plot = p_Q3, dpi = 100, width = 8.27,height = 11.69)



