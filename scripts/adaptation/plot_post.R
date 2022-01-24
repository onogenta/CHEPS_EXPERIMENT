#postデータのプロット

# data_post<-data_adapt%>%
#   filter(mode=="test_post")%>%
#   mutate(
#     condition=factor(
#       case_when(
#         (arm_delay!=0&delay==0)~"arm_delay=500,stimu_delay=0",
#         (arm_delay!=0&delay==0.5)~"arm_delay=500,stimu_delay=500",
#         (arm_delay!=0&delay==-0.5)~"arm_delay=500,stimu_delay=-500",
#         arm_delay==0~"arm_delay=0,stimu_delay=-500"
#       ),
#       levels = c("arm_delay=500,stimu_delay=-500","arm_delay=500,stimu_delay=0","arm_delay=500,stimu_delay=500","arm_delay=0,stimu_delay=-500"))
#   )



data_post<-data_adapt%>%
  filter(mode=="test_post")%>%
  filter(time_diff>-3&time_diff<3)%>%
  mutate(bin=cut(time_diff*1000,breaks = c(-10000,-250,250,10000),labels = c("~ -250","-250~250","250~"),right = FALSE))%>%
  mutate(trial_condition=factor(
    if_else(
      condition=="arm_delay=0,stimu_delay=-500",true = "catch",false = "base"
    )))%>%
  mutate(diff_sound_contact=knife_contact_time-sound_end_time)%>%
  mutate(condition=factor(condition,levels = c("arm_delay=500,stimu_delay=-500","arm_delay=500,stimu_delay=0","arm_delay=500,stimu_delay=500","arm_delay=0,stimu_delay=-500"))
  )
  
  
data_post_sum<-data_post%>%
  group_by(condition)%>%
  summarise(pain_avg=mean(rating_pain),pain_sd=sd(rating_pain),
            unp_avg=mean(rating_unp),unp_sd=sd(rating_unp),
            Q1_avg=mean(Q1),Q1_sd=sd(Q1),
            Q2_avg=mean(Q2),Q2_sd=sd(Q2),
            Q3_avg=mean(Q3),Q3_sd=sd(Q3))


data_post_sum_bin<-data_post%>%
  group_by(bin)%>%
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
  geom_bar(data=data_post_sum,aes(x=condition,y=pain_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=condition,y=rating_pain,group=condition,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum,aes(x=condition,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd,group=condition,width=0.2),position = position_dodge(0))+
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
  geom_bar(data=data_post_sum,aes(x=condition,y=unp_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=condition,y=rating_unp,group=condition,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum,aes(x=condition,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd,group=condition,width=0.2),position = position_dodge(0))+
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
  scale_y_continuous(limits = c(-20,120))+
  labs(title = "\nQ1\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum,aes(x=condition,y=Q1_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_post,aes(x=condition,y=Q1,group=condition,shape=simul),size=2,width=0.1)+
  geom_errorbar(data=data_post_sum,aes(x=condition,ymin=Q1_avg-Q1_sd,ymax=Q1_avg+Q1_sd,group=condition,width=0.2),position = position_dodge(0.5))+
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
  scale_y_continuous(limits = c(-20,120))+
  labs(title = "\nagency\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum,aes(x=condition,y=Q2_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_post,aes(x=condition,y=Q2,group=condition,shape=simul),size=2,width=0.1)+
  geom_errorbar(data=data_post_sum,aes(x=condition,ymin=Q2_avg-Q2_sd,ymax=Q2_avg+Q2_sd,group=condition,width=0.2),position = position_dodge(0.5))+
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
  scale_y_continuous(limits = c(-20,120))+
  labs(title = "\ncausality\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum,aes(x=condition,y=Q3_avg,fill=condition),stat='identity',width=0.5,position = position_dodge(0.5))+
  geom_jitter(data = data_post,aes(x=condition,y=Q3,group=condition,shape=simul),size=2,width=0.1)+
  geom_errorbar(data=data_post_sum,aes(x=condition,ymin=Q3_avg-Q3_sd,ymax=Q3_avg+Q3_sd,group=condition,width=0.2),position = position_dodge(0.5))+
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
file_name_pain=paste(file_pass,data_adapt$ID[1],"plot_pain_post.png",sep="")

ggsave(file = file_name_pain, plot = p_pain, dpi = 100, width = 8.27,height = 11.69)


file_name_unp=paste(file_pass,data_adapt$ID[1],"plot_unp_post.png",sep="")
ggsave(file = file_name_unp, plot = p_unp, dpi = 100, width = 8.27,height = 11.69)


file_name_Q1=paste(file_pass,data_adapt$ID[1],"plot_Q1_post.png",sep="")
ggsave(file = file_name_Q1, plot = p_Q1, dpi = 100, width = 8.27,height = 11.69)



file_name_Q2=paste(file_pass,data_adapt$ID[1],"plot_Q2_post.png",sep="")
ggsave(file = file_name_Q2, plot = p_Q2, dpi = 100, width = 8.27,height = 11.69)


file_name_Q3=paste(file_pass,data_adapt$ID[1],"plot_Q3_post.png",sep="")
ggsave(file = file_name_Q3, plot = p_Q3, dpi = 100, width = 8.27,height = 11.69)



plot_time_diff_post<-ggplot(data = data_post,aes(x=trial_num,y=diff_sound_contact))+
  theme_classic()+
  geom_point(size=5,aes(colour=trial_condition,shape=simul))+
  geom_line(size=2)+
  geom_hline(linetype="dashed",yintercept = 0,color="black")+
  scale_colour_brewer(palette="Set2")+
  scale_y_continuous(limits = c(0,1.5))

file_name_time_diff=paste(file_pass,data_adapt$ID[1],"plot_time_diff_post.png",sep="")
ggsave(file = file_name_time_diff, plot = plot_time_diff_post, dpi = 100, width = 11.69,height = 8.27)





#painのプロット(binを使用)

p_1<-ggplot()

p_2<-p_1+
  labs(x="bin",y="pain")+
  scale_y_continuous(limits = c(-1,120))+
  labs(title = "\npain_bin\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum_bin,aes(x=bin,y=pain_avg,fill=bin),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=bin,y=rating_pain,group=bin,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum_bin,aes(x=bin,ymin=pain_avg-pain_sd,ymax=pain_avg+pain_sd,group=bin,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_pain_bin<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))
p_pain_bin


#unpのプロット(binを使用)

p_1<-ggplot()

p_2<-p_1+
  labs(x="bin",y="unp")+
  scale_y_continuous(limits = c(-1,120))+
  labs(title = "\nunp_bin\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum_bin,aes(x=bin,y=unp_avg,fill=bin),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=bin,y=rating_unp,group=bin,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum_bin,aes(x=bin,ymin=unp_avg-unp_sd,ymax=unp_avg+unp_sd,group=bin,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_unp_bin<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_unp_bin

#Q1のプロット(binを使用)

p_1<-ggplot()

p_2<-p_1+
  labs(x="bin",y="Q1")+
  scale_y_continuous(limits = c(-1,120))+
  labs(title = "\nQ1_bin\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum_bin,aes(x=bin,y=Q1_avg,fill=bin),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=bin,y=Q1,group=bin,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum_bin,aes(x=bin,ymin=Q1_avg-Q1_sd,ymax=Q1_avg+Q1_sd,group=bin,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_Q1_bin<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q1_bin

#Q2のプロット(binを使用)

p_1<-ggplot()

p_2<-p_1+
  labs(x="bin",y="Q2")+
  scale_y_continuous(limits = c(-1,120))+
  labs(title = "\nQ2_bin\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum_bin,aes(x=bin,y=Q2_avg,fill=bin),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=bin,y=Q2,group=bin,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum_bin,aes(x=bin,ymin=Q2_avg-Q2_sd,ymax=Q2_avg+Q2_sd,group=bin,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_Q2_bin<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q2_bin

#Q3のプロット(binを使用)

p_1<-ggplot()

p_2<-p_1+
  labs(x="bin",y="Q3")+
  scale_y_continuous(limits = c(-1,120))+
  labs(title = "\nQ3_bin\n")+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_post_sum_bin,aes(x=bin,y=Q3_avg,fill=bin),stat='identity',width=0.5,position = position_dodge(0))+
  geom_jitter(data = data_post,aes(x=bin,y=Q3,group=bin,shape=simul),size=2,width = 0.1)+
  geom_errorbar(data=data_post_sum_bin,aes(x=bin,ymin=Q3_avg-Q3_sd,ymax=Q3_avg+Q3_sd,group=bin,width=0.2),position = position_dodge(0))+
  geom_hline(linetype="dashed",yintercept = 30,col="black")

p_Q3_bin<-p_3+
  
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=10,face = "bold",angle = 45,hjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=30,hjust = 0.5))

p_Q3_bin


file_pass=paste("result/",data_adapt$ID[1],"/",sep="")
file_name_pain=paste(file_pass,data_adapt$ID[1],"plot_pain_post_bin.png",sep="")

ggsave(file = file_name_pain, plot = p_pain_bin, dpi = 100, width = 8.27,height = 11.69)


file_name_unp=paste(file_pass,data_adapt$ID[1],"plot_unp_post_bin.png",sep="")
ggsave(file = file_name_unp, plot = p_unp_bin, dpi = 100, width = 8.27,height = 11.69)


file_name_Q1=paste(file_pass,data_adapt$ID[1],"plot_Q1_post_bin.png",sep="")
ggsave(file = file_name_Q1, plot = p_Q1_bin, dpi = 100, width = 8.27,height = 11.69)



file_name_Q2=paste(file_pass,data_adapt$ID[1],"plot_Q2_post_bin.png",sep="")
ggsave(file = file_name_Q2, plot = p_Q2_bin, dpi = 100, width = 8.27,height = 11.69)


file_name_Q3=paste(file_pass,data_adapt$ID[1],"plot_Q3_post_bin.png",sep="")
ggsave(file = file_name_Q3, plot = p_Q3_bin, dpi = 100, width = 8.27,height = 11.69)











