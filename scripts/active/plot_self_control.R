
data_load_pulse_self("all_self")
file_pass=paste("result/active_all/","/",sep="")
#テスト試行のみの全体平均で中心化
data_sum<-data_self%>%
  filter(mode=="test")%>%
  group_by(bin_delay,ID)%>%
  summarise(Q4_avg=mean(Q4),Q5_avg=mean(Q5),Q4_sd=sd(Q4),Q5_sd=sd(Q5))%>%
  ungroup()

data_sum_sum<-data_sum%>%
  group_by(bin_delay)%>%
  summarise(Q4_avg_avg=mean(Q4_avg),Q5_avg_avg=mean(Q5_avg),Q4_sd=sd(Q4_avg),Q5_sd=sd(Q5_avg))%>%
  ungroup()

##カラーパレットの色取得



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


#control-1のプロット


p_1<-ggplot()

p_2<-p_1+
  labs(x="Delay(ms)",y="Control-1(VAS)",title = "Control-1")+
  scale_y_continuous(limits = c(-20,100))+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_sum_sum,aes(x=bin_delay,y=Q4_avg_avg),fill="#77d9a8",stat='identity',width=0.8)+
  geom_jitter(data = data_sum,aes(x=bin_delay,y=Q4_avg),size=2)+
  geom_errorbar(data=data_sum_sum,aes(x=bin_delay,ymin=Q4_avg_avg-Q4_sd,ymax=Q4_avg_avg+Q4_sd,width=0.2))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q4<-p_3+
  
  template+
  guides(colour=FALSE,fill=FALSE)

p_Q4

file_name="result/active_all/plot_active_Q4.png"
ggsave(file = file_name, plot = p_Q4, dpi = 100, width = 20,height = 13)



#control-2のプロット


p_1<-ggplot()

p_2<-p_1+
  labs(x="Delay(ms)",y="Control-2(VAS)",title = "Control-2")+
  scale_y_continuous(limits = c(-20,100))+
  theme_classic()

p_3<-p_2+
  geom_bar(data=data_sum_sum,aes(x=bin_delay,y=Q5_avg_avg),fill="#ffca80",stat='identity',width=0.8)+
  geom_jitter(data = data_sum,aes(x=bin_delay,y=Q5_avg),size=2)+
  geom_errorbar(data=data_sum_sum,aes(x=bin_delay,ymin=Q5_avg_avg-Q5_sd,ymax=Q5_avg_avg+Q5_sd,width=0.2))+
  geom_hline(linetype="dashed",yintercept = 50,col="black")

p_Q5<-p_3+
  
  template+
  guides(colour=FALSE,fill=FALSE)

p_Q5

file_name="result/active_all/plot_active_Q5.png"
ggsave(file = file_name, plot = p_Q5, dpi = 100, width = 20,height = 13)

