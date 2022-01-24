#trainデータのプロット
#trainはrating_painの位置にagencyの測定値が来ている
data_load_adaptation("s501")
data_train<-data_adapt%>%
  filter(mode=="train")%>%
  rename(agency=rating_pain)%>%
  filter(time_diff<3)%>%
  filter(knife_contact_time!=0)%>%
  mutate(arm_delay=factor(arm_delay))

plot_time_diff<-ggplot(data = data_train,aes(x=trial_num,y=time_diff,colour=arm_delay))+
  theme_classic()+
  geom_point(size=5)+
  geom_line(size=2)+
  geom_hline(linetype="dashed",yintercept = 0,color="black")+
  scale_colour_brewer(palette="Set2")+
  scale_y_continuous(limits = c(-0.5,1.5))


plot_time_diff



##agencyのプロット
data_train_quetion<-data_train%>%
  filter(agency!=50)


plot_agency<-ggplot(data=data_train_quetion,aes(trial_num,y=agency,colour=arm_delay))+
  theme_classic()+
  scale_y_continuous(limits = c(0,100))+
  geom_point(size=5)+
  geom_line(size=2)+
  geom_hline(linetype="dashed",yintercept = 50,color="black")

plot_agency


file_pass=paste("result/",data_adapt$ID[1],"/",sep="")
file_name_agency=paste(file_pass,data_adapt$ID[1],"plot_train_ageny.png",sep="")

ggsave(file = file_name_agency, plot = plot_agency, dpi = 100, width = 8.27,height = 11.69)

file_name_time=paste(file_pass,data_adapt$ID[1],"plot_time_diff.png",sep="")

ggsave(file = file_name_time, plot = plot_time_diff, dpi = 100, width = 11.69,height = 8.27)

