time_plot<-ggplot()+geom_point(data = data_pos_1,aes(x=time,y=rating_pain))+geom_line(data = data_pos_1,aes(x=time,y=rating_pain,colour=stimu_pos))+
  geom_point(data = data_pos_2,aes(x=time,y=rating_pain))+geom_line(data = data_pos_2,aes(x=time,y=rating_pain,colour=stimu_pos))+
  scale_x_datetime(date_breaks = "5 mins",labels = date_format(format = "%H:%M"))+
  scale_y_continuous(limits = c(0,100))+
  theme_classic()+
  scale_color_manual(values = c(wrist='#F8766D',upper_arm='#00BFC4'))

file_pass=paste("result/",data$ID[1],"/",sep="")
file_name=paste(file_pass,data$ID[1],"time_plot.png",sep="")

  
 ggsave(file = file_name, plot = time_plot, dpi = 100, width = 11.69,height = 8.27)
# 
# data_time_q<-data_question%>%
#   filter(Question=="Q1")
# 
# time_plot_q<-ggplot()+geom_point(data = data_time_q,aes(x=time,y=rating_pain))+geom_line(data = data_time_q,aes(x=time,y=rating_pain,col="pain"))+
#   geom_point(data = data_time_q,aes(x=time,y=rating))+geom_line(data = data_time_q,aes(x=time,y=rating,col="Q1"))+
#   scale_x_datetime(date_breaks = "5 mins",labels = date_format(format = "%H:%M"))+
#   scale_y_continuous(limits = c(0,100))+
#   theme_classic()
# 
# +
#   scale_color_manual(values = c(wrist='#F8766D',upper_arm='#00BFC4'))
# 
