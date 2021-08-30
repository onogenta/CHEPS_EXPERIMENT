time_plot<-ggplot()+geom_point(data = data_pos_1,aes(x=time,y=rating_pain))+geom_line(data = data_pos_1,aes(x=time,y=rating_pain,colour=stimu_pos))+
  geom_point(data = data_pos_2,aes(x=time,y=rating_pain))+geom_line(data = data_pos_2,aes(x=time,y=rating_pain,colour=stimu_pos))+
  scale_x_datetime(date_breaks = "5 mins",labels = date_format(format = "%H:%M"))+
  scale_y_continuous(limits = c(0,100))+
  theme_classic()+
  scale_color_manual(values = c(wrist='#F8766D',upper_arm='#00BFC4'))



  
ggsave(file = "time_plot.png", plot = time_plot, dpi = 100, width = 11.69,height = 8.27)

