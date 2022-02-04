#diagram plot
library(diagram)
data <- c(0, "'-12.49***'", 0,
          0, 0, 0, 
          "'0.11***'", "'-1.69*** (-0.31)'", 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
diagram<- plotmat (M, pos=c(1,2), 
                name= c( "Embodiment","Position", "Pain"), 
                box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)


plot_diagram<-function(file_name,tre_name,med_name,out_name,tre_med_effect,med_out_effect,tre_out_effect,direct_effect,
                       tre_med_sig,med_out_sig,tre_out_sig,direct_sig){
  tre_med<-paste("'",tre_med_effect,tre_med_sig,"'",sep = "")
  med_out<-paste("'",med_out_effect,med_out_sig,"'",sep="")
  tre_out<-paste(tre_out_effect,tre_out_sig,sep="")
  direct<-paste(direct_effect,direct_sig,sep="")
  
  t_out<-paste("'",tre_out," (",direct,")","'")
  
  data <- c(0, tre_med, 0,
            0, 0, 0, 
            med_out, t_out, 0)
  M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
  plot_1<- plotmat (M, pos=c(1,2), 
                  name= c(med_name,tre_name,out_name), 
                  box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)
  
  # name<-paste("result/",file_name,sep ="")
  # 
  # png(filename = name)
  # plot_1
  # 
  # dev.off()
  
}

##passiveのダイアグラム

plot_diagram("passive_pos_emb_pain.png","Position","Embodiment","Pain","-12.49","0.10","-1.69","-0.41","***","***","**","")
plot_diagram("passive_pos_emb_pain.png","Delay_Abs","Embodiment","Pain","-19.13","0.10","-3.72","-1.74","***","***","*","")
plot_diagram("passive_pos_emb_pain.png","Delay_Abs","Simultaneity","Pain","1.14","-1.33","-3.72","-2.19","***","†","*","")
plot_diagram("passive_pos_emb_pain.png","Delay_Abs","Simultaneity","Embodiment","1.14","-9.49","-19.13","-8.43","**","***","***","***")
plot_diagram("passive_pos_emb_pain.png","Delay_Abs","Embodiment","Simultaneity","19.13","-5.73e-3","1.14","1.03","***","***","***","***")

##activeのダイアグラム
plot_diagram("passive_pos_emb_pain.png","Delay","Causality","Pain","7.39","0.10","2.00","1.25","***","***","***","***")
plot_diagram("passive_pos_emb_pain.png","Delay","Causality","Unpleasantness","7.39","0.12","1.24","0.37","***","***","***","")

