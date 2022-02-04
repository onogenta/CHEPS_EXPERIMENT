library(dplyr)#実験データのプロットと解析に必要なパッケージの読み込み
library(purrr)
library(ggplot2)
library(ggforce)
library(ggpubr)
library(tidyr)
library(lubridate)
library(scales)
library(rstatix)
library(lmerTest)
library(lme4)
library(report)
library(MuMIn)
library(ggpmisc)
library(ICC)
library(corrr)

data_load_vanish<-function(file_name){　　　　　　　　　#実験データの読み込み用関数の定義
  file_path=paste("raw_data/",file_name,sep="")　　　　 #データファイルが置いてある場所を設定
  
  data = list.files(path = file_path, full.names = T) %>%
    #列名を1列目から順番に定義
    map(read.csv, col.names=c("time",　　　　　　　#時刻
                              "ID",　　　　　　　　#被験者ID
                              "mode",　　　　　　　#実験モード，vanishだと(train or test_vanish)
                              "trial_num",         #試行回数
                              "stimu_pos",　　　　 #刺激部位(0,1,2がそれぞれ前腕の前・中・後部)　
                              "delay",　　　　　　 #導入したdelayの数値
                              "sound_end_time",　　#電子音の4音目がなったVR環境内の時刻，正常に動作していらば≒4の数値になる
                              "trigger_time",　　　#熱刺激がトリガーした時刻
                              "knife_contact_time",#ナイフがVR身体に接触した時刻　
                              "time_diff",　　　　 #ナイフの接触時刻と熱刺激のトリガー時刻の差　
                              "arm_delay",　　　　 #左腕に導入したdelayの数値
                              "vanish",　　　　　　#ナイフが接触と同時に消えたかどうか
                              "rating_pain",　　　 #痛みの回答値　
                              "rating_unp",　　　　#不快感の回答値
                              "simultaneity",　　　#同時性の回答値
                              "Q1","Q2","Q3","Q4","Q5"　#その他身体性に関する質問の回答値
                              ),
        colClasses = "character", header = F, sep = ",",na.strings="na")%>%
    
    reduce(rbind)%>%　#フォルダ内のcsvファイルを行で結合
    #各列の型の変換
    mutate(time=as.character(time), 
           ID=factor(ID),
           mode=factor(mode),
           stimu_pos=factor(stimu_pos),
           trial_num=as.integer(trial_num),
           delay=as.numeric(delay),
           sound_end_time=as.numeric(sound_end_time),
           trigger_time=as.numeric(trigger_time),
           knife_contact_time=as.numeric(knife_contact_time),
           time_diff=as.numeric(time_diff),
           arm_delay=as.numeric(arm_delay),
           vanish=factor(vanish),
           rating_pain=as.numeric(rating_pain),
           rating_unp=as.numeric(rating_unp),
           simultaneity=as.numeric(simultaneity),
           Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4),Q5=as.numeric(Q5))%>%
    
    #mutate_at("simultaneity",~replace(.,is.na(.),1))%>%
    mutate(time=as.POSIXct(time,format="%H:%M"),#timeは文字型から時刻型へ変換
           simul=factor　　　　　　　　　　　　 #同時性の回答値をfactor型にしてもう1つ用意
           (if_else(simultaneity==1,true="simul",false = "non_simul"))
    )%>%
    mutate(condition=paste("arm_delay=",round(arm_delay,1)*1000,",","stimu_delay=",delay*1000,sep = ""))%>%　#刺激delay，腕delayをまとめた列を追加()
    mutate(bin=cut(delay,breaks=c(-10000,-799,750,10000)*0.001,right = FALSE,　　　　　　　　　　　　　　　　#刺激delayでbin分け[-inf,-799),[-200,200),[750,inf)みたいな範囲
                   labels = c("-1200~-800","-200~200","800~1200")))　　　　　　　　　　　　　　　　　　　　　#binの名前を定義

  
  data$simul<- factor(data$simul,levels = c("simul","non_simul"))　#プロット時の表示順を定義
  
  ##各測定値のcwcの計算，mode=trainの行を省く必要あり
  # data<-data%>%
  #   group_by(ID)%>%
  #   mutate(pain_cwc=rating_pain-mean(rating_pain,na.rm=TRUE),unp_cwc=rating_unp-mean(rating_unp,na.rm=TRUE),
  #          Q1_cwc=Q1-mean(Q1,na.rm=TRUE),Q2_cwc=Q2-mean(Q2,na.rm=TRUE),Q3_cwc=Q3-mean(Q3,na.rm=TRUE))
  
  # data<-data%>%
  #   group_by(ID,mode)%>%
  #   mutate(pain_cwc_mode=rating_pain-mean(rating_pain,na.rm=TRUE),unp_cwc_mode=rating_unp-mean(rating_unp,na.rm=TRUE))
  
  
  
  assign("data_vanish",data,env=.GlobalEnv)　#読み込んだ実験データをデータ名「data_vanish」としてグローバル環境に登録
  
  
}


#,Q1=V10,Q2=V11,Q3=V12,Q4=V13
#,Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4)