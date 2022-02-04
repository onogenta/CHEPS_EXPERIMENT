library(dplyr)#passive実験の図示と解析で使用するパッケージをまとめて読み込み，不要な奴あり
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

data_load_pulse<-function(file_name,load_multi_file=FALSE){ #実験データ読み込み用関数の定義
  file_path=paste("raw_data/",file_name,sep="")             #実験データの被験者毎のファイルが置いてある場所を設定
  
  data = list.files(path = file_path, full.names = T) %>%   #データの読み込みと列名の定義
    map(read.csv, col.names=c("time",　　　　　　　　　　　 #データを記録した時刻
                              "ID",                         #被験者ID
                              "stimu_pos",                  #刺激部位0→整合位置，1→不整合位置
                              "trial_num",                  #トライアル番号
                              "delay",                      #導入したdelay
                              "delay_num",　　　　　　　　　#そのdelayの出現回数　　
                              "rating_pain",　　　　　　　　#痛み回答値
                              "rating_unp",                 #不快感の回答値　
                               "simultaneity",              #同時性の回答値
                              "Q1","Q2","Q3","Q4","Q5"),　　#その他アンケートの回答値
        colClasses = "character", header = F, sep = ",",na.strings="na")%>%
    reduce(rbind)%>%　　　　　　　　　　　　　　　　　　　　#複数ファイルの結合
    mutate(time=as.character(time),　　　　　　　　　　　　 #各列の型を定義
           ID=factor(ID),
           stimu_pos=factor(stimu_pos),
           trial_num=as.integer(trial_num),
           delay=as.numeric(delay),
           delay_num=factor(delay_num),
           delay_abs=factor(abs(delay)),
           rating_pain=as.numeric(rating_pain),
           rating_unp=as.numeric(rating_unp),
           simultaneity=as.numeric(simultaneity),
           Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4))%>%
    
    mutate(time=as.POSIXct(time,format="%H:%M"))#timeはプロット用に文字型から時系列型に変換
  
  
  
  if(load_multi_file==FALSE){#被験者全体のファイルを読み込まないloda_mult_file=FALSEの場合は，demo(delay=0ms で10回)のデータと本番データを別々の変数に保存
    data_demo<-data%>%
      filter(ID!=file_name)
    
    data<-data%>%
      filter(ID==file_name)
    
    assign("data_demo",data_demo,env=.GlobalEnv)
    
  }
 
 

  assign("data",data,env=.GlobalEnv)#読み込んだ実験データを加工したリスト型のデータを変数名「data」でグローバル環境の変数に設定
  
  
  
}


#,Q1=V10,Q2=V11,Q3=V12,Q4=V13
#,Q1=as.numeric(Q1),Q2=as.numeric(Q2),Q3=as.numeric(Q3),Q4=as.numeric(Q4)