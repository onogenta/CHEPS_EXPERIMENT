data_load_pulse("all",load_multi_file = TRUE)
# 
# data_avg<-data%>%
#   group_by(ID,delay,stimu_pos)%>%
#   summarise(pain_avg=mean(rating_pain,na.rm = TRUE),unp_avg=mean(rating_unp,na.rm = TRUE),simul_avg=mean(simultaneity,na.rm=TRUE),Q1_avg=mean(Q1,na.rm = TRUE))%>%
#   ungroup()

data_diff<-data%>%
  group_by(ID)%>%
  mutate(pain_diff=rating_pain-mean(rating_pain,na.rm=TRUE),unp_diff=rating_unp-mean(rating_unp,na.rm=TRUE),Q1_diff=Q1-mean(Q1,na.rm=TRUE))%>%
  ungroup()

data_diff<-data_diff%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm")),simultaneity=factor(if_else(simultaneity==1,true="simul",
                                                                                                false = "non_simul")))

data_diff$stimu_pos<- factor(data_diff$stimu_pos,levels = c("wrist","fore_arm"))


data_diff_avg<-data_diff%>%
  group_by(stimu_pos,simultaneity,ID)%>%
  summarise(pain_avg=mean(rating_pain,na.rm=TRUE),Q1_avg=mean(Q1,na.rm=TRUE),pain_diff_avg=mean(pain_diff,na.rm=TRUE),unp_diff_avg=mean(unp_diff,na.rm=TRUE),Q1_diff_avg=mean(Q1_diff,na.rm=TRUE))%>%
  ungroup()


data_diff_sum<-data_diff%>%
  group_by(delay,stimu_pos,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))%>%
  mutate(delay=factor(delay))

data_diff_sum_pos<-data_diff%>%
  group_by(stimu_pos,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))

data_diff_sum_simul<-data_diff%>%
  group_by(simultaneity,ID)%>%
  summarise(pain_avg=mean(pain_diff,na.rm=TRUE),unp_avg=mean(unp_diff,na.rm=TRUE),Q1_avg=mean(Q1_diff,na.rm=TRUE),pain_sd=sd(pain_diff,na.rm=TRUE),unp_sd=sd(unp_diff,na.rm=TRUE),Q1_sd=sd(Q1_diff,na.rm=TRUE))


#正規性の検定　シャピロテスト

# qqnorm(data_diff_avg$pain_diff_avg,ylab="pain")
# qqline(data_diff_avg$pain_diff_avg)
# shapiro.test(x=data_diff_avg[data_diff_avg$stimu_pos=="wrist"&data_diff_avg$simultaneity=="simul",]$pain_diff_avg)
# shapiro.test(x=data_diff_avg$pain_diff_avg)

data_diff_avg%>%
  group_by(stimu_pos,simultaneity)%>%
  shapiro_test(pain_diff_avg)

ggqqplot(data_diff_avg, "pain_diff_avg", ggtheme = theme_bw()) +
  facet_grid(simultaneity ~ stimu_pos, labeller = "label_both")


#outlier check
data_diff_avg%>%
  group_by(stimu_pos,simultaneity)%>%
  identify_outliers(pain_diff_avg)

#対応のある二元配置分散分析
res.aov<-anova_test(
  data=data_diff_avg,dv=pain_diff_avg,wid = ID,
  within = c(simultaneity,stimu_pos)
)

get_anova_table(res.aov)

#事後テス
tukey_hsd(data_diff_avg,pain_diff_avg~stimu_pos*simultaneity)



# summary(aov(pain_diff_avg~simultaneity*stimu_pos+Error(ID*ID:simultaneity*ID:stimu_pos*ID:simultaneity:stimu_pos),data = data_diff_avg))
# summary(aov(Q1_diff_avg~simultaneity*stimu_pos,data=data_diff_avg))
