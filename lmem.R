data_diff_avg<-data_diff_avg%>%
  mutate(stimu_pos=factor(stimu_pos,levels = c("wrist","fore_arm")),
         simultaneity=factor(simultaneity,levels = c("simul","non_simul")))

data_test<-data%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm"),levels = c("wrist","fore_arm")),
         simultaneity=factor(if_else(simultaneity==1,true="simul",
                                     false = "non_simul"),levels = c("simul","non_simul")))



data_test2<-data%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm"),levels = c("wrist","fore_arm")),
         simultaneity=as.numeric(if_else(simultaneity==1,true=0,
                                     false = 1)))

data_test2<-data_test2%>%
  group_by(ID)%>%
  mutate(simultaneity_n=simultaneity-mean(simultaneity,na.rm=TRUE),
         simul_avg=mean(simultaneity,na.rm=TRUE))






pain.lmem = lmer(rating_pain ~ stimu_pos + simultaneity + stimu_pos:simultaneity + (1|ID),
                 data = data_test,
                 REML = FALSE,
                 na.action = na.omit,
)

pain.lmem_2 = lmer(rating_pain ~ stimu_pos + simultaneity + stimu_pos:simultaneity + (1|ID),
                 data = data_test2,
                 REML = FALSE,
                 na.action = na.omit,
)

pain.lmem_3 = lmer(rating_pain ~ stimu_pos + simultaneity_n + stimu_pos:simultaneity_n + (1|ID),
                   data = data_test2,
                   REML = FALSE,
                   na.action = na.omit,
)

pain.lmem_4 = lmer(rating_pain ~ stimu_pos*simultaneity_n  + (1|ID),
                   data = data_test2,
                   REML = TRUE,
                   na.action = na.omit,
)



#同時性の級内相関係数の計算→およそ0.0647，級内相関があるとはいえない？－－＞級内分散を考慮しなくてもよい 
simul.lmer=lmer(simultaneity~1+(1|ID),
                data=data,
                REML=FALSE,
                na.action = na.omit)
summary(simul.lmer)
ICCest(x=ID,y=rating_pain,data=data)
ICCest(x=ID,y=as.numeric(simultaneity_n),data=data_test2)


summary(pain.lmem)
summary(pain.lmem_2)
summary(pain.lmem_3)
summary(pain.lmem_4)

#95%信頼区間の推定
confint(pain.lmem_4,level = .95,method="Wald")
#vif(分散拡大係数)の計算
require(car)
vif(pain.lmem_4)


(aov<-anova(pain.lmem_4))#type3の平方和を使ったanovaの結果を返す．
r.squaredGLMM(pain.lmem)
write.csv(report(pain.lmem),"test.csv")


pain.lmem = lmer(Q1 ~ stimu_pos + simultaneity + stimu_pos:simultaneity + (1|ID),
                 data = data,
                 REML = TRUE,
                 na.action = na.omit,
)


summary(pain.lmem)


data_test2%>%
  group_by(stimu_pos,simultaneity)%>%
  summarise(pain_avg=mean(rating_pain),Q1_avg=mean(Q1,na.rm=TRUE))

#emobodiment-painの全体プロット

# p.relation<-ggplot(data = data_diff_avg,aes(x=Q1_diff_avg,y=pain_diff_avg,color=stimu_pos:simultaneity))+
#   geom_point(aes(shape=stimu_pos:simultaneity))+
#   stat_smooth(method = "lm",se=FALSE,size=1)+
#   stat_poly_eq(formula = y~x,
#                aes(label=paste(stat(eq.label)
#                                )),
#                parse=TRUE)+
#   stat_poly_eq(formula = y~x,
#                aes(label=paste(stat(rr.label)
#                )),
#                label.x = "right",
#                label.y = "bottom",
#                parse=TRUE)+
#   theme_classic()

p.relation<-ggplot(data = data_diff_avg,aes(x=Q1_diff_avg,y=pain_diff_avg,color=stimu_pos:simultaneity))+
  geom_point(aes(shape=stimu_pos:simultaneity))+
  stat_smooth(method = "lm",se=FALSE,size=1)+
  stat_poly_eq(formula = y~x,
               aes(label=paste(stat(eq.label)
                               )),
               parse=TRUE)+
  stat_poly_eq(formula = y~x,
               aes(label=paste(stat(rr.label)
               )),
               label.x = "right",
               label.y = "bottom",
               parse=TRUE)

  theme(legend.position = "none")

p.relation


cor.test(as.numeric(data_test2$simultaneity_n),as.numeric(data_test2$simultaneity_n)*as.numeric(data_test2$stimu_pos))

独立変数の相関
cor.test(as.numeric(data$simultaneity),as.numeric(data$stimu_pos))

cor_data<-data.frame(stimu_pos=as.numeric(data_test2$stimu_pos),simultaneity=data_test2$simultaneity_n,intr=(data_test2$simultaneity_n)*as.numeric(data_test2$stimu_pos))


vif_test<-lm(simultaneity~stimu_pos+stimu_pos:simultaneity,data=data_test2,na.action = na.omit,)
summary(vif_test)