library(mediation)
library(dplyr)
library(lme4)
library(lmerTest)

detach("package:lmerTest", unload = T)


data_load_pulse("all",load_multi_file = TRUE)
data<-data%>%
  mutate(stimu_pos=factor(if_else(stimu_pos==1,true = "wrist",false = "fore_arm"),levels = c("wrist","fore_arm")),
         simultaneity=factor(if_else(simultaneity==1,true="simul",
                                     false = "non_simul"),levels = c("simul","non_simul")))



df=data_test2


# step1: The total effect (IV -> DV) # not always necessary to be significant
fit.totaleffect=lmer(rating_pain~stimu_pos*simultaneity_n + (1|ID),
                     data = df,
                     REML = FALSE,
                     na.action = na.omit)
summary(fit.totaleffect)

# step2: The effect of the IV onto the mediator (IV -> M) # always  necessary to be sig.
fit.mediator=lmer(Q1~simultaneity_n*stimu_pos + (1|ID),
                  data = df,
                  REML = FALSE,
                  na.action = na.omit)
summary(fit.mediator)

# step3: The effect of the mediator on the DV (m -> DV)
# while controlling the effect of IV on DV
fit.dv=lmer(rating_pain~simultaneity_n*stimu_pos + Q1 + (1|ID),
            data = df,
            REML = FALSE,
            na.action = na.omit)
summary(fit.dv)


# step4-1: Causal mediation analysis: iv is simultaneity
# only if lmerTest is unloaded
if (class(fit.mediator) == "lmerMod"){
  
  results.simul = mediate(fit.mediator, fit.dv, 
                          treat='simultaneity_n', 
                          mediator='Q1', 
                          boot = F)
  summary(results.simul)
}

if (class(fit.mediator) == "lmerMod"){
  
  results.simul = mediate(fit.mediator, fit.dv, 
                          treat='simultaneity', 
                          control.value = "simul",
                          treat.value = "non_simul",
                          mediator='Q1', 
                          boot = F)
  summary(results.simul)
}



# step4-2: Causal mediation analysis: iv is position
# only if lmerTest is unloaded
if (class(fit.mediator) == "lmerMod"){
  
  results.pos = mediate(fit.mediator, fit.dv, 
                        treat='stimu_pos', 
                        mediator='Q1', 
                        control.value = "wrist",
                        treat.value = "fore_arm",
                        boot = F)
  summary(results.pos)
}