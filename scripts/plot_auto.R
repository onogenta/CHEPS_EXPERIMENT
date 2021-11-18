
for(i in 31:34){
  number=300+i
  name=paste("s",number,sep = "")
  data_load_pulse(name)
  source("scripts/plot_matome.R")
  #source("scripts/plot_relation_memo.R")
}


for(i in 2:10){
  number=400+i
  name=paste("s",number,sep = "")
  data_load_pulse_self(name)
  source("scripts/plot_self_cond.R")
  source("scripts/plot_self_bin_diff.R")
  source("scripts/plot_self_bin_delay.R")
  #source("scripts/plot_relation_memo.R")
}