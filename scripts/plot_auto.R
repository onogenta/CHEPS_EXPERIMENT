
for(i in 8:20){
  number=300+i
  name=paste("s",number,sep = "")
  data_load_pulse(name)
  #source("scripts/plot_matome.R")
  source("scripts/plot_relation_memo.R")
}