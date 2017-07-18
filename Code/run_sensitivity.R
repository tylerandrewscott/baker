rm(list=ls())
#drop_list = c('Order Verbs',)
library(lubridate)
library(statnet)
library(btergm)
library(tidyverse)
library(broom)

load('Scratch/temp_btergm_results.RData')
gwd_sim_reps= 1000
Rnum_sim = 100
gwd_seq = seq(0.25,3,0.25)
gwd_combos = expand.grid(gwd_seq,gwd_seq) %>% 
  rename(gwod_shape = Var1,gwesp_shape=Var2)

library(parallel)
#gwd_sim_reps=3
#gwod_decay_sim <- runif(gwd_sim_reps,0.05,3)
#gwid_decay_sim <- runif(gwd_sim_reps,0.05,3)
#gwesp_decay_sim <- runif(gwd_sim_reps,0.05,3)

gwod_runif_results = gwesp_runif_results = list()

test_form = net_list[-1] ~ edges  + mutual + isolates + 
  nodecov('Meetings_Attended') +
  gwidegree(gwid_decay,fixed=T) + 
  gwesp(gwesp_decay,fixed=T) + 
  timecov(transform = function(t) t)+
  nodeofactor('High_Resource') + 
  nodeocov('Page_Rank_Prior') + 
  nodeocov('High_Resource_x_Page_Rank_Prior')
library(parallel)


# Initiate cluster
gwid_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)
library(pbapply)

form_test = "net_list[-1] ~ edges  + mutual + isolates+ 
         nodecov('Meetings_Attended') +
         gwidegree(gwid_decay_sim,fixed=T) + 
         gwesp(gwesp_decay_sim,fixed=T) + 
         nodeofactor('Mandatory') + edgecov(utility_list_odegree) + 
         timecov(transform = function(t) t)"

gwid_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)

gwd_sim_results = pblapply(1:1000,function(x) try(tidy(btergm(as.formula(
  gsub('gwesp_decay_sim',as.character(gwesp_decay_sim[x]),
       gsub('gwid_decay_sim',as.character(gwid_decay_sim[x]),form_test))),
  R=100,verbose=F))))

save.image('Scratch/temp_btergm_gw_results.RData',compress = TRUE,safe=TRUE)

