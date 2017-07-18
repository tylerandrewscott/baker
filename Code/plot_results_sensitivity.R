rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)
library(knitr)
library(texreg)
library(forcats)
load('Scratch/temp_btergm_gw_results.RData')

temp = gwd_sim_results
blank_replace = as.data.frame(temp[[1]] %>% mutate(estimate=NA,conf.low=NA,conf.high=NA))
for (i in 1:length(temp))
{
if (class(temp[[i]]) == 'try-error')
{temp[[i]] = blank_replace}
}

temp = lapply(1:length(temp),function(x) temp[[x]] %>% mutate(model = x) %>%
               mutate(term = gsub('\\.[0-9].*','',term)))

temp = do.call(rbind,temp) %>% left_join(.,data.frame(gwid_decay_sim,gwesp_decay_sim,
                                                      model = 1:length(gwd_sim_results)))

temp = temp %>% mutate(converge = ifelse(is.na(estimate),0,1))

library(viridis)
library(ggthemes)

model_master = as.data.frame(m1@coef) %>% mutate(term = rownames(.)) %>% 
  mutate(term = gsub('\\.[0-9].*','',term))

temp = left_join(temp,model_master)

sd_by_term = temp %>% group_by(term) %>% summarise(sd_term = sd(estimate,na.rm=T))

temp = left_join(temp,sd_by_term) %>% mutate(diff = estimate-`m1@coef`) %>% 
  mutate(diff_div_sd = diff / sd_term)

temp = temp%>%mutate(term = as.factor(term))

temp$term = fct_recode(temp$term,
                              `Isolates` = 'isolates',
                              `Edges` = "edges" ,
                              `GWESP` = "gwesp.fixed"    ,                        
                              `GWID` = "gwidegree"      ,                          
                              `Mutual` = "mutual"     ,     
                              `Meetings attended` = "nodecov.Meetings_Attended",
                              `Mandatory (sender)` = "nodeofactor.Mandatory"  ,  
                              `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                              `Time` = "edgecov.timecov1[[i]]")
temp$term = fct_relevel(temp$term,c(
  "Edges"               ,      
  "Mutual"      ,
  "Isolates",
  "GWESP"   ,                        
  "GWID"  ,
  "Meetings attended",                          
  "Mandatory (sender)" ,
  "Utility (sender)",
  "Time"))



ggplot(temp,aes(x=gwid_decay_sim,y=gwesp_decay_sim,shape=as.factor(19),
                colour=diff_div_sd),alpha=0.5) +
  geom_point()+
  scale_color_viridis( option='C',limits=c(-5,5),labels=c('< -5','-2.5','0',"2.5","> 5"),
                       breaks=c(-5,-2.5,0,2.5,5), name = 'SD from\nModel 1')+
#scale_color_viridis( limits=c(-5,5),name='Std.(coef)',breaks=c(-5,-2.5,0,2.5,5),
 #                   labels=c('< -5','-2.5','0',"2.5","> 5"),discrete = F,alpha=0.5,option='C')+
  facet_wrap(~term) + 
  geom_hline(lty=2,yintercept=2) + geom_vline(xintercept=2,lty=2)+
  theme_pander() + 
  scale_x_continuous(name='Fixed GWID decay parameter') + 
  scale_y_continuous(name='Fixed GWESP decay parameter')+
  scale_shape_manual(values=19,name = 'Failed to\n converge',labels='NA') +
  guides(shape = guide_legend(override.aes = list(size = 3,colour='grey60')))



temp = temp %>% mutate(signif = ifelse(is.na(conf.low),'Failed',
                                       ifelse(conf.low<0&conf.high>0,'p >= 0.05','p < 0.05')),
                positive = ifelse(is.na(estimate),'Failed',ifelse(estimate>0,'> 0','< 0')))

ggplot(temp,aes(x=gwid_decay_sim,y=gwesp_decay_sim,shape=signif,
                colour=positive),alpha=0.5) +
  geom_point()+
  scale_color_colorblind(name = 'Sign',labels=c('-','+','Failed \nconvergence'))+
  facet_wrap(~term) + 
  geom_hline(lty=2,yintercept=2,col='grey50',lwd=1) + geom_vline(xintercept=2,lty=2,lwd=1,col='grey50')+
  theme_pander()  +
  scale_x_continuous(name='Fixed GWID decay parameter') + 
  scale_y_continuous(name='Fixed GWESP decay parameter')+
  scale_shape_manual(values=c(17,19,21),name='p < 0.05',labels=c('Failed \nconvergence','Yes','No')) +
  theme(axis.title=element_text(size=16),axis.text=element_text(size=16),strip.text=element_text(size=16))


  ?scale_color_colorblind
  
  guides(shape = guide_legend(override.aes = list(size = 3,colour='grey60')))



  