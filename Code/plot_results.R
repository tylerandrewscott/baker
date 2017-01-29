library(statnet)
library(btergm)
library(tidyverse)
rm(list=ls())
library(knitr)
library(texreg)
library(stargazer)
load('Scratch/temp_btergm_results.RData')

#lapply(base_mod_list,function(x) sum(rowSums(is.na(x@boot$t))==0))
for (x in 1:length(base_mod_list))
{
  if(any(is.na(base_mod_list[[x]]@boot$t))) {base_mod_list[[x]]@boot$R <- sum(rowSums(is.na(base_mod_list[[x]]@boot$t))==0)} 
}

names(base_mod_list) = c(paste('M',1:length(base_mod_list),sep=''))


lapply(1:length(base_mod_list), function(x) {names(base_mod_list[[x]]@coef) <<- names(base_mod_list[[x]]@coef) %>% as.factor(.) %>% fct_recode(.,c(
  `Dynamic (scoping)` = "edgecov.dynamic_list_prior[[i]]"       ,   
  `Stability (pre-implementation)` = "edgecov.stability_list_prior[[i]]"     ,   
  `Dynamic (post-scoping)` = "edgecov.timecov.dynamic_list_prior[[i]]"  ,
  `Stability (implementation)` = "edgecov.timecov.stability_list_prior[[i]]",
  `Time` = "edgecov.timecov[[i]]",
  `Isolates` = 'isolates',
  `Utility * Time` = "edgecov.timecov.utility_list_odegree[[i]]",
  `Edges` = "edges"                        ,            
  `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
  `GWID (a = 2)` = "gwidegree"      ,                          
  `GWOD (a = 2)` = "gwodegree"     ,                           
  `Mutual` = "mutual"     ,                    
  `Prior core #` = "nodecov.Core_Order_Prior"  ,               
  `High resource (HR)` = "nodecov.High_Resource"    ,                
  `Prior core # * HR` = "nodecov.High_Resource_x_Core_Order_Prior", 
  `Mandatory (out)` = "nodeofactor.Mandatory.1"  ,        
  `Utility (out)` = "nodeofactor.Utility.1"  ))})

htmlreg(base_mod_list,level=0.99)
screenreg(base_mod_list,level=0.99)

library(forcats)
library(broom)

full_results = do.call(rbind,lapply(base_mod_list, function(x) summary(x,level=0.99) %>% tidy(.)))
full_results$model = gsub('\\.[0-9]{1,2}','',rownames(full_results))
names(full_results) = c('term','estimate','conf.low','conf.high','model')

full_results$term = fct_relevel(full_results$term,c(
  "Edges"               ,      
  "Mutual"      ,
  "Isolates",
  "GWESP (a = 2)"   ,                        
  "GWID (a = 2)"       ,                          
  "GWOD (a = 2)"      ,                           
  "Prior core #"  ,               
  "High resource (HR)"   ,                
  "Prior core # * HR" , 
  "Utility (out)"  ,        
  "Mandatory (out)" ,
  "Time",
  "Utility * Time" ,
  "Dynamic (scoping)",   
  "Dynamic (post-scoping)"  ,
  "Stability (pre-implementation)"  , 
  "Stability (implementation)" ))

library(ggthemes)

full_results = full_results %>% mutate(signif = ifelse(conf.low<0&conf.high>0,0,1))


yellow_col = '#E69F00'
blue_col = '#56B4E9'
green_col = '#009E73'


gg2 = ggplot(full_results%>% filter(grepl('M2',model)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M2'])[levels(full_results$term[full_results$model=='M2']) %in% full_results$term[full_results$model=='M2']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 99% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg2

gg3 = ggplot(full_results%>% filter(grepl('M3',model)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M3'])[levels(full_results$term[full_results$model=='M3']) %in% full_results$term[full_results$model=='M3']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 99% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,lineheight = 0.25),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)
gg3
library(forcats)

gg4 = ggplot(full_results%>% filter(grepl('M4',model)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M4'])[levels(full_results$term[full_results$model=='M4']) %in% full_results$term[full_results$model=='M4']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 99% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg4




gg5 = ggplot(full_results%>% filter(grepl('M5',model)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M5'])[levels(full_results$term[full_results$model=='M5']) %in% full_results$term[full_results$model=='M5']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 99% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg5

library(broom)
broom::tidy(full_mod_1)


summary(restricted_results[[1]])

lapply(restricted_results,summary)
gwd_



coef(full_mod_2)
summary(full_mod_2)




summary(full_mod_2)



unlist(gwesp_runif_results)
unlist(gwod_runif_results)
unlist(gwid_runif_results)
length(gwd_sim_summary_1)

unlist(gwd_sim_summary_1
       
       
       
       
       
       