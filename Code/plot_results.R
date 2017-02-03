rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)
library(knitr)
library(texreg)
library(forcats)
load('Scratch/temp_btergm_results.RData')

rm(list=ls()[!grepl('h_[a-z]{3}_list',ls())])
gc(reset = TRUE)

#lapply(h_mod_list,function(x) sum(rowSums(is.na(x@boot$t))==0))
for (x in 1:length(h_mod_list))
{
  if(any(is.na(h_mod_list[[x]]@boot$t))) {h_mod_list[[x]]@boot$R <- sum(rowSums(is.na(h_mod_list[[x]]@boot$t))==0)} 
}

names(h_mod_list) = c(paste('M',1:length(h_mod_list),sep=''))



lapply(1:length(h_mod_list), function(x) 
  {names(h_mod_list[[x]]@coef) <<- names(h_mod_list[[x]]@coef) %>% as.factor(.) %>% 
    fct_recode(.,
      `Isolates` = 'isolates',
      `Utility * Time` = "edgecov.timecov.utility_list_odegree[[i]]",
      `Edges` = "edges" ,
  `Dynamic (scoping)` = "edgecov.dynamic_list_prior[[i]]" ,
  `Stability (pre-imp.)` = "edgecov.stability_list_prior[[i]]",
  `Dynamic (post-scoping)` = "edgecov.timecov4.dynamic_list_prior[[i]]",
  `Stability (imp.)` = "edgecov.timecov4.stability_list_prior[[i]]",
  `Time` = "edgecov.timecov1[[i]]",      `Time^2` = "edgecov.timecov2[[i]]",    `Time^3` = "edgecov.timecov3[[i]]",                   
  `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
  `GWID (a = 2)` = "gwidegree"      ,                          
  `GWOD (a = 2)` = "gwodegree"     ,                           
  `Mutual` = "mutual"     ,                    
  `Peripheral (out)` = "nodeofactor.Peripheral.1"  ,               
  `High res. [HR] (out)` = "nodeofactor.High_Resource.1"    ,                
  `Peripheral * HR (out)` = "nodeofactor.High_Resource_x_Peripheral.1", 
  `Mandatory (out)` = "nodeofactor.Mandatory.1"  ,        
  `Utility (out)` = "nodeofactor.Utility.1"  )})


summary(h_mod_list[[2]],level=0.95)
screenreg(h_mod_list[[1]],level=0.95,digits=3)
htmlreg(h_mod_list,level=0.95,file='Scratch/mod_table.html',digits=3)


library(forcats)
library(broom)

full_results = do.call(rbind,lapply(h_mod_list, function(x) summary(x,level=0.95,digits=3) %>% tidy(.)))
full_results$model = gsub('\\.[0-9]{1,2}','',rownames(full_results))
names(full_results) = c('term','estimate','conf.low','conf.high','model')

full_results$term = fct_relevel(full_results$term,c(
  "Edges"               ,      
  "Mutual"      ,
  "Isolates",
  "GWESP (a = 2)"   ,                        
  "GWID (a = 2)"       ,                          
  "GWOD (a = 2)"      ,                           
  "Peripheral (out)"  ,               
  "High res. [HR] (out)"   ,                
  "Peripheral * HR (out)" , 
  "Utility (out)"  ,        
  "Mandatory (out)" ,
  "Time",  "Time^2",  "Time^3",
  "Utility * Time" ,
  "Dynamic (scoping)",   
  "Dynamic (post-scoping)"  ,
  "Stability (pre-imp.)"  , 
  "Stability (imp)" ))

library(ggthemes)

full_results = full_results %>% mutate(signif = ifelse(conf.low<0&conf.high>0,0,1))


gc(reset = T)
gdata::ll(unit='MB')

yellow_col = '#E69F00'
blue_col = '#56B4E9'
green_col = '#009E73'

gg2 = ggplot(full_results%>% filter(grepl('M2',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M2'])[
    levels(full_results$term[full_results$model=='M2']) %in% full_results$term[full_results$model=='M2'&full_results$term!='Edges']]),drop = T)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg2





gg3 = ggplot(full_results%>% filter(grepl('M3',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M3'])[levels(full_results$term[full_results$model=='M3']) %in% 
                                                                                    full_results$term[full_results$model=='M3'&full_results$term!='Edges']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18,lineheight = 0.25),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)
gg3
library(forcats)

gg4 = ggplot(full_results%>% filter(grepl('M4',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M4'])[levels(full_results$term[full_results$model=='M4']) %in%
                                                        full_results$term[full_results$model=='M4'&full_results$term!='Edges']]  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() +
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)
gg4

gg5 = ggplot(full_results%>% filter(grepl('M1',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='M1'])[levels(full_results$term[full_results$model=='M1']) %in% full_results$term[full_results$model=='M1'&full_results$term!='Edges']]
  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
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
       
       
       
       
       
       