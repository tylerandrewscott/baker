rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)

library(texreg)
library(forcats)
load('Scratch/temp_btergm_results.RData')
#load('Scratch/temp_btergm_results2.RData')

#rm(list=ls()[!grepl('h_[a-z]{3}_list',ls())])
gc(reset = TRUE)

length(unique(talkers$Meeting))
library(texreg)
library(broom)

names(m1@coef) = fct_recode(names(m1@coef),`Isolates` = 'isolates',
                            `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `GWOD (a = 2)` = "gwodegree"     ,                           
                            `Mutual` = "mutual"     ,     
                            `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                            `Prior centrality` = "nodecov.Page_Rank_Prior",
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Time` = "edgecov.timecov1[[i]]")

names(m2@coef) = fct_recode(names(m2@coef),`Isolates` = 'isolates',
                            `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `GWOD (a = 2)` = "gwodegree"     ,                           
                            `Mutual` = "mutual"     ,     
                            `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                            `Prior centrality` = "nodecov.Page_Rank_Prior",
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Time` = "edgecov.timecov1[[i]]",
                            `High resource [HR]` = "nodeofactor.High_Resource.1",
                            `Prior centrality` = "nodeocov.Page_Rank_Prior")

names(m2a@coef) = fct_recode(names(m2a@coef),`Isolates` = 'isolates',
                             `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                             `Edges` = "edges" ,
                             `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                             `GWID (a = 2)` = "gwidegree"      ,                          
                             `GWOD (a = 2)` = "gwodegree"     ,                           
                             `Mutual` = "mutual"     ,     
                             `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                             `Prior centrality` = "nodecov.Page_Rank_Prior",
                             `Meetings attended` = "nodecov.Meetings_Attended",
                             `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                             `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                             `Time` = "edgecov.timecov1[[i]]",
                             `High resource [HR]` = "nodeofactor.High_Resource.1",
                             `Prior centrality` = "nodeocov.Page_Rank_Prior",
                             `HR * Prior centrality` = "nodeocov.High_Resource_x_Page_Rank_Prior")


names(m3@coef) = fct_recode(names(m3@coef),`Isolates` = 'isolates',
                            `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `GWOD (a = 2)` = "gwodegree"     ,                           
                            `Mutual` = "mutual"     ,     
                            `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                            `Prior centrality` = "nodecov.Page_Rank_Prior",
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Time` = "edgecov.timecov1[[i]]",
                            `Post-planning` = "edgecov.timecov2[[i]]",
                            `Dyad stability` = "edgecov.stability_list_prior[[i]]",
                            `Stability * post-planning` = "edgecov.timecov3.stability_list_prior[[i]]")

names(m4@coef) = fct_recode(names(m4@coef),`Isolates` = 'isolates',
                            `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `GWOD (a = 2)` = "gwodegree"     ,                           
                            `Mutual` = "mutual"     ,     
                            `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                            `Prior centrality` = "nodecov.Page_Rank_Prior",
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Time` = "edgecov.timecov1[[i]]",
                            `Implementation` = "edgecov.timecov2[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Stability * implementation` = "edgecov.timecov3.autoreg_list_prior[[i]]")

names(m5@coef) = fct_recode(names(m5@coef),`Isolates` = 'isolates',
                            `Utility * Time` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `GWOD (a = 2)` = "gwodegree"     ,                           
                            `Mutual` = "mutual"     ,     
                            `High res. [HR]` = "nodefactor.High_Resource.1"    ,  
                            `Prior centrality` = "nodecov.Page_Rank_Prior",
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Time` = "edgecov.timecov1[[i]]",
                            `Utility * time` = "edgecov.timecov2.utility_list_odegree[[i]]")




screenreg(list(m1,m2,m2a,m3,m4,m5),level=0.95,digits=3)
htmlreg(list(m1,m2,m2a),level=0.95,file='Scratch/mod_table_122a.html',digits=3)
htmlreg(list(m3,m4,m5),level=0.95,file='Scratch/mod_table_345.html',digits=3)


library(broom)
mod_results = do.call(rbind,lapply(grep('^m[0-9]',ls(),value=T), function(x) tidy(get(x)) %>% mutate(model = x)))
mod_results$term = as.factor(mod_results$term)
full_results = mod_results

levels(full_results$term)
full_results$term = fct_relevel(full_results$term,c(
  "Edges"               ,      
  "Mutual"      ,
  "Isolates",
  "GWESP (a = 2)"   ,                        
  "GWID (a = 2)"  ,
  "Meetings attended",                          
  "Prior centrality",
  "High resource [HR]"   , 
  "HR * Prior centrality",    
  "Mandatory (sender)" ,
  "Utility (sender)",
  "Time", 
  "Utility * time" ,
  "Post-planning",
  "Dyad stability",
  "Tie stability",
  "Stability * post-planning",
  "Implementation",
  "Stability * implementation"))

library(ggthemes)

full_results = full_results %>% mutate(signif = ifelse(conf.low<0&conf.high>0,0,1))

gc(reset = T)
#gdata::ll(unit='MB')

yellow_col = '#E69F00'
blue_col = '#56B4E9'
green_col = '#009E73'

gg1 = ggplot(full_results%>% filter(grepl('m1',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m1'])[
    levels(full_results$term[full_results$model=='m1']) %in% full_results$term[full_results$model=='m1'&full_results$term!='Edges']]),drop = T)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

###Note: takes a long time to run; generates giant 19M row dataframe
eprobs_m2a = edgeprob(m2a)
eprobs_m2a$phase = ifelse(eprobs_m2a$t %in% 1:7,'Planning/scoping',ifelse(eprobs_m2a$t %in% 8:10,'Application/development',
                                                                          ifelse(eprobs_m2a$t %in% 11:18,'Review','Implementation')))
library(parallel)
cl = makeCluster(getOption("cl.cores", 8))
clusterExport(cl,c('eprobs_m2a','any_meeting_mat'))
attend_prior_meeting = parLapply(1:nrow(eprobs_m2a),function(x) any_meeting_mat[[eprobs_m2a$t[x]]][eprobs_m2a$i[x],eprobs_m2a$j[x]]==1,cl = cl)

eprobs_m2a_priormeeting = eprobs_m2a %>% filter(unlist(attend_prior_meeting),probability!=1)


gp <- ggplot(data = eprobs_m2a_priormeeting, aes(x = `nodeocov.Page_Rank_Prior`, y = probability, colour = factor(`nodeofactor.High_Resource.1`))) +
  stat_smooth(method = "lm", fullrange = FALSE,se=FALSE) 
  #stat_smooth(method = "loess", fullrange = FALSE,se=FALSE,lty=2)

gp + scale_x_continuous(name = "Prior centrality (page rank)") + scale_y_continuous(name = "Probability")+
  #ggtitle("Prior centrality conditional on organizational resources")  + 
  scale_color_colorblind(name = 'Organization (sender)',labels=c('Low resource',"High resource")) + 
  theme_bw() + 
 # scale_linetype_manual(name='fit',labels=c('lm','Loess'))+
  theme(legend.position = c(0.8,0.2),axis.text=element_text(size=18), 
        axis.title = element_text(size=18),legend.title=element_text(size=18),
        legend.text=element_text(size=18))


gg2 = ggplot(full_results%>% filter(grepl('m2$',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m1'])[
    levels(full_results$term[full_results$model=='m2']) %in% full_results$term[full_results$model=='m2'&full_results$term!='Edges']]),drop = T)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg2a = ggplot(full_results%>% filter(grepl('m2a$',model),!grepl('Edges',term)),
              aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m1'])[
    levels(full_results$term[full_results$model=='m2a']) %in% full_results$term[full_results$model=='m2a'&full_results$term!='Edges']]),drop = T)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)


gg3 = ggplot(full_results%>% filter(grepl('m3',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m3'])[levels(full_results$term[full_results$model=='m3']) %in% 
                                                                                    full_results$term[full_results$model=='m3'&full_results$term!='Edges']]
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




gg4 = ggplot(full_results%>% filter(grepl('m4',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m4'])[levels(full_results$term[full_results$model=='m4']) %in%
                                                                                    full_results$term[full_results$model=='m4'&full_results$term!='Edges']]  ),drop = F)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() +
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
  scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg5 = ggplot(full_results%>% filter(grepl('m5',model),!grepl('Edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
  scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m5'])[levels(full_results$term[full_results$model=='m5']) %in% full_results$term[full_results$model=='m5'&full_results$term!='Edges']]
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






