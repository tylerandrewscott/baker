rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)
library(knitr)
library(texreg)
library(forcats)
load('Scratch/temp_btergm_results.RData')

#rm(list=ls()[!grepl('h_[a-z]{3}_list',ls())])
gc(reset = TRUE)
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
htmlreg(list(m1,m2,m2a,m3,m4,m5),level=0.95,file='Scratch/mod_table.html',digits=3)
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
gg1
###Note: takes a long time to run; generates giant 19M row dataframe
eprobs_m2a = edgeprob(m2a,ncpus = 8, parallel = "multicore")
eprobs_m2a$phase = ifelse(eprobs_m2a$t %in% 1:7,'Planning/scoping',ifelse(eprobs_m2a$t %in% 8:10,'Application/development',
                                                                          ifelse(eprobs_m2a$t %in% 11:18,'Review','Implementation')))
eprobs_m2a = eprobs_m2a %>% filter(probability!=1)

test = eprobs_m2a[seq(0,19000000,10000),]


facets(edgeprobs = test,mem=mem,number=3,var1 =
         "nodeocov.Page_Rank_Prior", var2 =
         "nodeofactor.High_Resource.1", varname1 = "Page rank",
       varname2 = "High resource")
facets(edgeprobs = edgeprob.3, mem = mem, number = 3, var1 =
         "nodeocov.Page_Rank_Prior", var2 =
         "nodeofactor.High_Resource.1", varname1 = "Page rank",
       varname2 = "High resource") 







library(intergraph)

temp = eprobs_m2a %>% group_by(nodecov.Page_Rank_Prior,nodefactor.High_Resource.1,phase) %>% select(nodecov.Page_Rank_Prior,nodefactor.High_Resource.1,phase,t,probability)
temp$phase = as.factor(temp$phase)
temp$phase = fct_relevel(temp$phase,c('Planning/scoping','Application/development','Review','Implementation'))
temp = temp[!duplicated(temp),]

ggplot(temp,aes(x=nodecov.Page_Rank_Prior,y=probability,
                colour=as.factor(nodefactor.High_Resource.1))) +
  geom_point(pch=21) + scale_color_colorblind(name = 'Tie involving..',labels=c("0 high resource actors",
                                                                                '1 high resource actor',
                                                                                '2 high resource actors'))+ 
  stat_smooth(fullrange = FALSE) + facet_wrap(~phase,scales = 'free_x')+theme_tufte(ticks=F) + 
  scale_y_continuous(name = 'Predicted value',limits=c(0,1)) + scale_x_continuous(name = 'Prior centrality score') +
  theme(legend.position =c(0.8,0.15),strip.text=element_text(size=18),
        axis.text = element_text(size=16),axis.title = element_text(size=18),
        legend.text=element_text(size=14),legend.title=element_text(size=16)) 



temp = eprobs_m2a %>% filter(t %in% seq(5,25,5))

gdata::ll(unit='MB')
head(eprobs_m2a)
test = eprobs_m2a %>% filter(t %in% c(10)) %>%
  dplyr::select(nodecov.Core_Rank_Prior,nodefactor.High_Resource.1, nodecov.Core_Rank_Prior_x_High_Resource,t,probability) %>%
  group_by(nodecov.Core_Rank_Prior,nodefactor.High_Resource.1, nodecov.Core_Rank_Prior_x_High_Resource,t) %>% 
  summarise(avg_prob = mean(probability))

ggplot(test %>% arrange(nodecov.Core_Rank_Prior),aes(x=nodecov.Core_Rank_Prior,y=avg_prob,
                                                     linetype=as.factor(nodecov.High_Resource))) + geom_line() +
  stat_smooth()


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


test = btergm(net_list[-1] ~ edges + mutual + isolates + nodecov("Meetings_Attended") + 
                gwidegree(gwid_decay, fixed = T) + gwesp(gwesp_decay, fixed = T) + 
                timecov(transform = function(t) t) + 
                timecov(transform = function(t) t^2) + 
                timecov(transform = function(t) t^3) + 
                nodeofactor("Mandatory") + 
                edgecov(utility_list_odegree) + timecov(utility_list_odegree, 
                                                        transform = function(t) t),R=100)
summary(test)
library(broom)
broom::tidy(full_mod_1)
summary(m5)
test = btergm(form5,R=100,returndata = T)

test = btergm(net_list[-1] ~  mutual + isolates + nodecov("Meetings_Attended") + 
                gwidegree(gwid_decay, fixed = T) + gwesp(gwesp_decay, fixed = T) + 
                nodeofactor("Mandatory") + edgecov(utility_list_odegree) +
                timecov(maximum = 7,transform=function(t) 1) + 
                timecov(minimum = 8,maximum = 10, transform = function(t) 1) +
                timecov(minimum = 11,maximum = 18, transform = function(t) 1) +
                timecov(minimum = 19, transform = function(t) 1),R=100,returndata=F)

test2 = btergm(net_list[-1] ~  edges + mutual + isolates + nodecov("Meetings_Attended") + 
                 gwidegree(gwid_decay, fixed = T) + gwesp(gwesp_decay, fixed = T) + 
                 nodeofactor("Mandatory") + edgecov(utility_list_odegree) +
                 timecov(minimum = 8,maximum = 10, transform = function(t) 1) +
                 timecov(minimum = 11,maximum = 18, transform = function(t) 1) +
                 timecov(minimum = 19, transform = function(t) 1)
               
               
               ,R=100,returndata=F)


test@boot$t = test@boot$t[rowSums(is.na(test@boot$t))==0,]
test@boot$R = nrow(test@boot$t)
test2@boot$t = test2@boot$t[rowSums(is.na(test2@boot$t))==0,]
test2@boot$R = nrow(test2@boot$t)
summary(test)
summary(test2)
screenreg(list(test,test2))

plotreg(test)
head(test)
glm(Y~.-1,test,family='binomial')


warnings()

edgecov(utility_list_odegree) + timecov(x = utility_list_odegree, 
                                        minimum = 8, transform = function(t) 1),R=100)

summary(test)
timecov(x = utility_list_odegree, minimum = 11, maximum = 18, transform = function(t) 1) + 
  timecov(x = utility_list_odegree, minimum = 19, transform = function(t) 1)

table(test$`edgecov.utility_list_odegree[[i]]`)
table(test$`edgecov.timecov2.utility_list_odegree[[i]]`)
table(test$`edgecov.timecov3.utility_list_odegree[[i]]`)
table(test$`edgecov.utility_list_odegree[[i]]`,test$`edgecov.timecov3.utility_list_odegree[[i]]`)
test$`edgecov.timecov3.utility_list_odegree[[i]]`)


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
       
       
       
       
       
       