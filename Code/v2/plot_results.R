rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)

library(texreg)
library(forcats)
load('Scratch/temp_btergm_results3.RData')
#load('Scratch/temp_btergm_results2.RData')
form_ex = net_list[-1] ~ edges  + 
  ostar(k=4) + 
  nodecov('Meetings_Attended') + 
  edgecov(mandatory_list_odegree) +  edgecov(utility_list_odegree) + 
  edgecov(autoreg_list_prior) + 
  #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
  timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
  timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
  timecov(minimum = 18, transform = function(t) 1)
mex = btergm(form_ex,R=Rnum,parallel='multicore',ncpus=8)

summary(mex)

names(m0@coef) = fct_recode(names(m0@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]")

names(m1@coef) = fct_recode(names(m2@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Stability * Phase 2` = "edgecov.timecov4.autoreg_list_prior[[i]]",
                            `Stability * Phase 3` = "edgecov.timecov5.autoreg_list_prior[[i]]",
                            `Stability * Phase 4` = "edgecov.timecov6.autoreg_list_prior[[i]]")



names(m2@coef) = fct_recode(names(m3@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Mandatory * Phase 2` = "edgecov.timecov4.mandatory_list_odegree[[i]]",
                            `Mandatory * Phase 3` = "edgecov.timecov5.mandatory_list_odegree[[i]]",
                            `Mandatory * Phase 4` = "edgecov.timecov6.mandatory_list_odegree[[i]]")

names(m3@coef) = fct_recode(names(m4@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Utility * Phase 2` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Utility * Phase 3` = "edgecov.timecov5.utility_list_odegree[[i]]",
                            `Utility * Phase 4` = "edgecov.timecov6.utility_list_odegree[[i]]")


screenreg(list(m1,m2,m3,m4),level=0.95,digits=3)
htmlreg(list(m1,m2,m3,m4),level=0.95,file='Scratch/mod_table_H123.html',digits=3)

eprob_list = mclapply(grep('^m[0-3]',ls(),value=T),function(x) edgeprob(get(x)),mc.cores=4,mc.cleanup=T)


#rm(list=ls()[!grepl('h_[a-z]{3}_list',ls())])
gc(reset = TRUE)

library(texreg)
library(broom)
mex@boot$R = sum(rowSums(is.na(mex@boot$t))==0)
mex@boot$t = mex@boot$t[rowSums(is.na(mex@boot$t))==0,]


mexog@boot$R = sum(rowSums(is.na(mexog@boot$t))==0)
mexog@boot$t = mexog@boot$t[rowSums(is.na(mexog@boot$t))==0,]
m0@boot$R = sum(rowSums(is.na(m0@boot$t))==0)
m0@boot$t = m0@boot$t[rowSums(is.na(m0@boot$t))==0,]
m1@boot$R = sum(rowSums(is.na(m1@boot$t))==0)
m1@boot$t = m1@boot$t[rowSums(is.na(m1@boot$t))==0,]
m2@boot$R = sum(rowSums(is.na(m2@boot$t))==0)
m2@boot$t = m2@boot$t[rowSums(is.na(m2@boot$t))==0,]
m3@boot$R = sum(rowSums(is.na(m3@boot$t))==0)
m3@boot$t = m3@boot$t[rowSums(is.na(m3@boot$t))==0,]

names(mexog@coef) = fct_recode(names(mexog@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]")
screenreg(list(mexog,m0))



temp1 = data.frame(mod = 'Model 1',phase = rep(1:4, each=nrow(m1@boot$t)),
lc = c(m1@boot$t[,c("edgecov.autoreg_list_prior[[i]]")],
rowSums(m1@boot$t[,c("edgecov.autoreg_list_prior[[i]]","edgecov.timecov4.autoreg_list_prior[[i]]")]),
rowSums(m1@boot$t[,c("edgecov.autoreg_list_prior[[i]]","edgecov.timecov5.autoreg_list_prior[[i]]")]),
rowSums(m1@boot$t[,c("edgecov.autoreg_list_prior[[i]]","edgecov.timecov6.autoreg_list_prior[[i]]")])))

temp2 = data.frame(mod = 'Model 2',phase = rep(1:4, each=nrow(m2@boot$t)),
                   lc = c(m2@boot$t[,c("edgecov.mandatory_list_odegree[[i]]")],
                          rowSums(m2@boot$t[,c("edgecov.mandatory_list_odegree[[i]]","edgecov.timecov4.mandatory_list_odegree[[i]]")]),
                          rowSums(m2@boot$t[,c("edgecov.mandatory_list_odegree[[i]]","edgecov.timecov5.mandatory_list_odegree[[i]]")]),
                          rowSums(m2@boot$t[,c("edgecov.mandatory_list_odegree[[i]]","edgecov.timecov6.mandatory_list_odegree[[i]]")])))

temp3 = data.frame(mod = 'Model 3',
  phase = rep(1:4, each=nrow(m3@boot$t)),
                   lc = c(m3@boot$t[,c("edgecov.utility_list_odegree[[i]]")],
                          rowSums(m3@boot$t[,c("edgecov.utility_list_odegree[[i]]","edgecov.timecov4.utility_list_odegree[[i]]")]),
                          rowSums(m3@boot$t[,c("edgecov.utility_list_odegree[[i]]","edgecov.timecov5.utility_list_odegree[[i]]")]),
                          rowSums(m3@boot$t[,c("edgecov.utility_list_odegree[[i]]","edgecov.timecov6.utility_list_odegree[[i]]")])))
temp_all = rbind(temp1,temp2,temp3)

ggplot(temp_all,aes(x=as.factor(phase),y=lc)) + geom_boxplot() +
  facet_wrap(~mod,ncol=2,scales = 'free_y') + theme_bw() + 
  scale_y_continuous(name = 'Linear combinations of bootstrapped samples')+
  scale_x_discrete(labels=c('Planning/scoping','Application','Review','Implementation'))+
  theme(axis.text.x=element_text(hjust=.5,angle=(360-45),vjust=0.5),
        axis.title.x=element_blank(),axis.title.y=element_text(size=14),
        axis.text = element_text(size=12))
summary(mexog)
head(ep3)






eprob_list[[2]] %>% group_by(`edgecov.autoreg_list_prior[[i]]`,
                 `edgecov.timecov5.autoreg_list_prior[[i]]`) %>%
  dplyr::summarise(tt = mean(probability)) %>%
  dplyr::rename(autoreg = `edgecov.autoreg_list_prior[[i]]`,
         autoreg_p3 = `edgecov.timecov5.autoreg_list_prior[[i]]`) %>% 
  mutate(tt = round(tt,4)) 

marginalplot(model, var1 = "edgecov.wealth.icov",
             var2 = "edgecov.priorates.icov", inter = "edgecov.interac",
             color = "darkred", rug = TRUE, point = FALSE, xlab = "Priorates",
             ylab = "Wealth") + theme_bw() + ggtitle("Interaction effect")



btergm::interpret()
test = ep3 %>% mutate(Phase = ifelse(`edgecov.timecov1[[i]]`==1,'P2',
                                     ifelse(`edgecov.timecov2[[i]]`==1,'P3',ifelse(`edgecov.timecov3[[i]]`==1,'P4','P1'))))


temp = eprob_list[[2]] %>% group_by(`edgecov.timecov1[[i]]`,
                        `edgecov.timecov2[[i]]`,
                        `edgecov.timecov3[[i]]`,
  `edgecov.timecov4.autoreg_list_prior[[i]]`,
  `edgecov.timecov5.autoreg_list_prior[[i]]`,
  `edgecov.timecov6.autoreg_list_prior[[i]]`,
  `edgecov.autoreg_list_prior[[i]]`) %>% 
  rename(Auto = `edgecov.autoreg_list_prior[[i]]`,
    P2 =`edgecov.timecov1[[i]]`,
                P3 =`edgecov.timecov2[[i]]`,
                P4 =`edgecov.timecov3[[i]]`,
                P2_Auto =`edgecov.timecov4.autoreg_list_prior[[i]]`,
                P3_Auto =`edgecov.timecov5.autoreg_list_prior[[i]]`,
                P4_Auto =`edgecov.timecov6.autoreg_list_prior[[i]]`) %>%
  mutate(Phase = ifelse(P2==1,2,ifelse(P3==1,3,ifelse(P4==1,4,1))))
  

ggplot(tt) + geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`,x=Phase,group=Auto))
?geom_errorbar

summary(eprob_list[[2]]$probability)
eptest = edgeprob(test)
ggplot(test,aes(x=Phase,y=probability,colour=Auto)) + geom_jitter(pch=19,position = 'dodge')
  


dplyr::summarise(round(median(probability),3))
cor(as.matrix(md))
dim(md)
screenreg(list(test_mod,m1))
md = btergm(form1,returndata=T)

test_mod = glm(Y ~ .-1, family = "binomial", data = md)

summary(test_mod)

summary(test_mod)


returndata(m1)



summary(m1)
names(test)[1:3] <- c('p2','p3','p4')



?marginalplot()


summary(m3)


attendance %>% group_by(Meeting) %>% summarise(in_att = n()) %>%
  summarise(min(in_att),median(in_att),mean(in_att))


summary(m2)
btergm::marginalplot(m2,var1 = "edgecov.autoreg_list_prior[[i]]",
                     var2 = "edgecov.timecov2[[i]]",
                     inter = "edgecov.timecov5.mandatory_list_odegree[[i]]",
                     point = TRUE,rug=FALSE)


screenreg(list(mexog,m0))
tapply(test$probability,test$Phase,mean)


period_list = lapply(2:length(net_list),function(x) net_list[[x]] %>% 
                       as.sociomatrix() %>% 
                       as.data.frame() %>%
                        mutate(period = x))





  group_by(Phase) %>% summarise(mean(probability))
  
  group_by(as.factor(`edgecov.autoreg_list_prior[[i]]`),
                 as.factor(`edgecov.timecov1[[i]]`),
                 as.factor(`edgecov.timecov2[[i]]`),
                 as.factor(`edgecov.timecov3[[i]]`)) %>%
  summarise(mean(probability))



table(ep3$`edgecov.autoreg_list_prior[[i]]`)


library(Rmisc)
Rmisc::group.CI(stability_lc~phase,temp)

tapply(temp$stability_lc,temp$phase,)



ggplot(temp2,aes(x=as.factor(phase),y=lc)) + geom_boxplot()
ggplot(temp3,aes(x=as.factor(phase),y=lc)) + geom_boxplot()

ggplot() + stat_summary(geom = "errorbar")
ggplot(temp,aes(x=as.factor(phase),y=stability_lc)) + 
  #stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")


ggplot(ep3,aes(y=probability,x=t,colour = `edgecov.autoreg_list_prior[[i]]`)) + stat_smooth(method='lm')



# 
# m5@boot$R = sum(rowSums(is.na(m5@boot$t))==0)
# m5@boot$t = m5@boot$t[rowSums(is.na(m5@boot$t))==0,]


names(m1@coef) = fct_recode(names(m1@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]")

summary(m3)
names(m2@coef) = fct_recode(names(m2@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Stability * Phase 2` = "edgecov.timecov4.autoreg_list_prior[[i]]",
                            `Stability * Phase 3` = "edgecov.timecov5.autoreg_list_prior[[i]]",
                            `Stability * Phase 4` = "edgecov.timecov6.autoreg_list_prior[[i]]")

names(m3@coef) = fct_recode(names(m3@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Mandatory * Phase 2` = "edgecov.timecov4.mandatory_list_odegree[[i]]",
                            `Mandatory * Phase 3` = "edgecov.timecov5.mandatory_list_odegree[[i]]",
                            `Mandatory * Phase 4` = "edgecov.timecov6.mandatory_list_odegree[[i]]")

names(m4@coef) = fct_recode(names(m4@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "edgecov.mandatory_list_odegree[[i]]"  , 
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Utility * Phase 2` = "edgecov.timecov4.utility_list_odegree[[i]]",
                            `Utility * Phase 3` = "edgecov.timecov5.utility_list_odegree[[i]]",
                            `Utility * Phase 4` = "edgecov.timecov6.utility_list_odegree[[i]]")


screenreg(list(m1,m2,m3,m4),level=0.95,digits=3)
htmlreg(list(m1,m2,m3,m4),level=0.95,file='Scratch/mod_table_H123.html',digits=3)

#htmlreg(list(m3,m4,m5),level=0.95,file='Scratch/mod_table_345.html',digits=3)


library(broom)
mod_results = do.call(rbind,lapply(grep('^m[0-9]',ls(),value=T), function(x) tidy(get(x)) %>% mutate(model = x)))
mod_results$term = as.factor(mod_results$term)
full_results = mod_results

full_results$term = fct_relevel(full_results$term,c(
  "Edges"               ,      
  "GWESP (a = 2)"   ,                        
  "GWID (a = 2)"  ,
  "Meetings attended",                          
  "Mandatory (sender)" ,
  "Utility (sender)",
  "Tie stability",
  "Phase 2",
  "Phase 3",
  "Phase 4",
  "Stability * Phase 2",
  "Stability * Phase 3",
  "Stability * Phase 4",
  "Mandatory * Phase 2",
  "Mandatory * Phase 3",
  "Mandatory * Phase 4",
  "Utility * Phase 2",
  "Utility * Phase 3",
  "Utility * Phase 4"))
library(ggthemes)
full_results = full_results %>% mutate(signif = ifelse(conf.low<0&conf.high>0,0,1)) %>%
  filter(model != 'm1')
gc(reset = T)
#gdata::ll(unit='MB')

yellow_col = '#E69F00'
blue_col = '#56B4E9'
green_col = '#009E73'

library(ggplot2)
full_results$model = gsub('m','Model ',full_results$model)
library(ggthemes)
gg1 = ggplot(full_results%>% filter(grepl('Model [2-9]',model),!grepl('Edges|edges',term)),
             aes(ymin=conf.low,ymax=conf.high,x=term,y=estimate)) + 
  geom_hline(yintercept=0,lty=2) +
  facet_wrap(~model,nrow=1) + 
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=as.factor(signif)),size=2,,pch=21) + 
  coord_flip() +
  scale_x_discrete(name = '',limits=rev(levels(full_results$term)[levels(full_results$term)!='Edges'])) +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  scale_fill_manual(name = '',values=c('white','black'))+
  theme_bw()  + guides(fill = FALSE) + 
  theme(strip.background = element_blank(),axis.text = element_text(size=12),
        axis.title =element_text(size= 14),strip.text = element_text(size = 14))
gg1

 # geom_hline(yintercept=0,lty=2)+
  geom_errorbar(position='dodge', width=0.25,lwd=1) +
  geom_point(aes(fill=paste(signif,model)),size=2,position = position_dodge(width = 0.25),pch=21) + 
 scale_x_discrete(limits=rev(levels(full_results$term[full_results$model=='m1'])[
  #  levels(full_results$term[full_results$model=='m1']) %in% full_results$term[full_results$model=='m1'&full_results$term!='Edges']]),drop = T)  +
  scale_y_continuous(name = 'Bootstrapped 95% confidence intervals')+ coord_flip() + 
  theme_bw() + 
  #scale_color_colorblind(name = 'Autoregressive comparison',labels = c('Pre/post scoping',    'Pre/post implementation')) + 
 # scale_fill_manual(name = 'Autoregressive comparison',values=c('white','black')) +
  theme(legend.position = c(0.8,0.1), legend.text = element_text(size=18),
        legend.title = element_text(size=18),  axis.title.y = element_blank(),
        axis.title.x = element_text(size=18),axis.ticks=element_blank(),
        axis.text=element_text(size=18)) + guides(fill=FALSE)

gg1




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
gg2

library(parallel)
h_gof = lapply(list(mexog,m0),function(x) 
    gof(x,statistic=c(rocpr),parallel='multicore',ncpus=8,MCMC.interval=10000))
  # save.image('Scratch/temp_btergm_results3.RData',compress = TRUE,safe=TRUE)
  
mex_gof = gof(mex,statistic=c(rocpr),parallel='multicore',ncpus=8,MCMC.interval=1000)
plot(mex_gof)
summary(mex)
plot(h_gof[[1]])
plot(h_gof[[2]])  



