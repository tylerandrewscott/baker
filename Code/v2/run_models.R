rm(list=ls())
set.seed(24)
library(lubridate)
library(statnet)
library(btergm)
library(tidyverse)
library(stringr)
library(texreg)

library(intergraph)
Rnum = 1000
Rnum_sim = 100
gwd_sim_reps= 1000
verb.type = 'all'
drop_list = c("Verbs with Predicative Complements")
#drop_list = c('Order Verbs',)

#old
#phase_break_date = c(mdy('5/8/2003'),mdy('11/24/2004'),mdy('10/17/2008'),mdy('01/01/2015'))
#new
phase_break_date = c(mdy('5/2/2002'),mdy('11/24/2004'),mdy('10/17/2008'),mdy('01/01/2015'))

phase_name = c('planning/scoping','application/settlement development',
               'agency review','license implementation')
phase_break_ddate  = decimal_date(phase_break_date)
period_break_ddates = seq(decimal_date(mdy('05/01/2000')),decimal_date(mdy('11/01/2014')),0.5)

talkers = read_csv('Input/scraped_data/participation_detail.csv')

meeting_master = read_csv('Input/scraped_data/meeting_master.csv')

attendance = read_csv('Input/scraped_data/attendance_summary.csv')
talkers = talkers %>% filter(!is.na(Verb.Type),!Verb.Type %in% drop_list)
talkers = talkers %>% filter(!duplicated(paste(Meeting,rowid,Subject)))

attendance = attendance %>% filter(Meeting %in% talkers$Meeting)

attendance %>% group_by(Phase,Name) %>% summarise(att_in_phase = n())  %>%
  arrange(att_in_phase)

att_in_phase = as.data.frame(table(attendance$Name,attendance$Phase)) %>% 
  rename(Name = Var1,Phase = Var2, ATT_IN_PHASE = Freq) %>%
  filter(ATT_IN_PHASE > 0)


joint_df = full_join(as.data.frame(table(attendance$Name,attendance$Meeting)) %>% 
                       rename(Attend = Freq,Name = Var1,Meeting = Var2),
                     as.data.frame(table(talkers$Subject_Match,talkers$Meeting)) %>%
                       rename(Part_Freq = Freq,Name = Var1,Meeting = Var2) %>% 
                       filter(Part_Freq>0)) %>% 
  mutate(Attend = ifelse(is.na(Attend)&!is.na(Part_Freq)&Part_Freq>0,1,Attend))

meeting_master$Meeting_Topic = tolower(gsub('[0-9]','',meeting_master$Meeting))
meeting_master$Topic = NA
meeting_master$Topic[grepl('trig|terrestrial|wildlife|botanical',meeting_master$Meeting_Topic)] = 'TRIG'
meeting_master$Topic[grepl('crag|cultural|craig',meeting_master$Meeting_Topic)] = 'CRAG'
meeting_master$Topic[grepl('brcc|bricc|solution|process',meeting_master$Meeting_Topic)] = 'BRCC'
meeting_master$Topic[grepl('arg|fish|aquatic|instream|water|fptwg',meeting_master$Meeting_Topic)] = 'ARG'
meeting_master$Topic[grepl('rrg|recreational|lep',meeting_master$Meeting_Topic)] = 'ARG'
meeting_master$Topic[grepl('economic|tst|floodcontrol',meeting_master$Meeting_Topic)] = 'EOG'
joint_df = left_join(joint_df,meeting_master)
joint_df$Part_Freq[joint_df$Attend==1&is.na(joint_df$Part_Freq)] <- 0

library(parallel)
library(pbapply)





plot(bipnet,col = c(rep('red',748),rep('blue',1339-748)))






past6_df = do.call(rbind,mclapply(unique(joint_df$Name), function(x) 
joint_df[joint_df$Name==x,] %>%
  mutate(past6m_att = sapply(Date, function(y) sum(Date < y & Date >= y - months(6) & Attend==1 ,na.rm=T)),
         past6m_part = sapply(Date, function(y) sum(Part_Freq[Date < y & Date >= y - months(6)],na.rm=T)))))


joint_df <- left_join(joint_df,past6_df)

joint_df$Meeting_Num <- as.numeric(fct_reorder(as.factor(joint_df$Meeting),joint_df$Date,min))

topic_meeting_nums <- do.call(rbind,lapply(unique(joint_df$Topic),function(x) joint_df %>% filter(Topic == x) %>%
                mutate(Topic_Meeting_Num = as.numeric(fct_reorder(as.factor(Meeting),Date,min)))))

joint_df$Topic_Meeting_Num <- topic_meeting_nums$Topic_Meeting_Num[match(joint_df$Meeting,topic_meeting_nums$Meeting)]

save.image('Scratch/scratch_file.RData')

joint_df$Part_Freq <- ifelse(!is.na(joint_df$Part_Freq),joint_df$Part_Freq,0)
joint_df$Attend_Last_Meeting <- joint_df$Attend[match(paste(joint_df$Name,joint_df$Meeting_Num),paste(joint_df$Name,joint_df$Meeting_Num+1))]
joint_df$Attend_Last_Topic_Meeting <- joint_df$Attend[match(paste(joint_df$Name,joint_df$Topic,joint_df$Topic_Meeting_Num),paste(joint_df$Name,joint_df$Topic,joint_df$Topic_Meeting_Num+1))]
joint_df$Part_Last_Meeting <- joint_df$Part_Freq[match(paste(joint_df$Name,joint_df$Meeting_Num),paste(joint_df$Name,joint_df$Meeting_Num+1))]
joint_df$Part_Last_Topic_Meeting <- joint_df$Part_Freq[match(paste(joint_df$Name,joint_df$Topic,joint_df$Topic_Meeting_Num),paste(joint_df$Name,joint_df$Topic,joint_df$Topic_Meeting_Num+1))]

in_period_df = joint_df[paste(joint_df$Name,joint_df$Phase) %in%paste(att_in_phase$Name,att_in_phase$Phase),]

att_in_topic = joint_df %>% group_by(Topic,Name,Attend) %>% summarise(Topic_Meetings = n()) %>%
  filter(Attend >0)

in_topic_df = joint_df %>% filter(paste(Name,Topic) %in% paste(att_in_topic$Name,att_in_topic$Topic))

in_phase_topic = joint_df %>% group_by(Topic,Phase,Name,Attend) %>% summarise(Topic_Phase = n()) %>%
  filter(Attend > 0 )

in_phase_topic_df = joint_df %>% filter(paste(Name,Phase,Topic) %in% paste(in_phase_topic$Name,in_phase_topic$Phase,in_phase_topic$Topic))




attendance[attendance$Name == "Chris Elizabeth",]
test<- attendance %>% group_by(Name) %>% summarise(co = n()) %>% filter(co ==1)

test$Name[test$Name %in% attendance$Name[is.na(attendance$Org)]]


require(INLA)
n = nrow(in_phase_topic_df)
u <- (in_phase_topic_df$Attend>0) + 0
y <- ifelse(in_phase_topic_df$Attend>0,in_phase_topic_df$Part_Freq,NA)
center_continuous_cov = TRUE
idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
idat$u_meeting = c(in_phase_topic_df$Meeting,in_phase_topic_df$Meeting)
idat$y_meeting = c(in_phase_topic_df$Meeting,in_phase_topic_df$Meeting)
idat$u_meeting_num = c(in_phase_topic_df$Meeting_Num-1,in_phase_topic_df$Meeting_Num-1)
idat$y_meeting_num = c(in_phase_topic_df$Meeting_Num-1,in_phase_topic_df$Meeting_Num-1)
idat$u_topic = c(in_phase_topic_df$Topic,in_phase_topic_df$Topic)
idat$y_topic = c(in_phase_topic_df$Topic,in_phase_topic_df$Topic)
idat$u_name = c(in_phase_topic_df$Name,in_phase_topic_df$Name)
idat$y_name = c(in_phase_topic_df$Name,in_phase_topic_df$Name)
idat$u_year <- c(in_phase_topic_df$Year-2000, rep(0,n))
idat$y_year<- c(rep(0,n), in_phase_topic_df$Year-2000)
idat$u_date <- c(in_phase_topic_df$Dec_Date-2000, rep(0,n))
idat$y_date <- c(rep(0,n),in_phase_topic_df$Dec_Date-2000)
idat$u_phase <- c(as.factor(in_phase_topic_df$Phase), as.factor(in_phase_topic_df$Phase))
idat$y_phase <- c(as.factor(in_phase_topic_df$Phase),as.factor(in_phase_topic_df$Phase))
idat$y.i <- idat$u.i <- c(1:n, 1:n)

idat$u_att_p6 <- c(in_phase_topic_df$past6m_att, rep(0,n))
idat$y_att_p6 <- c(rep(0,n), in_phase_topic_df$past6m_att)
idat$u_part_p6 <- c(in_phase_topic_df$past6m_part, rep(0,n))
idat$y_part_p6 <- c(rep(0,n), in_phase_topic_df$past6m_part)
idat$u_attend_last_meet <- c(in_phase_topic_df$Attend_Last_Meeting,rep(0,n))
idat$y_attend_last_meet <- c(rep(0,n),in_phase_topic_df$Attend_Last_Meeting)

#f(u_meeting, model='iid') + f(y_meeting,model='iid') +
form = "Y ~ 0 + mu.u + mu.y + u_date + y_date + 
u_attend_last_meet + y_attend_last_meet +
u_att_p6 + y_att_p6 + u_part_p6 + y_part_p6 + 
u_att_p6:u_part_p6 + y_att_p6:y_part_p6 + 
f(u_meeting_num,model = 'ar1') + f(y_meeting_num,model= 'ar1') + 
f(u_topic, model='iid') + f(y_topic,model='iid') +
f(u_phase, model='iid') + f(y_phase,model='iid')"


form = "Y ~ 0 + mu.u + mu.y + u_date + y_date + 
f(u_meeting_num,model = 'ar1',hyper = list(theta = list(prior="pc.prec", param=c(u,0.01)))) + 
f(y_meeting_num,model= 'ar1',hyper = list(theta = list(prior="pc.prec", param=c(u,0.01)))) + 
f(u_phase, model='iid') + f(y_phase,model='iid')"

v = 1
form = "Y ~ 0 + mu.u + mu.y + 
f(u_meeting_num,model = 'ar1',value = u_phase,hyper = list(theta = list(prior='pc.prec',param=c(v,0.01))))+
f(y_meeting_num,model = 'ar1',value = y_phase,hyper = list(theta = list(prior='pc.prec',param=c(v,0.01))))"


f(<whatever>, model="rw1", scale.model = TRUE
  hyper = list(theta = list(prior="pc.prec", param=c(u,0.01))))
inla.doc("pc.prec")


mod_topic_phase2 = inla(as.formula(form),c('binomial', 'poisson'),
               #control.fixed = list(expand.factor.strategy = "inla"),
               data=idat,control.compute = list(openmp.strategy="huge",waic=TRUE),
               #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
               control.inla= list(diagonal = 1000, strategy = "gaussian", int.strategy = "eb",correct = TRUE, correct.factor = 10),
               #control.mode=list(restart=T, theta= c(3.382153,3.351998, 6.101442,6.283440, 4.263011, 4.455681)),
               verbose=F,num.threads=8)

form2 = "Y ~ 0 + mu.u + mu.y + u_date + y_date + 
u_att_p6 + y_att_p6 + u_part_p6 + y_part_p6 + 
u_att_p6:u_part_p6 + y_att_p6:y_part_p6 + 
f(u_topic, model='iid') + f(y_topic,model='iid') +
f(u_phase, model='iid') + f(y_phase,model='iid')"

mod_topic_phase_2 = inla(as.formula(form2),c('binomial', 'poisson'),
                       #control.fixed = list(expand.factor.strategy = "inla"),
                       data=idat,control.compute = list(openmp.strategy="huge",waic=TRUE),
                       #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                       control.inla= list(diagonal = 1000, strategy = "gaussian", int.strategy = "eb",correct = TRUE, correct.factor = 10),
                       #control.mode=list(restart=T, theta= c(3.382153,3.351998, 6.101442,6.283440, 4.263011, 4.455681)),
                       verbose=F,num.threads=8)

summary(mod_topic_phase)
summary(mod_topic_phase_2)
mod_topic_phase$summary.random

summary(mod_topic_phase$summary.fitted.values$mean[1:n])
summary(mod_topic_phase$summary.fitted.values$mean[n+1:n])
length(idat$mu.u)

sub_sample = joint_df[sample(1:nrow(joint_df),50000,replace = F),]

n = nrow(sub_sample)
u <- (sub_sample$Attend>0) + 0
y <- ifelse(sub_sample$Attend>0,sub_sample$Part_Freq,NA)
center_continuous_cov = TRUE
idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
idat$u_meeting = c(sub_sample$Meeting,sub_sample$Meeting)
idat$y_meeting = c(sub_sample$Meeting,sub_sample$Meeting)
idat$u_topic = c(sub_sample$Topic,sub_sample$Topic)
idat$y_topic = c(sub_sample$Topic,sub_sample$Topic)
idat$u_year <- c(sub_sample$Year-2000, rep(0,n))
idat$y_year<- c(rep(0,n), sub_sample$Year-2000)
idat$u_date <- c(sub_sample$Dec_Date-2000, rep(0,n))
idat$y_date <- c(rep(0,n),sub_sample$Dec_Date-2000)
idat$u_phase <- c(as.factor(sub_sample$Phase), as.factor(sub_sample$Phase))
idat$y_phase <- c(as.factor(sub_sample$Phase),as.factor(sub_sample$Phase))
idat$y.i <- idat$u.i <- c(1:n, 1:n)
require(INLA)

#f(u_meeting, model='iid') + f(y_meeting,model='iid') +
form = "Y ~ 0 + mu.u + mu.y + u_date + y_date + 
f(u_topic, model='iid') + f(y_topic,model='iid') +
f(u_phase, model='iid') + f(y_phase,model='iid')"
#
mod1000 = inla(as.formula(form),c('binomial', 'poisson'),
           #control.fixed = list(expand.factor.strategy = "inla"),
           data=idat,control.compute = list(openmp.strategy="huge",waic=TRUE),
           #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
           control.inla= list(diagonal = 1000, strategy = "gaussian", int.strategy = "eb",correct = TRUE, correct.factor = 10),
          #control.mode=list(restart=T, theta= c(3.382153,3.351998, 6.101442,6.283440, 4.263011, 4.455681)),
           verbose=T,num.threads=8)

mod100 = inla(as.formula(form),c('binomial', 'poisson'),
              #control.fixed = list(expand.factor.strategy = "inla"),
              data=idat,control.compute = list(openmp.strategy="huge",waic=TRUE),
              #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
              control.inla= list(diagonal = 100, strategy = "gaussian", int.strategy = "eb",correct = TRUE, correct.factor = 10),
              control.mode=list(result = mod1000, restart = TRUE),
              verbose=T,num.threads=8)


mod10 = inla(as.formula(form),c('binomial', 'poisson'),
              #control.fixed = list(expand.factor.strategy = "inla"),
              data=idat,control.compute = list(openmp.strategy="huge",waic=TRUE),
              #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
             control.inla= list(diagonal = 10, strategy = "gaussian", int.strategy = "eb",correct = TRUE, correct.factor = 10),
             control.mode=list(result = mod100, restart = TRUE),
              verbose=T,num.threads=8)

n = nrow(joint_df)
u <- (joint_df$Attend>0) + 0
y <- ifelse(joint_df$Attend>0,joint_df$Part_Freq,NA)
center_continuous_cov = TRUE
idat_full <- list(Y=matrix(NA,2*n,2))
idat_full$Y[1:n,1] <- u
idat_full$Y[n+1:n,2] <- y
idat_full$mu.u <- rep(1:0, each=n)
idat_full$mu.y <- rep(0:1, each=n)
idat_full$u_meeting = c(joint_df$Meeting,joint_df$Meeting)
idat_full$y_meeting = c(joint_df$Meeting,joint_df$Meeting)
idat_full$u_topic = c(joint_df$Topic,joint_df$Topic)
idat_full$y_topic = c(joint_df$Topic,joint_df$Topic)
idat_full$u_year <- c(joint_df$Year-2000, rep(0,n))
idat_full$y_year<- c(rep(0,n), joint_df$Year-2000)
idat_full$u_date <- c(joint_df$Dec_Date-2000, rep(0,n))
idat_full$y_date <- c(rep(0,n),joint_df$Dec_Date-2000)
idat_full$u_phase <- c(as.factor(joint_df$Phase), as.factor(joint_df$Phase))
idat_full$y_phase <- c(as.factor(joint_df$Phase),as.factor(joint_df$Phase))
idat_full$y.i <- idat_full$u.i <- c(1:n, 1:n)



mod_full = inla(as.formula(form),c('binomial', 'poisson'),
             #control.fixed = list(expand.factor.strategy = "inla"),
             data=idat_full,control.compute = list(openmp.strategy="huge",waic=TRUE),
             #control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
             control.inla= list(strategy = "gaussian", correct = TRUE, correct.factor = 10),
             control.mode=list(theta = mod10$mode$theta, restart = TRUE),
             verbose=T,num.threads=8)

control.mode=list(theta=res5$mode$theta,




           #control.predictor=list(compute=TRUE),)
summary(mod)



for (i in 1:nrow(joint_df))
{
  joint_df %>% filter(Name == Name[i]) %>% filter()
}


inla(..., control.compute = list(openmp.strategy="huge")
     

     

     
     
    
     warnings()
     
     
     
     
     mod2 = inla(as.formula(form),c('binomial', 'poisson'),
                 control.fixed = list(expand.factor.strategy = "inla"),
                 data=idat_sub, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                 control.predictor=list(compute=TRUE),verbose=T)
     
     
     
     
     
     
     table(is.na(attendance$Topic))
     attendance$Meeting[is.na(attendance$Topic)]
     
     idat$u_meeting = c(joint_df$Meeting,joint_df$Meeting)
     idat$y_meeting = c(joint_df$Meeting,joint_df$Meeting)
     
     length(u)
     
     glm(u~1,family = 'binomial')
     
     
     
     
     
     test = attendance %>% group_by(Meeting) %>% summarise(freq = n()) %>% arrange(freq)
     
     
     
     
     
     
     
     
     #match(paste(talkers$Meeting,talkers$Subject_Match),paste(talkers_summary$Meeting,talkers_summary$Subject_Match))
     # verb_in_attendance_names = unlist(lapply(str_to_title(talkers$Verb), 
     #                     function(x) any(grepl(paste0(x,'$|','^',x),unique(attendance$Name)))))
     
     #as.data.frame(table(talkers$Verb.Type)) %>% arrange(-Freq)
     
     
     
     verb_cats = c('all','Verbs of Communication','Verbs of Creation and Transformation','Verbs of Change of Possession','other')
     
     
     #talker_list = 
     talkers_list = list(talkers %>% filter(!duplicated(paste0(Subject_Match,Meeting))),
                         talkers %>% filter(Verb.Type=='Verbs of Communication') %>% 
                           filter(!duplicated(paste0(Subject_Match,Meeting))),
                         talkers %>% filter(Verb.Type=='Verbs of Creation and Transformation') %>% 
                           filter(!duplicated(paste0(Subject_Match,Meeting))),
                         talkers %>% filter(Verb.Type=='Verbs of Change of Possession') %>% 
                           filter(!duplicated(paste0(Subject_Match,Meeting))),
                         talkers %>% filter(!Verb.Type %in% c('Verbs of Change of Possession','Verbs of Change of Possession','Verbs of Communication')) %>% 
                           filter(!duplicated(paste0(Subject_Match,Meeting))))
     
     
     meeting_attendees = sort(unique(attendance$Name))
     total_people = length(meeting_attendees)
     node_base = attendance %>% dplyr::select(-Meeting,-Interval) %>%
       filter(!duplicated(.))
     node_base$Agency = ifelse(grepl('USFS|FW|DOE|FERC|UCACE|DOT|NMFS|USDA',node_base$Org),1,0)
     node_base$Consultant = ifelse(grepl('Group|Associates|Consult|QEA|Engin',
                                         node_base$Org),1,0)
     node_base$Utility = ifelse(grepl('PSE',node_base$Org),1,0)
     
     base_matrix = matrix(0,ncol=total_people,nrow=total_people)
     colnames(base_matrix) = rownames(base_matrix) = meeting_attendees
     
     
     talkers = talkers_list[[1]]
     talkers_summary = talkers %>% select(-X1,-X,-Subject) %>% group_by(Meeting,Subject_Match,Interval,Phase,Date,Dec_Date,Year) %>% 
       summarise(part_count = n())
     participation_edges = do.call(rbind,lapply(1:nrow(talkers_summary),function(i)
       data.frame(Participant = talkers_summary$Subject_Match[i],Attendee= attendance$Name[attendance$Meeting==talkers_summary$Meeting[i]],
                  part_count = talkers_summary$part_count[i],Meeting = talkers_summary$Meeting[i],Year = talkers_summary$Year[i],Date = talkers_summary$Date[i],
                  Dec_Date = talkers_summary$Dec_Date[i],Phase = talkers_summary$Phase[i],Interval = talkers_summary$Interval[i])))
     
     interval_engagement_edges = participation_edges %>% group_by(Participant, Attendee,Interval) %>% 
       summarise(direct_engagement = sum(part_count))
     
     net_array = replicate(length(unique(interval_engagement_edges$Interval)),base_matrix)
     
     dimnames(net_array)[[3]] = as.character(0:max(interval_engagement_edges$Interval))
     
     
     placement = cbind(match(interval_engagement_edges$Participant,meeting_attendees),
                       match(interval_engagement_edges$Attendee,meeting_attendees),
                       match(interval_engagement_edges$Interval,dimnames(net_array)[[3]]))
     net_array[placement] = interval_engagement_edges$direct_engagement
     
     any_meeting = lapply(1:dim(net_array)[[3]],function(x) 
       ifelse(dimnames(net_array)[[1]] %in% 
                attendance$Name[attendance$Interval==x-1],1,0))
     
     any_meeting_mat = lapply(any_meeting,function(x) x %o% x)
     
     tie_within_attendees = lapply(1:dim(net_array)[[3]],function(x) 
       net_array[,,x][any_meeting[[x]]==1,any_meeting[[x]]==1])
     
     
     net_list = lapply(1:dim(net_array)[[3]],function(x) as.network(net_array[,,x],directed=TRUE,matrix.type='adjacency'))
     
     
     #net_list = lapply(1:dim(net_array)[3],function(x) 
     #  as.network(net_array[,,x],directed=T,matrix.type='adjacency'))
     
     #net_list = lapply(tie_within_attendees,function(x) as.network(x,directed=T,matrix.type='adjacency'))
     
     att_list = lapply(sort(unique(attendance$Meeting)),function(x) filter(attendance,Meeting==x))
     att_list_mat = lapply(att_list,function(x) ifelse(rownames(base_matrix) %in% x$Name,1,0)%o%ifelse(rownames(base_matrix) %in% x$Name,1,0))
     names(att_list_mat)  = sort(unique(attendance$Meeting))
     net_list_attendance = lapply(sort(unique(attendance$Interval)), function(x) Reduce('+',att_list_mat[names(att_list_mat)%in%attendance$Meeting[attendance$Interval==x]]))
     net_list_attendance = lapply(net_list_attendance,function(x) as.network(x,directed=F,matrix.type='adjacency'))
     
     
     stability_list = lapply(net_list,function(x) ifelse(as.sociomatrix(x)==1,1,-1))
     stability_list_prior = stability_list[-length(stability_list)]
     
     attendance_stability_list = lapply(net_list_attendance,function(x) ifelse(as.sociomatrix(x)==1,1,-1))
     attendance_stability_prior = attendance_stability_list[-length(attendance_stability_list)]
     
     
     autoreg_list = lapply(net_list,function(x) ifelse(as.sociomatrix(x)==1,1,0))
     autoreg_list_prior = autoreg_list[-length(autoreg_list)]
     
     autoreg_attendance_list = lapply(net_list_attendance,function(x) ifelse(as.sociomatrix(x)==1,1,0))
     autoreg_attendance_prior = autoreg_attendance_list[-length(autoreg_attendance_list)]
     
     
     for (x in 1:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'High_Resource',
                                     value = rowSums(node_base[match(network.vertex.names(net_list[[x]]),node_base$Name),
                                                               c('Agency','Utility','Consultant')]))
     }
     
     require(igraph)
     coreness_metric_list = lapply(1:length(net_list),function(x)
       graph.coreness(graph_from_adjacency_matrix(as.sociomatrix(net_list[[x]]))))
     coreness_rank_list = lapply(coreness_metric_list,function(x) as.numeric(as.factor(x)))
     
     pagerank_list = lapply(1:length(net_list),function(x) igraph::page_rank(asIgraph((net_list[[x]]))))
     pagerank_prior = lapply(pagerank_list[-length(pagerank_list)],function(x) x[[1]])
     pagerank_prior_odegree = lapply(pagerank_prior,function(x) 
       replicate(length(x),x*100))
     
     
     for (x in 2:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'Page_Rank_Prior',
                                     value = 100*pagerank_list[[x-1]]$vector)}
     
     for (x in 2:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'Core_Order_Prior',
                                     value = coreness_metric_list[[x-1]][
                                       match(network.vertex.names(net_list[[x]]),names(coreness_metric_list[[x-1]]))])}
     
     for (x in 2:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],attrname = 'Core_Rank_Prior',
                                    value = coreness_rank_list[[x-1]][
                                      match(network.vertex.names(net_list[[x]]),names(coreness_metric_list[[x-1]]))]-1)}
     
     for (x in 2:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],
                                    value = network::get.vertex.attribute(net_list[[x]],'High_Resource') *
                                      network::get.vertex.attribute(net_list[[x]],'Core_Rank_Prior'),
                                    attrname = 'High_Resource_x_Core_Rank_Prior')}
     
     for (x in 2:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],
                                    value = network::get.vertex.attribute(net_list[[x]],'High_Resource') *
                                      network::get.vertex.attribute(net_list[[x]],'Page_Rank_Prior'),
                                    attrname = 'High_Resource_x_Page_Rank_Prior')}
     
     
     for (x in 1:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'Utility',
                                     value = ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Utility==1],1,0))
     }
     
     
     node_base$Mandatory = ifelse(grepl("NMFS|USFS|WDOE",node_base$Org),1,0)
     
     for (x in 1:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'Mandatory',
                                     value = ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Mandatory==1],1,0))
     }
     
     for (x in 1:length(net_list))
     {
       network::set.vertex.attribute(net_list[[x]],attrname = 'Org_Type',
                                     value = ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Mandatory==1],'Mandatory',
                                                    ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Utility==1],'Utility','Other')))
     }
     
     
     utility_list_odegree = lapply(net_list,function(x) replicate(network.size(x),network::get.vertex.attribute(x,'Utility')))
     invisible(lapply(1:length(utility_list_odegree),function(x) rownames(utility_list_odegree[[x]]) <<- colnames(utility_list_odegree[[x]]) <<- network.vertex.names(net_list[[x]])))
     utility_list_odegree = utility_list_odegree[-1] 
     utility_list_idegree = lapply(utility_list_odegree,t)
     
     
     
     mandatory_list_odegree = lapply(net_list,function(x) replicate(network.size(x),network::get.vertex.attribute(x,'Mandatory')))
     invisible(lapply(1:length(mandatory_list_odegree),function(x) rownames(mandatory_list_odegree[[x]]) <<- colnames(mandatory_list_odegree[[x]]) <<- network.vertex.names(net_list[[x]])))
     mandatory_list_odegree = mandatory_list_odegree[-1] 
     mandatory_list_idegree = lapply(mandatory_list_odegree,t)
     
     num_meetings_attended = tidyr::expand(attendance,nesting(Name,Org),nesting(Interval,Phase)) %>%
       left_join(.,attendance %>% group_by(Name,Org,Interval,Phase) %>% summarise(Meetings_Attended = n())) %>%
       mutate(Meetings_Attended = ifelse(is.na(Meetings_Attended),0,Meetings_Attended))
     num_meetings_attended$Interval = num_meetings_attended$Interval + 1
     
     meet_num_list = lapply(1:length(net_list),function(x)
       (num_meetings_attended %>% filter(Interval == x))$Meetings_Attended[match(network.vertex.names(net_list[[x]]),
                                                                                 (num_meetings_attended %>% filter(Interval == x))$Name)])
     
     for (x in 1:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],attrname = 'Meetings_Attended',
                                    value = meet_num_list[[x]])}
     
     for (x in 2:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],attrname = 'Prior_Meetings_Attended',
                                    value = meet_num_list[[x-1]])}
     
     for (x in 2:length(net_list))
     {network::set.vertex.attribute(net_list[[x]],attrname = 'Attended_Any_Prior',
                                    value = ifelse(meet_num_list[[x-1]]==0,0,1))}
     
     # 
     # for (x in 2:length(net_list))
     # {network::set.vertex.attribute(net_list[[x]],attrname = 'Attended_Any_Prior_x_Core_Rank_Prior',
     #                                value = network::get.vertex.attribute(net_list[[x]],'Attended_Any_Prior') * 
     #                                  network::get.vertex.attribute(net_list[[x]],'Core_Rank_Prior'))}
     # 
     # for (x in 2:length(net_list))
     # {network::set.vertex.attribute(net_list[[x]],attrname = 'Attended_Any_Prior_x_Core_Rank_Prior_x_High_Resource',
     #                                value = network::get.vertex.attribute(net_list[[x]],'Attended_Any_Prior_x_Core_Rank_Prior') * 
     #                                  network::get.vertex.attribute(net_list[[x]],'High_Resource'))}
     
     gwid_decay = gwod_decay = 2
     gwesp_decay = 2
     
     # form0A = net_list[-1] ~ edges  + mutual + isolates + gwidegree(gwid_decay,fixed=T) +  
     #   #gwodegree(gwod_decay,fixed=T) +
     #   gwesp(gwesp_decay,fixed=T) + timecov(transform = function(t) t)
     # form0B = net_list[-1] ~ edges  + mutual + isolates + gwidegree(gwid_decay,fixed=T) +  
     #   #gwodegree(gwod_decay,fixed=T) + 
     #   gwesp(gwesp_decay,fixed=T) + timecov(transform = function(t) t) + timecov(transform = function(t) t^2)
     # form0C = net_list[-1] ~ edges  + mutual + isolates + gwidegree(gwid_decay,fixed=T) +  
     #   #gwodegree(gwod_decay,fixed=T) + 
     #   gwesp(gwesp_decay,fixed=T) + 
     #   timecov(transform = function(t) t)+ timecov(transform = function(t) t^2)+ timecov(transform = function(t) t^3)
     
     #null_mod_list = lapply(grep('form0',ls(),value=T),function(x) btergm(get(x),R=Rnum,parallel = 'multicore',ncpus = 4))
     #null_mod_list2 = lapply(grep('form0',ls(),value=T),function(x) btergm(get(x),R=100,parallel = 'multicore',ncpus = 4))
     
     # form0B = net_list[-1] ~ edges  + mutual + isolates + 
     #   gwidegree(gwid_decay,fixed=T) +  #gwodegree(gwod_decay,fixed=T) + 
     #   gwesp(gwesp_decay,fixed=T) + 
     #   timecov(transform = function(t) t) +
     #   timecov(transform = function(t) t^2)
     # 
     # form0C = net_list[-1] ~ edges  + mutual + isolates + gwidegree(gwid_decay,fixed=T) +  gwodegree(gwod_decay,fixed=T) + 
     #   gwesp(gwesp_decay,fixed=T) + 
     #   timecov(transform = function(t) t) +
     #   timecov(transform = function(t) t^2) +
     #   timecov(transform = function(t) t^3)
     
     
     utility_to_utility = lapply(1:length(utility_list_odegree),function(x) utility_list_odegree[[x]]*utility_list_idegree[[x]])
     mandatory_to_mandatory = lapply(1:length(utility_list_odegree),function(x) mandatory_list_odegree[[x]]*mandatory_list_idegree[[x]])
     utility_to_mandatory = lapply(1:length(utility_list_odegree),function(x) utility_list_odegree[[x]]*mandatory_list_idegree[[x]])
     mandatory_to_utility = lapply(1:length(utility_list_odegree),function(x) mandatory_list_odegree[[x]]*utility_list_idegree[[x]])
     
     mandatory_to_other = lapply(1:length(utility_list_odegree),function(x) mandatory_list_odegree[[x]]*abs(utility_list_idegree[[x]]+mandatory_list_idegree[[x]] - 1))
     utility_to_other = lapply(1:length(utility_list_odegree),function(x) utility_list_odegree[[x]]*abs(utility_list_idegree[[x]]+mandatory_list_idegree[[x]] - 1))
     
     other_to_utility = lapply(1:length(utility_list_odegree),function(x) abs(utility_list_odegree[[x]]+mandatory_list_odegree[[x]] - 1) * utility_list_idegree[[x]])
     other_to_mandatory = lapply(1:length(utility_list_odegree),function(x) abs(utility_list_odegree[[x]]+mandatory_list_odegree[[x]] - 1) * mandatory_list_idegree[[x]])
     other_to_other = lapply(1:length(utility_list_odegree),function(x) abs(utility_list_odegree[[x]]+mandatory_list_odegree[[x]] - 1) * abs(utility_list_idegree[[x]]+mandatory_list_idegree[[x]] - 1))
     
     
     
     
     
     form_test = net_list[-1] ~ #edges  + 
       nodecov('Prior_Meetings_Attended') +
       nodecov('Meetings_Attended') +
       #nodematch('Org_Type',diff=TRUE) + 
       nodemix('Org_Type')
     
     form_test = net_list[-1] ~ edges  + 
       nodecov('Prior_Meetings_Attended') +
       nodecov('Meetings_Attended') +
       #nodematch('Org_Type',diff=TRUE) + 
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)
     
     
     form_test2 = net_list[-1] ~ #edges  + 
       nodecov('Prior_Meetings_Attended') +
       nodecov('Meetings_Attended') +
       #nodematch('Org_Type',diff=TRUE) + 
       edgecov(mandatory_to_mandatory)+
       edgecov(other_to_mandatory)+
       edgecov(utility_to_mandatory)+
       edgecov(mandatory_to_other)+
       edgecov(other_to_other)+
       edgecov(utility_to_other)+
       edgecov(mandatory_to_utility)+
       edgecov(other_to_utility)+
       edgecov(utility_to_utility)+
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)+
       timecov(mandatory_to_mandatory,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(mandatory_to_mandatory,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(mandatory_to_mandatory,minimum = 18, transform = function(t) 1)+
       timecov(mandatory_to_other,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(mandatory_to_other,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(mandatory_to_other,minimum = 18, transform = function(t) 1)+
       timecov(mandatory_to_utility,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(mandatory_to_utility,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(mandatory_to_utility,minimum = 18, transform = function(t) 1)+
       timecov(utility_to_mandatory,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(utility_to_mandatory,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(utility_to_mandatory,minimum = 18, transform = function(t) 1)+
       timecov(utility_to_other,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(utility_to_other,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(utility_to_other,minimum = 18, transform = function(t) 1)+
       timecov(utility_to_utility,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(utility_to_utility,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(utility_to_utility,minimum = 18, transform = function(t) 1)+
       timecov(other_to_mandatory,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(other_to_mandatory,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(other_to_mandatory,minimum = 18, transform = function(t) 1)+
       timecov(other_to_other,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(other_to_other,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(other_to_other,minimum = 18, transform = function(t) 1)+
       timecov(other_to_utility,minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(other_to_utility,minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(other_to_utility,minimum = 18, transform = function(t) 1)
     
     
     #gwidegree(gwid_decay,fixed=T) + 
     #gwodegree(gwod_decay,fixed=T) + 
     #gwesp(gwesp_decay,fixed=T) + 
     #edgecov(mandatory_list_odegree) +  edgecov(utility_list_odegree) + 
     edgecov(autoreg_list_prior)
     
     form_test = net_list[-1] ~ 
       nodecov('Prior_Meetings_Attended') +
       nodecov('Meetings_Attended') +
       #nodematch('Org_Type',diff=TRUE) + 
       nodemix('Org_Type') +
       #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
       #timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       #timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(transform = function(t) t)
     
     
     form_test2 = net_list[-1] ~ 
       nodecov('Prior_Meetings_Attended') +
       nodecov('Meetings_Attended') +
       #nodematch('Org_Type',diff=TRUE) + 
       nodemix('Org_Type') +
       #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
       #timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       #timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(transform = function(t) t)+
       timecov(transform = function(t) t^2)+
       timecov(transform = function(t) t^3)+
       timecov(transform = function(t) t^4)
     
     
     test = btergm(form_test,R=100)
     test2 = btergm(form_test2,R=100)
     
     
     timecov(minimum = 18, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) t) +
       timecov(autoreg_list_prior,minimum = 18, transform = function(t) 1)
     
     
     test2@boot$R = sum(rowSums(is.na(test2@boot$t))==0)
     test2@boot$t = test2@boot$t[rowSums(is.na(test2@boot$t))==0,]
     
     
     test@boot$R = sum(rowSums(is.na(test@boot$t))==0)
     test@boot$t = test@boot$t[rowSums(is.na(test@boot$t))==0,]
     summary(test2)
     summary(test)
     summary(test2)
     summary(m2)
     screenreg(list(test,test2))
     
     form0 = net_list[-1] ~ edges  + 
       nodecov('Meetings_Attended') + 
       gwidegree(gwid_decay,fixed=T) + 
       #gwodegree(gwod_decay,fixed=T) + 
       gwesp(gwesp_decay,fixed=T) + 
       edgecov(mandatory_list_odegree) +  edgecov(utility_list_odegree) + 
       edgecov(autoreg_list_prior) + 
       #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)
     
     
     # timecov(minimum = 8,maximum = 10, transform = function(t) t) +
     #  timecov(minimum = 11,maximum = 17, transform = function(t) t) +
     # timecov(minimum = 18, transform = function(t) t)
     #timecov(transform = function(t) t^2)+ 
     #timecov(transform = function(t) t^3)
     
     form1 = net_list[-1] ~ edges  + 
       nodecov('Meetings_Attended') + 
       gwidegree(gwid_decay,fixed=T) + 
       #gwodegree(gwod_decay,fixed=T) + 
       gwesp(gwesp_decay,fixed=T) + 
       edgecov(mandatory_list_odegree) +  edgecov(utility_list_odegree) + 
       edgecov(autoreg_list_prior) + 
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)+
       timecov(x = autoreg_list_prior, minimum = 8, maximum = 10, transform = function(t) 1) +
       timecov(x = autoreg_list_prior, minimum = 11, maximum = 17, transform = function(t) 1) + 
       timecov(x = autoreg_list_prior, minimum = 18,  transform = function(t) 1)
     
     # form3 = net_list[-1] ~ edges  +
     #   nodecov('Meetings_Attended') +
     #   gwidegree(gwid_decay,fixed=T) +
     #   #gwodegree(gwod_decay,fixed=T) +
     #   gwesp(gwesp_decay,fixed=T) +
     #   nodeofactor('Mandatory') +  edgecov(utility_list_odegree) +
     #   edgecov(pagerank_prior_odegree) +
     #   #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
     #   timecov(minimum = 6, maximum = 10,transform = function(t) 1) +
     #   timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
     #   timecov(minimum = 18, transform = function(t) 1)+
     #   edgecov(autoreg_list_prior) +
     #   timecov(x = pagerank_prior_odegree, minimum = 6, maximum = 10, transform = function(t) 1) +
     #   timecov(x = pagerank_prior_odegree, minimum = 11, maximum = 17, transform = function(t) 1) +
     #   timecov(x = pagerank_prior_odegree, minimum = 18,  transform = function(t) 1)
     
     form2 = net_list[-1] ~ edges  + 
       nodecov('Meetings_Attended') + 
       gwidegree(gwid_decay,fixed=T) + 
       #gwodegree(gwod_decay,fixed=T) + 
       gwesp(gwesp_decay,fixed=T) + 
       edgecov(mandatory_list_odegree)  +  edgecov(utility_list_odegree) + 
       edgecov(autoreg_list_prior) + 
       #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)+
       timecov(x = mandatory_list_odegree, minimum = 8, maximum = 10, transform = function(t) 1) +
       timecov(x = mandatory_list_odegree, minimum = 11, maximum = 17, transform = function(t) 1) + 
       timecov(x = mandatory_list_odegree, minimum = 18,  transform = function(t) 1)
     
     form3 = net_list[-1] ~ edges  + 
       nodecov('Meetings_Attended') + 
       gwidegree(gwid_decay,fixed=T) + 
       #gwodegree(gwod_decay,fixed=T) + 
       gwesp(gwesp_decay,fixed=T) + 
       edgecov(mandatory_list_odegree)  +  edgecov(utility_list_odegree) + 
       edgecov(autoreg_list_prior) + 
       #timecov(minimum = 8, maximum = 17,transform = function(t) 1) +
       timecov(minimum = 8, maximum = 10,transform = function(t) 1) +
       timecov(minimum = 11,maximum = 17, transform = function(t) 1) +
       timecov(minimum = 18, transform = function(t) 1)+
       timecov(x = utility_list_odegree, minimum = 8, maximum = 10, transform = function(t) 1) +
       timecov(x = utility_list_odegree, minimum = 11, maximum = 17, transform = function(t) 1) + 
       timecov(x = utility_list_odegree, minimum = 18,  transform = function(t) 1) 
     # form4 = net_list[-1] ~ edges + 
     #   #mutual + isolates + 
     #   nodecov('Meetings_Attended') + 
     #   gwidegree(gwid_decay,fixed=T) + 
     #   gwodegree(gwod_decay,fixed=T) + 
     #   #gwesp(gwesp_decay,fixed=T) + 
     #   nodeofactor('Mandatory') +  edgecov(utility_list_odegree) + 
     #   timecov(transform = function(t) t)+
     #   timecov(minimum = 19, transform = function(t) 1) +
     #   edgecov(autoreg_list_prior) + 
     #   timecov(x = autoreg_list_prior, minimum = 19, transform = function(t) 1) 
     
     # form5 = net_list[-1] ~ edges  + 
     #   #mutual + isolates + 
     #   nodecov('Meetings_Attended') + 
     #   gwidegree(gwid_decay,fixed=T) + 
     #   gwodegree(gwod_decay,fixed=T) + 
     #   #gwesp(gwesp_decay,fixed=T) + 
     #   timecov(transform = function(t) t)+ #timecov(transform = function(t) t^2)+ timecov(transform = function(t) t^3)+
     #   nodeofactor('Mandatory') + #nodeofactor('Utility') + 
     #   edgecov(utility_list_odegree) + 
     #   timecov(transform = function(t) t)+
     #   timecov(x = utility_list_odegree,  transform = function(t) t)
     
     
     h1_block = #H1
       "edgecov(dynamic_list_prior)+ 
     timecov(x = dynamic_list_prior,  minimum = 8, transform = function(t) 1)"
     h2_block =  #H2
       "nodecov('High_Resource')+nodecov('Core_Order_Prior')+nodecov('High_Resource_x_Core_Order_Prior')"
     h3_block =   #H3
       "edgecov(stability_list_prior) + 
     timecov(x = stability_list_prior,  minimum = 18, transform = function(t) 1)"
     h4_block = #h5
       "timecov(x = utility_list_odegree,function(t) t)"
     h5_block =  #H5
       "nodeofactor('Mandatory') + nodeofactor('Utility')" 
     
     library(parallel)
     
     #base_mod_list = lapply(grep('form[0-9][A-Z]',ls(),value=T),function(x) btergm(get(x),R=Rnum,parallel = 'multicore',ncpus = 4))
     
     #h_mod_list = lapply(grep('form[0-9]',ls(),value=T),function(x) btergm(get(x),R=Rnum,parallel = 'multicore',ncpus = 4))
     
     m0 = btergm(form0,R=Rnum,parallel='multicore',ncpus=8)
     #save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
     m1 = btergm(form1,R=Rnum,parallel='multicore',ncpus=8)
     #m2a = btergm(form2A,R=Rnum,parallel='multicore',ncpus=8)
     #save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
     #m3 = btergm(form3,R=Rnum,parallel='multicore',ncpus=8)
     #save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
     m2 = btergm(form2,R=Rnum,parallel='multicore',ncpus=8)
     #save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
     m3 = btergm(form3,R=Rnum,parallel='multicore',ncpus=8)
     
     save.image('Scratch/temp_btergm_results3.RData',compress = TRUE,safe=TRUE)
     
     # h_gof_list = lapply(list(m0),function(x) 
     #   gof(x,statistic=c(rocpr,dsp,esp,ideg,odeg),parallel='multicore',ncpus=8,MCMC.interval=10000))
     # save.image('Scratch/temp_btergm_results3.RData',compress = TRUE,safe=TRUE)
     
     # eprobs_m2a = edgeprob(m2a)
     # eprobs_m2a$phase = ifelse(eprobs_m2a$t %in% 1:7,'Planning/scoping',ifelse(eprobs_m2a$t %in% 8:10,'Application/development',
     #                                                                           ifelse(eprobs_m2a$t %in% 11:18,'Review','Implementation')))
     # eprobs_summary = eprobs_m2a %>% group_by(nodecov.Page_Rank_Prior,nodefactor.High_Resource.1,phase) %>%
     #   summarize(mean_pred = mean(probability))
     # rm(eprobs_m2a)
     # save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
     
     # full_mod_1 = btergm(as.formula(paste(base,h1_block,h2_block,h5_block,sep='+')),R= Rnum)
     # 
     # full_mod_2 = btergm(as.formula(paste(base,h3_block,h2_block,h5_block,sep='+')),R= Rnum)
     # 
     # full_mod_3 = btergm(as.formula(paste(base,h2_block,h4_block,h5_block,sep='+')),R= Rnum)
     