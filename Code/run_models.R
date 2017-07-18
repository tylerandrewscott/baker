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

phase_break_date = c(mdy('5/8/2003'),mdy('11/24/2004'),mdy('10/17/2008'),mdy('01/01/2015'))
phase_name = c('planning/scoping','application/settlement development',
               'agency review','license implementation')
phase_break_ddate  = decimal_date(phase_break_date)
period_break_ddates = seq(decimal_date(mdy('05/01/2000')),decimal_date(mdy('11/01/2014')),0.5)

talkers = read_csv('Input/scraped_data/participation_detail.csv')

meeting_master = read_csv('Input/scraped_data/meeting_master.csv')

attendance = read_csv('Input/scraped_data/attendance_summary.csv')
talkers = talkers %>% filter(!is.na(Verb.Type),!Verb.Type %in% drop_list)
attendance = attendance %>% filter(Meeting %in% talkers$Meeting)



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


utility_list_odegree = lapply(net_list,function(x) replicate(network.size(x),network::get.vertex.attribute(x,'Utility')))
invisible(lapply(1:length(utility_list_odegree),function(x) rownames(utility_list_odegree[[x]]) <<- colnames(utility_list_odegree[[x]]) <<- network.vertex.names(net_list[[x]])))
utility_list_odegree = utility_list_odegree[-1] 

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




form1 = net_list[-1] ~ edges  + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  nodeofactor('Mandatory') +  edgecov(utility_list_odegree) + 
  timecov(transform = function(t) t)

# timecov(minimum = 8,maximum = 10, transform = function(t) t) +
#  timecov(minimum = 11,maximum = 18, transform = function(t) t) +
# timecov(minimum = 19, transform = function(t) t)
#timecov(transform = function(t) t^2)+ 
#timecov(transform = function(t) t^3)

form2 = net_list[-1] ~ edges  + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  timecov(transform = function(t) t)+
  #timecov(transform = function(t) t^2)+ 
  #timecov(transform = function(t) t^3)+
  nodeofactor('High_Resource') + 
  nodeocov('Page_Rank_Prior')

form2A = net_list[-1] ~ edges  + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  timecov(transform = function(t) t)+
  nodeofactor('High_Resource') + 
  nodeocov('Page_Rank_Prior') + 
  nodeocov('High_Resource_x_Page_Rank_Prior')


form3 = net_list[-1] ~ edges  + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  #timecov(transform = function(t) t^2)+ timecov(transform = function(t) t^3)+
  #timecov(transform = function(t) t^2) +
  nodeofactor('Mandatory') +  edgecov(utility_list_odegree) + 
  timecov(transform = function(t) t) +
  timecov(minimum = 8, transform = function(t) 1) +
  edgecov(stability_list_prior) + 
  timecov(x = stability_list_prior, minimum = 8, transform = function(t) 1)


form4 = net_list[-1] ~ edges + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  nodeofactor('Mandatory') +  edgecov(utility_list_odegree) + 
  timecov(transform = function(t) t)+
  timecov(minimum = 19, transform = function(t) 1) +
  edgecov(autoreg_list_prior) + 
  timecov(x = autoreg_list_prior, minimum = 19, transform = function(t) 1) 

form5 = net_list[-1] ~ edges  + 
  #mutual + isolates + 
  nodecov('Meetings_Attended') + 
  gwidegree(gwid_decay,fixed=T) + 
  gwodegree(gwod_decay,fixed=T) + 
  #gwesp(gwesp_decay,fixed=T) + 
  timecov(transform = function(t) t)+ #timecov(transform = function(t) t^2)+ timecov(transform = function(t) t^3)+
  nodeofactor('Mandatory') + #nodeofactor('Utility') + 
  edgecov(utility_list_odegree) + 
  timecov(transform = function(t) t)+
  timecov(x = utility_list_odegree,  transform = function(t) t)


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

m1 = btergm(form1,R=Rnum,parallel='multicore',ncpus=8)
#save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
m2 = btergm(form2,R=Rnum,parallel='multicore',ncpus=8)
m2a = btergm(form2A,R=Rnum,parallel='multicore',ncpus=8)
#save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
m3 = btergm(form3,R=Rnum,parallel='multicore',ncpus=8)
#save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
m4 = btergm(form4,R=Rnum,parallel='multicore',ncpus=8)
#save.image('Scratch/temp_btergm_results.RData',compress = TRUE,safe=TRUE)
m5 = btergm(form5,R=Rnum,parallel='multicore',ncpus=8)

save.image('Scratch/temp_btergm_results2.RData',compress = TRUE,safe=TRUE)

h_gof_list = lapply(list(m1,m2,m2a,m3,m4,m5),function(x) 
  gof(x,statistic=c(rocpr,dsp,esp,ideg,odeg),parallel='multicore',ncpus=8,MCMC.interval=10000))
save.image('Scratch/temp_btergm_results2.RData',compress = TRUE,safe=TRUE)



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
