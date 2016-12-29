library(tidyverse)
library(stringr)
library(statnet)
library(btergm)

rm(list=ls())

verb.type = 'communication'

talkers = read_csv('Input/scraped_data/participation_detail.csv')
talkers_summary = read_csv('Input/scraped_data/participation_summary.csv')
meeting_master = read_csv('Input/scraped_data/meeting_master.csv')
attendance = read_csv('Input/scraped_data/attendance_summary.csv')

#match(paste(talkers$Meeting,talkers$Subject_Match),paste(talkers_summary$Meeting,talkers_summary$Subject_Match))

data.frame(table(talkers$Verb.Type)) %>% arrange(-Freq)

verb_in_attendance_names = unlist(lapply(talkers$Verb, function(x) any(grepl(x,unique(attendance$Name)))))

talkers$Verb[verb_in_attendance_names]





talkers[talkers$Verb=='report'&talkers$Verb.Type=='Verbs with Predicative Complements',]
talkers[is.na(talkers$Verb.Type),]

table(talkers$Verb[talkers$Verb.Type=='Verbs with Predicative Complements'])


if(verb.type=='all')
{talkers = talkers %>% filter(!duplicated(paste0(Subject,Meeting)))}

if(verb.type=='communication')
{talkers = talkers %>% filter(Verb.Type=='Verbs of Communication') %>% 
  filter(!duplicated(paste0(Subject,Meeting)))}




participation_edges = do.call(rbind,lapply(1:nrow(talkers_summary),function(i)
  data.frame(Participant = talkers_summary$Subject_Match[i],Attendee= attendance$Name[attendance$Meeting==talkers_summary$Meeting[i]],
             part_count = talkers_summary$part_count[i],Meeting = talkers_summary$Meeting[i],Year = talkers_summary$Year[i],Date = talkers_summary$Date[i],
             Dec_Date = talkers_summary$Dec_Date[i],Phase = talkers_summary$Phase[i],Interval = talkers_summary$Interval[i])))

interval_engagement_edges = participation_edges %>% group_by(Participant, Attendee,Interval) %>% 
  summarise(direct_engagement = sum(part_count))

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

net_array = replicate(length(unique(interval_engagement_edges$Interval)),base_matrix)
dimnames(net_array)[[3]] = sort(unique(as.character(interval_engagement_edges$Interval)))
placement = cbind(match(interval_engagement_edges$Participant,meeting_attendees),
                  match(interval_engagement_edges$Attendee,meeting_attendees),
                  match(interval_engagement_edges$Interval,dimnames(net_array)[[3]]))
net_array[placement] = interval_engagement_edges$direct_engagement

net_list = lapply(1:dim(net_array)[3],function(x) 
  as.network(net_array[,,x],directed=T,matrix.type='adjacency'))


interval_attendance = lapply(1:ncol(table(attendance$Name,attendance$Interval)),function(x)
  as.data.frame.matrix(table(attendance$Name,attendance$Interval)[,replicate(nrow(table(attendance$Name,attendance$Interval)),x)],
            dimnames=list(rownames(table(attendance$Name,attendance$Interval)),
                          rownames(table(attendance$Name,attendance$Interval)))))
for (i in 1:length(interval_attendance))
{colnames(interval_attendance[[i]]) =  rownames(table(attendance$Name,attendance$Interval))}

interval_attendance_prior = interval_attendance[-length(interval_attendance)]
interval_attendance_prior= lapply(interval_attendance_prior,as.matrix)
interval_attendance_prior = lapply(interval_attendance_prior,sqrt)

interval_outdegree = apply(net_array,3,rowSums)
#lapply(1:ncol(yearly_outdegree),function(x) as.data.frame.matrix()
interval_outdegree = lapply(1:ncol(interval_outdegree),function(i)
cbind(replicate(nrow(interval_outdegree),interval_outdegree[,i])))
for (i in 1:length(interval_outdegree)){colnames(interval_outdegree[[i]]) = 
  rownames(interval_outdegree[[i]])}
interval_outdegree_prior = interval_outdegree[-length(interval_outdegree)]
interval_outdegree_prior = lapply(interval_outdegree_prior,sqrt)

interval_outdegree_x_attendance_prior = lapply(1:length(interval_outdegree_prior),function(x) interval_outdegree_prior[[x]]*
         interval_attendance_prior[[x]])



dynamic_list = lapply(1:dim(net_array)[3],function(x) base_matrix)
for (mat in 2:length(net_list))
{
  dynamic_list[[mat]] <- ifelse(as.sociomatrix(net_list[[mat-1]])>0,1,-1)
}
dynamic_list_prior = dynamic_list[-dim(net_array)[3]]

stability_list = lapply(1:dim(net_array)[3],function(x) base_matrix)
for (mat in 2:length(net_list))
{
  stability_list[[mat]] <- ifelse(as.sociomatrix(net_list[[mat-1]])>0,1,0)
}
stability_list_prior = stability_list[-dim(net_array)[3]]


for (x in 1:length(net_list))
{
  network::set.vertex.attribute(net_list[[x]],attrname = 'High_Resource',
                       value = rowSums(node_base[match(network.vertex.names(net_list[[x]]),node_base$Name),
                                                 c('Agency','Utility','Consultant')]))
}

require(igraph)
coreness_metric_list = lapply(1:length(net_list),function(x)
graph.coreness(graph_from_adjacency_matrix(as.sociomatrix(net_list[[x]]))))

for (x in 2:length(net_list))
{
  network::set.vertex.attribute(net_list[[x]],attrname = 'Core_Order_Prior',
          value = coreness_metric_list[[x-1]][
            match(network.vertex.names(net_list[[x]]),names(coreness_metric_list[[x-1]]))])
}

for (x in 2:length(net_list))
{
  network::set.vertex.attribute(net_list[[x]],value = network::get.vertex.attribute(net_list[[x]],'High_Resource') *
  network::get.vertex.attribute(net_list[[x]],'Core_Order_Prior'),
  attrname = 'High_Resource_x_Core_Order_Prior')}



for (x in 2:length(net_list))
{
  network::set.vertex.attribute(net_list[[x]],attrname = 'Utility',
                                value = ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Utility==1],1,0))
}


node_base$Mandatory = ifelse(grepl("NMFS|USFS|WDOE",node_base$Org),1,0)

for (x in 2:length(net_list))
{
  network::set.vertex.attribute(net_list[[x]],attrname = 'Mandatory',
                                value = ifelse(network::network.vertex.names(net_list[[x]]) %in% node_base$Name[node_base$Mandatory==1],1,0))
}


utility_matrix_odegree = do.call(cbind,lapply(1:network::network.size(net_list[[2]]),function(x) network::get.vertex.attribute(net_list[[2]],'Utility')))
utility_list_odegree  = lapply(1:length(net_list),function(x) utility_matrix_odegree)

utility_matrix_idegree = do.call(rbind,lapply(1:network::network.size(net_list[[2]]),function(x) network::get.vertex.attribute(net_list[[2]],'Utility')))
utility_list_idegree  = lapply(1:length(net_list),function(x) utility_matrix_idegree)

utility_list_odegree = utility_list_odegree[-1] 
utility_list_idegree = utility_list_idegree[-1]


# signatory_matrix_odegree = do.call(cbind,lapply(1:network.size(net_list[[2]]),function(x) get.vertex.attribute(net_list[[2]],'Signatory')))
# signatory_list_odegree  = lapply(1:length(net_list),function(x) signatory_matrix_odegree)
# 
# signatory_matrix_idegree = do.call(rbind,lapply(1:network.size(net_list[[2]]),function(x) get.vertex.attribute(net_list[[2]],'Signatory')))
# signatory_list_idegree  = lapply(1:length(net_list),function(x) signatory_matrix_idegree)
# 
# signatory_list_odegree = signatory_list_odegree[-1] 
# signatory_list_idegree = signatory_list_idegree[-1]


gwd_decay = 2
gwesp_decay = 2
Rnum = 1000

base = "net_list[-1] ~ edges  + mutual + gwidegree(gwd_decay,fixed=T) +  gwodegree(gwd_decay,fixed=T) + gwesp(gwesp_decay,fixed=T)"

h1_block = #H1
  "edgecov(dynamic_list_prior)+ 
timecov(x = dynamic_list_prior,  minimum = 8, transform = function(t) 1)"

h2_block =  #H2
  "nodecov('High_Resource')+nodecov('Core_Order_Prior')+nodecov('High_Resource_x_Core_Order_Prior')"

h3_block =   #H3
  "edgecov(stability_list_prior) + 
timecov(x = stability_list_prior,  minimum = 18, transform = function(t) 1)"

h4_block = #h5
  "timecov(x = utility_list_odegree[-1],function(t) t)"

h5_block =  #H5
  "nodeofactor('Utility') + nodeifactor('Utility') + nodeofactor('Mandatory') + nodeifactor('Mandatory')" 


table(talkers$Verb[talkers$Verb.Type=='Verbs with Predicative Complements'])
data.frame(table(talkers$Verb.Type)) %>% arrange(-Freq)

full_mod = btergm(as.formula(paste(base,h2_block,h5_block,h4_block,h1_block,h5_block,sep='+')),R= Rnum)
noh5_mod = btergm(as.formula(paste(base,h2_block,h5_block,h4_block,h1_block,sep='+')),R= Rnum)
noh1_mod = btergm(as.formula(paste(base,h2_block,h5_block,h4_block,h5_block,sep='+')),R= Rnum)


#talkers[talkers$Verb=='report'&talkers$Meeting=='2000aquatictech20000928',c('Subject','Verb','class','base_Class','Verb.Type')]

testA = btergm(as.formula(paste(base,h1_block,sep='+')),R=100)
testB = btergm(as.formula(paste(base,h3_block,sep='+')),R=100)
testC = btergm(as.formula(paste(base,h1_block,h3_block,sep='+')),R=100)

summary(testA)
summary(testB)
summary(testC)

as.formula(paste(base,h2_block,sep='+'))
as.formula(paste(base,h1_block,h2_block,sep='+'))

period_break_ddates


summary(test2)
,R=10000)


twopath +             
    nodecov('High_Resource')+
    nodecov('Core_Order_Prior')+
    nodecov('High_Resource_x_Core_Order_Prior')+
    nodeofactor('Utility') + nodeifactor('Utility') +
    memory(type = "autoregression", lag = 1) +
    edgecov(dynamic_list_prior)+ 
    timecov(x = dynamic_list_prior,  minimum = 8, transform = function(t) 1),R=100)





test2 = btergm(net_list[-1] ~ edges  + 
                #Controls
                mutual + gwidegree(2,fixed=T) +  gwodegree(2,fixed=T) + gwesp(1,fixed=T) + 
  
                #edgecov(utility_list_odegree) + 
                #edgecov(utility_list_idegree) + 
                #H5
                nodeofactor('Signatory') + nodeifactor('Signatory') + 
                #H3
                edgecov(stability_list_prior) +  
                #memory(type = "autoregression", lag = 1) +
                #H1
                edgecov(dynamic_list_prior)+ 
                timecov(x = dynamic_list_prior,  minimum = 8, transform = function(t) 1) +
                #H4
                timecov(x = utility_list_odegree[-1],function(t) t),R=10000)


?memory
library(texreg)
library(gwdegree)
gwdegree()
plotreg(test2)

summary(test2)


lapply(net_list,function(x) degreedist(x))



    


summary(test)
data('knecht')
demographics
preprocess(node_base$Utility,net_list,lag=F,covariate = T)



sex.cov <- preprocess(demographics$sex, primary.cov, friendship,
                      + lag = FALSE, covariate = TRUE)
    
    
    timecov(x = dynamic_list_prior,  transform = function(t) 1) ,R = 1000)





degreedist(net_list[[3]])



test@time.steps

network::get.vertex.attribute(net_list[[3]],'Org')

net_list[[3]]
summary(test)


colSums(is.na(test@bootsamp))
sum(is.na(test@effects))
      
  #edgecov(yearly_outdegree_prior) + edgecov(yearly_attendance_prior)+
  #edgecov(yearly_outdegree_x_attendance_prior) +
  #  edgecov(yearly_outdegree_x_attendance_x_highres_prior) +
  edgecov(dynamic_list_prior) + 
  timecov(x = dynamic_list_prior, minimum = 4, transform = function(t) 1) ,R = 100)

summary(test)



?memory

{
  if (x==1){base_matrix}
  else {ifelse(as.sociomatrix(net_list[[x-1]])>0,1,-1)}
}


  ifelse(x==1,base_matrix,ifelse(as.sociomatrix(net_list[[x-1]])>0,1,-1)))


lapply(net_list,function(x) table(as.sociomatrix(x)>0))
lapply(dynamic_list,function(x) table(x>0))






test = lapply(1:length(net_list),function(i){
if (i == 1) {base_matrix}
ifelse(as.sociomatrix(net_list[i-1])>0,1,-1)})
lapply(test,sum)

    ifelse(net_array[,,(x-1)]>0,1,-1))



sum(as.sociomatrix(net_list[[1]]))
sum(ifelse(dynamic_list[[2]]<0,0,dynamic_list[[2]]))

dynamic_list[[1]][TRUE]<- (-1)
dynamic_l


library(btergm)
dynamic_list[[1]]
set.seed(seed = 24)









test = lapply(net_list,as.sociomatrix)
test = lapply(test,function(x) ifelse(x>0,1,-1))

cbind(sapply(test,sum),
sapply(dynamic_list,sum))

data("knecht")
lapply(friendship,dim)



test = preprocess(lapply(net_list,as.sociomatrix),lag=TRUE,covariate=TRUE,
           memory='dynamic')

class(test)


mem <- preprocess(friendship, primary, demographics$sex,
                   lag = TRUE, covariate = TRUE, memory = "dynamic",
                   na = NA, na.method = "fillmode", structzero = 10,
                   structzero.method = "remove")


?`btergm-terms`
btergm(net_list~edges + timecov() + mutual,R = 100)



test = ifelse(net_array[,,1]>0,1,-1)



In the list of matrices, include a 1 whenever the previous dyad also had a 1 
and -1 if the previous dyad had a 0. 


See the attached paper for the mathematical details of the change statistic 
for dyadic dynamic.

You can include this list of matrices via an edgecov term.
You can check if you did it right by comparing the results to a 
model with the built-in memory term. 
After that, include a timecov model term (see ?"btergm-terms" for details) with the following specification 
to do the actual interaction (where 'ds' indicates the list of dyadic dynamic matrices):
  
  timecov(x = ds, minimum = 6, transform = function(t) 1)

This includes two model terms: the main effect for time, 
where 0 is used for t=2 to t=5 and 1 is used for t=6 to t=10, and the 
interaction between this term and the dyadic dynamic change statistic.
?timecov


library(btergm)


require(btergm)
 
test_plot = placement %>% as.data.frame(.) %>% filter(V3==3) %>% mutate(V1 = V1-1,V2 = V2-1)
# Load package
library(networkD3)

simpleNetwork(net_list[[1]])

networkData <- test_plot
data("MisLinks")




forceNetwork(Links = test_plot, Source = 'V1',Target = 'V2',Nodes = data.frame(people = 0:total_people-1,group=1),NodeID = 'people',Group='group')


sankeyNetwork(Links = test_plot, Source = 'V1',Target = 'V2',Nodes = data.frame(people = 0:total_people-1,group=1),NodeID = 'people',Group='group')

Links = test_plot, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)



# Plot
simpleNetwork(networkData)


?networkDynamic

class(net_list)

?btergm::`tergm-terms`

?network

