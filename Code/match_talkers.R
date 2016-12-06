library(tidyverse)
library(stringr)
talkers = read_csv('Input/pdfs/extraction/named_entities.csv') %>% 
  dplyr::select(-X1) %>%
  filter(!grepl('[A-Z]{4}',Subject),!grepl('^[a-z]',Subject)) %>%
  mutate(Subject = gsub('\\.$','',Subject)) %>%
  mutate(Meeting = gsub('\\.txt$','',Meeting)) %>%
  filter(!grepl('Upper Baker',Subject)) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))

attendance = read_csv('Input/scraped_data/temp_cleaned_data.csv') %>% 
  dplyr::select(-X1) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))

match_as_present = lapply(1:length(talkers$Subject),
                          function(x) ifelse(length( grep(talkers$Subject[x],attendance$Name[attendance$Meeting==talkers$Meeting[x]]))==0,NA,
                                             grep(talkers$Subject[x],attendance$Name[attendance$Meeting==talkers$Meeting[x]],value=T)))

talkers = talkers %>% mutate(Subject_Match = unlist(match_as_present)) %>%
  filter(!is.na(Subject_Match)) %>% group_by(Subject_Match,Meeting,Year) %>% summarise(part_count = n())

participation_edges = do.call(rbind,lapply(1:nrow(talkers),function(i)
  data.frame(Participant = talkers$Subject_Match[i],Attendee= attendance$Name[attendance$Meeting==talkers$Meeting[i]],
             part_count = talkers$part_count[i],Meeting = talkers$Meeting[i],Year = talkers$Year[i])))

year_engagement_edges = participation_edges %>% group_by(Participant, Attendee, Year) %>% summarise(direct_engagement = sum(part_count))


meeting_attendees = sort(unique(attendance$Name))
total_people = length(meeting_attendees)

node_base = attendance %>% dplyr::select(-Meeting,-Year) %>%
  filter(!duplicated(.))

node_base$Agency = ifelse(grepl('USFS|FW|DOE|FERC|UCACE|DOT',node_base$Org),1,0)
node_base$Consultant = ifelse(grepl('Group|Associates|Consulting|QEA|Engin',
                                   node_base$Org),1,0)
node_base$Utility = ifelse(grepl('PSE',node_base$Org),1,0)

base_matrix = matrix(0,ncol=total_people,nrow=total_people)
colnames(base_matrix) = rownames(base_matrix) = meeting_attendees

net_array = replicate(length(unique(year_engagement_edges$Year)),base_matrix)
dimnames(net_array)[[3]] = sort(unique(as.character(year_engagement_edges$Year)))
placement = cbind(match(year_engagement_edges$Participant,meeting_attendees),
                  match(year_engagement_edges$Attendee,meeting_attendees),
                  match(year_engagement_edges$Year,dimnames(net_array)[[3]]))
net_array[placement] = year_engagement_edges$direct_engagement

net_list = lapply(1:dim(net_array)[3],function(x) 
  as.network(net_array[,,x],directed=T,matrix.type='adjacency'))


yearly_attendance = lapply(1:ncol(table(attendance$Name,attendance$Year)),function(x)
  as.data.frame.matrix(table(attendance$Name,attendance$Year)[,replicate(nrow(table(attendance$Name,attendance$Year)),x)],
            dimnames=list(rownames(table(attendance$Name,attendance$Year)),
                          rownames(table(attendance$Name,attendance$Year)))))
for (i in 1:length(yearly_attendance))
colnames(yearly_attendance[[i]]) =  rownames(table(attendance$Name,attendance$Year))
yearly_attendance_prior = yearly_attendance[-length(yearly_attendance)]
yearly_attendance_prior= lapply(yearly_attendance_prior,as.matrix)
yearly_attendance_prior = lapply(yearly_attendance_prior,sqrt)


yearly_outdegree = apply(net_array,3,rowSums)
#lapply(1:ncol(yearly_outdegree),function(x) as.data.frame.matrix()
yearly_outdegree = lapply(1:ncol(yearly_outdegree),function(i)
cbind(replicate(nrow(yearly_outdegree),yearly_outdegree[,i])))
for (i in 1:length(yearly_outdegree)){colnames(yearly_outdegree[[i]]) = 
  rownames(yearly_outdegree[[i]])}
yearly_outdegree_prior = yearly_outdegree[-length(yearly_outdegree)]
yearly_outdegree_prior = lapply(yearly_outdegree_prior,sqrt)

yearly_outdegree_x_attendance_prior = lapply(1:length(yearly_outdegree_prior),function(x) yearly_outdegree_prior[[x]]*
         yearly_attendance_prior[[x]])


stability_list = lapply(1:dim(net_array)[3],function(x) base_matrix)
stability_list = lapply(1:16,function(x) base_matrix)
for (mat in 2:length(net_list))
{
  stability_list[[mat]] <- ifelse(as.sociomatrix(net_list[[mat-1]])>0,1,-1)
}
stability_list_prior = stability_list[-16]

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

test = btergm(net_list[-1] ~ edges  + mutual + nodecov('High_Resource')+
    nodecov('Core_Order_Prior')+
    nodecov('High_Resource_x_Core_Order_Prior')+
    memory(type = "autoregression", lag = 1)+
    edgecov(stability_list_prior)+ 
    timecov(x = stability_list_prior, minimum = 7, transform = function(t) 1) ,R = 1000)

test@time.steps

network::get.vertex.attribute(net_list[[3]],'Org')

net_list[[3]]
summary(test)


colSums(is.na(test@bootsamp))
sum(is.na(test@effects))
      
  #edgecov(yearly_outdegree_prior) + edgecov(yearly_attendance_prior)+
  #edgecov(yearly_outdegree_x_attendance_prior) +
  #  edgecov(yearly_outdegree_x_attendance_x_highres_prior) +
  edgecov(stability_list_prior) + 
  timecov(x = stability_list_prior, minimum = 4, transform = function(t) 1) ,R = 100)

summary(test)



?memory

{
  if (x==1){base_matrix}
  else {ifelse(as.sociomatrix(net_list[[x-1]])>0,1,-1)}
}


  ifelse(x==1,base_matrix,ifelse(as.sociomatrix(net_list[[x-1]])>0,1,-1)))


lapply(net_list,function(x) table(as.sociomatrix(x)>0))
lapply(stability_list,function(x) table(x>0))






test = lapply(1:length(net_list),function(i){
if (i == 1) {base_matrix}
ifelse(as.sociomatrix(net_list[i-1])>0,1,-1)})
lapply(test,sum)

    ifelse(net_array[,,(x-1)]>0,1,-1))



sum(as.sociomatrix(net_list[[1]]))
sum(ifelse(stability_list[[2]]<0,0,stability_list[[2]]))

stability_list[[1]][TRUE]<- (-1)
stability_l


library(btergm)
stability_list[[1]]
set.seed(seed = 24)









test = lapply(net_list,as.sociomatrix)
test = lapply(test,function(x) ifelse(x>0,1,-1))

cbind(sapply(test,sum),
sapply(stability_list,sum))

data("knecht")
lapply(friendship,dim)



test = preprocess(lapply(net_list,as.sociomatrix),lag=TRUE,covariate=TRUE,
           memory='stability')

class(test)


mem <- preprocess(friendship, primary, demographics$sex,
                   lag = TRUE, covariate = TRUE, memory = "stability",
                   na = NA, na.method = "fillmode", structzero = 10,
                   structzero.method = "remove")


?`btergm-terms`
btergm(net_list~edges + timecov() + mutual,R = 100)



test = ifelse(net_array[,,1]>0,1,-1)



In the list of matrices, include a 1 whenever the previous dyad also had a 1 
and -1 if the previous dyad had a 0. 


See the attached paper for the mathematical details of the change statistic 
for dyadic stability.

You can include this list of matrices via an edgecov term.
You can check if you did it right by comparing the results to a 
model with the built-in memory term. 
After that, include a timecov model term (see ?"btergm-terms" for details) with the following specification 
to do the actual interaction (where 'ds' indicates the list of dyadic stability matrices):
  
  timecov(x = ds, minimum = 6, transform = function(t) 1)

This includes two model terms: the main effect for time, 
where 0 is used for t=2 to t=5 and 1 is used for t=6 to t=10, and the 
interaction between this term and the dyadic stability change statistic.
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

