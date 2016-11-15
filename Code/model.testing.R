library(statnet)
library(dplyr)
library(tidyr)
library(knitr)
library(lubridate)
rm(list=ls())
dat = read.csv('Input/wa_longitudinal/scraped_data/temp_cleaned_data.csv',stringsAsFactors = F)


dim(dat)

minor_dates = read.csv('Input/wa_longitudinal/minor_dates.csv',stringsAsFactors = F)
dat = dat %>% select(-X) %>% mutate(dec_date = decimal_date(ymd(Date)))
  
## Code for major period of meeting
end.of.planning_scoping = mdy('5/8/2003')
end.of.application_settlement_development = mdy('11/24/2004')
end.of.agency_review = mdy('10/17/2008')
key_dates = c(end.of.planning_scoping,end.of.application_settlement_development,end.of.agency_review)
how_many_after = unlist(lapply(lapply(dat$Date,function(x) x > key_dates),sum))
period_reference = data.frame( how_many_after = 0:3,period = c('A','B','C','D'))
dat$Period = period_reference$period[match(how_many_after,period_reference$how_many_after)]

## Code for minor period of meeting
minor_dates$Date = mdy(minor_dates$Date)
minor_dates$Minor_Period = 1:nrow(minor_dates)
how_many_after_minor = unlist(lapply(lapply(dat$Date,function(x) x > minor_dates$Date),sum))
dat$Period_Minor = how_many_after_minor



library(ggplot2)
library(ggthemes)
p2 = ggplot(dat %>% group_by(Name,Period) %>% summarise(meetings_attended = n())) + theme_tufte(ticks=F) + 
  facet_wrap(~Period,scales = 'free_x') + 
  geom_histogram(aes(x=meetings_attended),binwidth = 1) +  ggtitle('Distribution of meetings attended by period')
p2



p3 = ggplot(dat %>% group_by(Meeting,Date,Topic) %>% 
summarise(num_attending = n()) %>%  mutate(decim_date = decimal_date(ymd(Date)))) + 
  theme_tufte(ticks=F) + ylab('# persons in attendance') + xlab('date')+
#  facet_wrap(~Period) + 
  geom_point(aes(x=decim_date,y=num_attending,colour=Topic)) + scale_color_colorblind()+
  stat_smooth(aes(x=decim_date,y=num_attending,colour=Topic),method = 'loess',se=FALSE)


dat = dat %>% mutate(Name = as.factor(Name),Meeting = as.factor(Meeting))

dat_by_period = lapply(levels(dat$Period),function(x) dat %>% filter(Period==x))

bipadj_by_period = lapply(dat_by_period,function(x) as.matrix(table(x$Name,x$Meeting)))
          
personadj_by_period = lapply(bipadj_by_period,tcrossprod)
person_network_by_period = lapply(personadj_by_period,as.network,directed=F,ignore.eval=F,names.eval='coattended')


library(xergm)
require("parallel")
library(statnet)

bipfull_matrix= as.matrix(table(dat$Name,dat$Meeting))
full_adjmatrix = tcrossprod(bipfull_matrix)
full_network = as.network(full_adjmatrix,ignore.eval=F,names.eval='coattended')

library(ggnetwork)
library(viridis)
sna::cen

period_summaries = data.frame(
  period = c('Planning/Scoping',
             'Application/Settlement development',
             'Agency review',
             'License implementation'),
  meeting_attendees_in_period = tabulate(dat$Period),
meetings_in_period = tabulate(dat$Period[!duplicated(dat$Meeting)]),
edges = sapply(person_network_by_period,function(x) length(x$mel)),
density = round(sapply(person_network_by_period, function(x) sna::gden(x,mode = 'graph')),2),
degree_centralization = round(sapply(person_network_by_period, function(x) 
  sna::centralization(x,FUN = degree)),2))


btergm(person_network_by_period~edges,)


kable(period_summaries,format = 'html')

lapply(as.sociomatrix(person_network_by_period),sna::centralization,FUN=degree)
class(person_network_by_period[[1]])

densityplot(density(degreedist(person_network_by_period[[1]])))
par(mfrow=c(2,2))
lapply(person_network_by_period,function(pp) 
  print(ggplot(data.frame(ddist = degreedist(pp,print = F),degree = as.numeric(gsub('degree','',names(degreedist(pp,print = F)))))) + 
  geom_bar(aes(x=degree,y=ddist),stat='identity')+ theme_bw()))

lapply(person_network_by_period,class)
grid.arrange(gg_ddist_by_period,nrow=2)
person_network_by_period[[1]]


g2 = ggplot(data.frame(ddist = degreedist(person_network_by_period[[2]],print = F),degree = as.numeric(gsub('degree','',names(degreedist(person_network_by_period[[2]],print = F)))))) + 
  geom_bar(aes(x=degree,y=ddist),stat='identity')+ theme_bw()
g3 = ggplot(data.frame(ddist = degreedist(person_network_by_period[[3]],print = F),degree = as.numeric(gsub('degree','',names(degreedist(person_network_by_period[[3]],print = F)))))) + 
  geom_bar(aes(x=degree,y=ddist),stat='identity') + theme_bw()
g4 = ggplot(data.frame(ddist = degreedist(person_network_by_period[[4]],print = F),degree = as.numeric(gsub('degree','',names(degreedist(person_network_by_period[[4]],print = F)))))) + 
  geom_bar(aes(x=degree,y=ddist),stat='identity')+ theme_bw()

library(gridExtra)
?grid.arrange
grid.arrange(list(g1,g2,g3,g3),nrow=2)
ddist = unlist(degreedist(person_network_by_period[[1]]))

dg1
#Plot simple histograms of the degree distribution:
par(mfrow=c(2,2)) # Set up a 2x2 display
hist(degreedist(person_network_by_period[[1]]), xlab="Degree", main="Indegree Distribution", prob=TRUE)
hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
par(mfrow=c(1,1))
     
     
sna::centralization(person_network_by_period[[1]])

structure.statistics(person_network_by_period[[1]])

ggplot(ggnetwork(full_network, weights = "coattended",by='coattended'), aes(x, y, xend = xend, yend = yend)) + 
  geom_edges(aes(colour=coattended),arrow = arrow(length = unit(0.3, "lines")),curvature = 0.15, alpha = 0.5) +
  #geom_nodes(shape = 21, fill = "gold", color = "tomato") +
  geom_nodes(shape=19)+
  theme_blank() + scale_color_viridis()
  
  
  + scale_color_viridis(limits=c(1,100),breaks=c(25,75,100),labels=c(25,75,'100+'))




table(get.edge.attribute(full_network,'coattended'))
facet_wrap(~ Frequency) +
  
  


ggplot(person_network, aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes()

get.edge.attribute(person_network,'coattended')



?geom_edges

fit <- btergm(personadj_by_period ~ edges + istar(2))

summary(fit)
              
              + edgecov(covariates),
              R = 100, parallel = "snow", ncpus = 25)
# Equivalently, a PSOCK cluster can be provided as follows:
require("parallel")




library(bipartite)
bipartite::plotweb(adj_by_period[[1]])


              %>% as.matrix(table(.$Name,.$Meeting)))


bipartite_adjacency_empty = as.matrix(table(dat$Name,dat$Meeting))
bipartite_adjacency_empty[TRUE] = 0

as.factor(dat$Name)






table(dat$Date[!duplicated(dat$Meeting)])

warnings()
head(dat %>% group_by(Meeting,Date) %>% summarise(num_attending = n()))



p3
+ scale_x_continuous(
    breaks=c(1,100,200,300),labels=c(1,100,200,300)) + ggtitle('Distribution of meetings attended by period')
p3





+ scale_x_continuous(limits=c(1,400),expand=c(0,0))

  

%>% ggvis(~meetings_attended) %>%
  layer_histograms(width = 2)

dev.copy(p1)

  scale_("x", domain = c(1, 400), nice = FALSE) 
ggvis::sc

  scale_numeric("y", domain = c(0, 6), nice = FALSE)


  add_axis("x", title = "# Meetings Attended", ticks = 40,
           properties = axis_props(
             labels = list(
               fill = "steelblue",
               angle = 50,
               fontSize = 14,
               align = "left",
               baseline = "middle",
               dx = 3
             )))
  
  ?add_axis()


test = dat %>% group_by(Name) %>% summarise(meetings_attended = n())







n.uq.persons = length(unique(dat$Name))
unique.persons = sort(unique(dat$Name))
unique.meetings = sort(unique(dat$Meeting))
n.uq.meetings = length(unique(dat$Meeting))










uq.names = sort(unique(dat$Name))

n.names = length(uq.names)

uq.meetings = sort(unique(dat$Meeting))
n.meet = length(uq.meetings)
unique(dat$Topic)
empty.bipartite = matrix(0,ncol=n.meet,nrow=n.names)
colnames(empty.bipartite) = uq.meetings
rownames(empty.bipartite) = uq.names

all.possible = expand.grid(uq.names,uq.meetings);colnames(all.possible) = c('Name','Meeting')
all.possible$At.Meeting = 0
all.possible$At.Meeting[paste(all.possible$Name,all.possible$Meeting) %in% paste(dat$Name,dat$Meeting)] = 1
all.possible = all.possible %>% arrange(Name,Meeting)

bipartite.df = spread(all.possible,Meeting,At.Meeting) 
rownames(bipartite.df) = bipartite.df$Name
bipartite.matrix = bipartite.df %>% select(-Name) %>% as.matrix(.)
library(ggnetwork);library(lubridate)
bipartite.network = network::network(bipartite.matrix,bipartite=T,directed=F)
network::set.vertex.attribute(bipartite.network,attrname = 'Level',value = c(rep('Person',n.names),rep('Meeting',n.meet)))
network::set.vertex.attribute(bipartite.network,attrname = 'MeetingTopic',value = as.character(dat$Topic[match(network.vertex.names(bipartite.network),dat$Meeting)]))
network::set.vertex.attribute(bipartite.network,attrname = 'MeetingDate',value = as.character(dat$Date[match(network.vertex.names(bipartite.network),dat$Meeting)]))
network::set.vertex.attribute(bipartite.network,attrname = 'MeetingYear',value = dat$Year[match(network.vertex.names(bipartite.network),dat$Meeting)])
network::set.vertex.attribute(bipartite.network,attrname = 'NodeSize',value = c(rep(3,n.names),rep(7,n.meet)))


test = ergm(bipartite.network~edges +gwb1degree(0.25))
+ gwb2degree(0.25, fixed=TRUE) )
bipartite.network



