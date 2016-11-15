dat = read.csv('Input/wa_longitudinal/scraped_data/temp_cleaned_data.csv',row.names=1)
dat$Topic = as.character(dat$Topic)
dat$Topic[dat$Topic %in% c('rrg','recreational','rrg_lep')] = 'recreational'
dat$Topic[dat$Topic %in% c('trig','trig_elk','trig_bot','trig_loon','terrestrial','wildlife')] = 'terrestrial'
dat$Topic[dat$Topic %in% c('crag','cultural')] = 'cultural'
dat$Topic[dat$Topic %in% c('aquatic','aquatictech','arg','instream','fish','water')] = 'aquatic/fish'
dat$Topic[dat$Topic %in% c('economic','economics','floodcontrol')] = 'economic'
dat$Topic[dat$Topic %in% c('tst','solution')] = 'solution'
dat$Topic[dat$Topic %in% c('public','workshop')] = 'public/general'
dat$Topic[dat$Topic %in% c('bricc','solution','process')] = 'admin'
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

dat$Date = gsub('_','',gsub('^20[0-9]{2}','',gsub('[A-Za-z]','',dat$Meeting)))

library(lubridate)
dat$Date = ymd(dat$Date)

attendees = dat %>% group_by(Meeting) %>% summarise(attendees = n())
attendees$date = dat$Date[match(attendees$Meeting,dat$Meeting)]
attendees$topic = dat$Topic[match(attendees$Meeting,dat$Meeting)]
head(attendees)
library(ggplot2)
library(ggthemes)
library(ggvis)

attendees %>% ggvis(~date,~topic,fill=~topic,size=~attendees) %>% 
  layer_points(shape := "circle",opacity := 0.4) %>%
  scale_datetime("x", nice = "month") %>%
  add_legend(c("size", "fill")) %>%
  add_axis("x", title = "",properties = axis_props(
    ticks = list(stroke = 0, strokeWidth = 0),
    labels = list(angle = 45, align = "left", fontSize = 14)
  )) %>%
  add_axis("y", title = "",properties = axis_props(
    ticks = list(stroke = 0, strokeWidth = 0),
    labels = list(angle = 0,  fontSize = 14)
  ))
ggvis::add_scale()


ggplot(attendees,aes(x=date,y = topic)) + geom_point() + themes_tufte()




