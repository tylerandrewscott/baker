rm(list=ls())

verb.type = 'all'
drop_list = c("Verbs with Predicative Complements")
#drop_list = c('Order Verbs',)
library(lubridate)
phase_break_date = c(mdy('5/8/2003'),mdy('11/24/2004'),mdy('10/17/2008'),mdy('01/01/2015'))
phase_name = c('planning/scoping','application/settlement development',
               'agency review','license implementation')
phase_break_ddate  = decimal_date(phase_break_date)
period_break_ddates = seq(decimal_date(mdy('05/01/2000')),decimal_date(mdy('11/01/2014')),0.5)


talkers = read_csv('Input/scraped_data/participation_detail.csv')

meeting_master = read_csv('Input/scraped_data/meeting_master.csv')

attendance = read_csv('Input/scraped_data/attendance_summary.csv')

talkers = talkers %>% filter(!is.na(Verb.Type),!Verb.Type %in% drop_list)

#match(paste(talkers$Meeting,talkers$Subject_Match),paste(talkers_summary$Meeting,talkers_summary$Subject_Match))
# verb_in_attendance_names = unlist(lapply(str_to_title(talkers$Verb), 
#                     function(x) any(grepl(paste0(x,'$|','^',x),unique(attendance$Name)))))


#as.data.frame(table(talkers$Verb.Type)) %>% arrange(-Freq)
Rnum = 100
library(stringr)

verb_cats = c('all','Verbs of Communication','Verbs of Creation and Transformation','Verbs of Change of Possession','other')

tie_sets = lapply(verb_cats,function(x) {
  if (x=='all'){talkers %>%  mutate(Category = x)}
  else if (x!='all'&x!='other')
  {talkers %>% filter(Verb.Type==x) %>% mutate(Category = x)}
  else if (x=='other'){talkers %>% 
      filter(!Verb.Type%in% c('Verbs of Communication','Verbs of Creation and Transformation','Verbs of Change of Possession')) %>% 
      mutate(Category = x)}
})


interaction_time_summary = do.call(rbind,lapply(tie_sets,function(x) x %>% filter(!duplicated(rowid)) %>% group_by(Interval,Category,Phase) %>% summarize(interactions = n())))
interaction_time_summary$Phase_Start = interaction_time_summary$Phase-1
interaction_time_summary$Interval_Start = interaction_time_summary$Interval-1

library(ggthemes)
ggplot(interaction_time_summary) + 
  geom_rect( aes(NULL, NULL, xmin = Interval_Start, xmax = Interval, fill = Phase), 
             ymin = -Inf, ymax = Inf) +
  geom_path(aes(x=Interval,y=interactions,colour=Category)) +
  scale_x_continuous(name = '6 month intervals',expand=c(0,0),limits=c(0,30),breaks=c(7,10,18),
                     labels=gsub(' .*','',date_decimal(period_break_ddates[c(7,10,18)])))+
  scale_fill_gradient(guide = 'legend',low = "gray50", high = "gray95",name = 'Phase',labels=c('Planning/scoping','Application/settlement',
                                                                                               'Agency review','Implementation')) + 
  scale_y_continuous(name='# observed actions') +
  scale_color_colorblind(name='Verb type',labels=c('All','Other','Change of possession','Communication','Creation/transformation')) +
  theme_tufte(ticks=F) + theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=18),
                               axis.title.x=element_text(size=18),legend.title=element_text(size=18,hjust = .5),
                               axis.text.x=element_text(size=12),legend.position = c(0.80,0.7),
                               legend.text=element_text(size=16)) +
  ggplot2::annotate('text',x=3.5,y=900,label='Planning/scoping',size=6) +
  ggplot2::annotate('text',x=8.5,y=850,label='Application',size=6)  +
  ggplot2::annotate('text',x=14,y=900,label='Review',size=6) +
  ggplot2::annotate('text',x=24,y=900,label='Implementation',size=6)+
  guides(fill=FALSE)


library(googlesheets)  
library(zoo)
hand_base = read_csv('https://docs.google.com/spreadsheets/d/1ZdFzmCqx1CxRfsS-9v-Q4Mece4rfxJ4BnsD2F2RyNaI/pub?output=csv')%>%
  mutate(`Meeting name` = na.locf(`Meeting name`),Filename = na.locf(Filename),Date = na.locf(Date),
         First = na.locf(First),Last = na.locf(Last),Organization = na.locf(Organization)) %>%
  mutate(Filename = gsub('\\.pdf','',Filename)) %>% mutate(Participation_binary = ifelse(!is.na(Participation),1,0))


talkers_in_hand = talkers %>% filter(Meeting %in% hand_base$Filename)
attendance_in_hand = attendance %>% filter(Meeting %in% hand_base$Filename)
hand_att = hand_base %>% group_by(Filename,Date) %>% filter(!duplicated(paste(First,Last))) %>% summarise(hand_att_count = n()) %>% arrange(hand_att_count)
hand_att$meeting_plot_order = 1:nrow(hand_att)
machine_att = attendance_in_hand %>% group_by(Meeting) %>% filter(!duplicated(Name)) %>% summarise(machine_att_count = n())
machine_att$meeting_plot_order = hand_att$meeting_plot_order[match(machine_att$Meeting,hand_att$Filename)]

ggplot() + 
  geom_point(data = machine_att,aes(x=meeting_plot_order,y=machine_att_count,colour='machine',shape=19),size=3) + 
  geom_point(data = hand_att,aes(x=meeting_plot_order,y=hand_att_count,colour='hand',shape=1),size=3) + 
  theme_tufte(ticks=F) + scale_color_colorblind(name='Coding method',labels=c('Hand','Machine')) +
  scale_x_continuous(name = 'Sample of 49 meetings ordered by hand-coded # of attendees') +
  scale_y_continuous(name='# of attendees (by coding method)') + 
  scale_shape_identity()+
  theme(legend.position = c(0.8,0.25), axis.text.x=element_blank(),axis.text.y = element_text(size=18),
        axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title = element_text(size=18))+
  guides(colour = guide_legend(override.aes = list(shape=c(1,19))))
  

test = full_join(hand_att %>% rename(Meeting = Filename),machine_att) %>%
  mutate(diff = hand_att_count-machine_att_count) %>% arrange(diff)
mean(test$diff)/mean(test$hand_att_count)


hand_talkers = hand_base %>% group_by(Filename,Date) %>% filter(Participation_binary==1) %>%
  summarise(hand_talk_count = n()) %>% arrange(hand_talk_count)
hand_talkers$meeting_plot_order = 1:nrow(hand_talkers)

machine_talkers = talkers_in_hand %>% group_by(Meeting) %>% filter(!duplicated(rowid)) %>% summarise(machine_talk_count = n())
machine_talkers$meeting_plot_order = hand_talkers$meeting_plot_order[match(machine_talkers$Meeting,hand_talkers$Filename)]


library(scales)
#show_col(colorblind_pal()(3))

blue_col = '#56B4E9'
orange_col = '#E69F00'

ggplot() + 
  geom_point(data = machine_talkers,aes(x=meeting_plot_order,y=machine_talk_count,colour='machine'),size=3) + 
  geom_point(data = hand_talkers,aes(x=meeting_plot_order,y=hand_talk_count,colour='hand'),size=3,pch=1) + 
  theme_tufte(ticks=F) + 
  scale_color_manual(name='Coding method',labels=c('Hand','Machine'),values=c('black',blue_col)) +
  scale_x_continuous(name = 'Sample of 49 meetings ordered by hand-coded # of actions') +
  scale_y_continuous(name='# of observed actions (by coding method)') + 
  theme(legend.position = c(0.9,0.15), axis.text.x=element_blank(),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18),legend.text=element_text(size=18),
        legend.title = element_text(size=18))+
  guides(colour = guide_legend(override.aes = list(shape=c(1,19))))

test = full_join(hand_talkers %>% rename(Meeting = Filename),machine_talkers) %>%
  mutate(diff = hand_talk_count-machine_talk_count) %>% arrange(diff)
mean(test$diff)


hand_talkers_uq = hand_base %>% group_by(Filename,Date) %>% filter(Participation_binary==1) %>%
  mutate(Name = paste(First,Last,sep=' ')) %>% filter(!duplicated(paste(Name,Filename))) %>%
  summarise(hand_talk_uq_count = n()) %>% arrange(hand_talk_uq_count)
hand_talkers_uq$meeting_plot_order = 1:nrow(hand_talkers_uq)

machine_talkers_uq = talkers_in_hand %>% group_by(Meeting) %>% filter(!duplicated(rowid),!duplicated(paste(Subject_Match,Meeting))) %>% 
  summarise(machine_talk_uq_count = n())
machine_talkers_uq$meeting_plot_order = hand_talkers_uq$meeting_plot_order[match(machine_talkers_uq$Meeting,hand_talkers_uq$Filename)]


green_col = '#009E73'
ggplot() + 
  geom_point(data = machine_talkers_uq,aes(x=meeting_plot_order,y=machine_talk_uq_count,colour='machine'),size=3) + 
  geom_point(data = hand_talkers_uq,aes(x=meeting_plot_order,y=hand_talk_uq_count,colour='hand'),size=3,pch=1) + 
  theme_tufte(ticks=F) + 
  scale_color_manual(name='Coding method',labels=c('Hand','Machine'),values=c('black',green_col)) +
  scale_x_continuous(name = 'Sample of 49 meetings ordered by hand-coded # of actions') +
  scale_y_continuous(name='# unique participating actors (by coding method)') + 
  theme(legend.position = c(0.9,0.15), axis.text.x=element_blank(),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18),legend.text=element_text(size=18),
        legend.title = element_text(size=18))+
  guides(colour = guide_legend(override.aes = list(shape=c(1,19))))

 



