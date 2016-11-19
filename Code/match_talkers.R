library(tidyverse)
library(stringr)
talkers = read_csv('Input/pdfs/extraction/named_entities.csv') %>% 
  dplyr::select(-X1) %>%
  filter(!grepl('[A-Z]{4}',Subject),!grepl('^[a-z]',Subject)) %>%
  mutate(Subject = gsub('\\.$','',Subject)) %>%
  mutate(Meeting = gsub('\\.txt$','',Meeting)) %>%
  filter(!grepl('Upper Baker',Subject)) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))

attendance = read_csv('Input/scraped_data/temp_cleaned_data.csv') %>% dplyr::select(-X1) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))

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

base_matrix = matrix(0,ncol=total_people,nrow=total_people)
colnames(base_matrix) = rownames(base_matrix) = meeting_attendees

net_array = replicate(length(unique(year_engagement_edges$Year)),base_matrix)

dimnames(net_array)[[3]] = sort(unique(as.character(year_engagement_edges$Year)))

placement = cbind(match(year_engagement_edges$Participant,meeting_attendees),
match(year_engagement_edges$Attendee,meeting_attendees),
match(year_engagement_edges$Year,dimnames(net_array)[[3]]))
net_array[placement] = year_engagement_edges$direct_engagement



library(statnet)
#net_list = lapply(1:dim(net_array)[3],function(x) as.network(net_array[,,x],directed=T,matrix.type='adjacency'))




library(btergm)

btergm(net_list~edges + mutual,R = 100)

 
test_plot = placement %>% as.data.frame(.) %>% filter(V3==3) %>% mutate(V1 = V1-1,V2 = V2-1)
# Load package
library(networkD3)

simpleNetwork(net_list[[1]])

networkData <- test_plot
data("MisLinks")




forceNetwork(Links = test_plot, Source = 'V1',Target = 'V2',Nodes = data.frame(people = 0:total_people-1,group=1),NodeID = 'people',Group='group')

# Plot
simpleNetwork(networkData)


?networkDynamic

class(net_list)



?network

