require(ggplot2)
require(network)
require(igraph)
require(sna)
require(ggnet)
require(ergm)
require(intergraph)
require(RColorBrewer)


temp = joint_df %>% filter(Attend!=0)
mat <- as.matrix(table(temp$Name,temp$Meeting))
bipnet <- network(mat,bipartite=T,matrix.type='incidence',directed=F)

gr <- graph_from_incidence_matrix(mat)

bipnet %v% 'Topic' <- meeting_master$Topic[match(network.vertex.names(bipnet),meeting_master$Meeting)]
bipnet %v% 'Actor_Type' <- meeting_master$Topic[match(network.vertex.names(bipnet),meeting_master$Meeting)]

attendance[attendance$Name %in% c('Jim Johnston'),]

attendance[is.na(attendance$Org),] %>% group_by(Name) %>% summarise(obs = n()) %>% arrange(-obs) %>%
  filter(obs<2)
joint_df %>% filter(Name == 'Lou Ellyn Jones',Attend==1)


'Andrew Harris',

joint_df %>% filter(Name == 'Chick Sweeney',Attend==1)
grep('Freet',attendance$Name,value=T)



