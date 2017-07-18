
networks <- list()
for(i in 1:10){            # create 10 random networks with 10 actors
  mat <- matrix(rbinom(100, 1, .25), nrow = 10, ncol = 10)
  diag(mat) <- 0           # loops are excluded
  nw <- network(mat)       # create network object
  networks[[i]] <- nw      # add network to the list
}

covariates <- list()
for (i in 1:10) {          # create 10 matrices as covariate
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  covariates[[i]] <- mat   # add matrix to the list
}

uts = c(1,3,7)





utility = c(1,0,1,0,0,0,1,0,0,0)

utility_list = lapply(1:10,function(i) replicate(10,utility))

rowSums(utility_list[[1]])
rowSums(utility_list_odegree[[1]])

for (i in 1:length(networks))
{network::set.vertex.attribute(networks[[i]],attrname = 'Utility',value=utility)}


test1 = btergm(networks ~ edges + mutual + nodeocov('Utility')+ timecov(transform=function(t) t),R=100)
test2 = btergm(networks ~ edges + mutual + edgecov(utility_list) + timecov(transform=function(t) t),R=100)
test3 = btergm(networks ~ edges + mutual + edgecov(utility_list) + timecov(transform=function(t) t)+
                 timecov(utility_list,transform=function(t) t) ,R=100)

temp = net_list[-1]
test1 = btergm(temp ~ edges + mutual + nodeocov('Utility')+ timecov(transform=function(t) t),R=100)
test2 = btergm(temp ~ edges + mutual + edgecov(utility_list_odegree) + timecov(transform=function(t) t),R=100)
test3 = btergm(temp ~ edges + mutual + edgecov(utility_list_odegree) + timecov(transform=function(t) t)+
                 timecov(utility_list_odegree,transform=function(t) t) ,R=100)

summary(test3)

library(texreg)

screenreg(list(test1,test2,test3))



utility_list_odegree


mtest = btergm(net_list[-1]~edges + mutual + isolates + nodeofactor('Utility'),R=100)
mtest1 = btergm(net_list[-1]~edges + mutual + isolates +edgecov(utility_list_odegree),R=100)
mtest2 = btergm(net_list[-1]~edges + mutual + isolates +
                  edgecov(utility_list_odegree) + 
                  timecov(x = utility_list_odegree,  minimum = 8,maximum=10, transform = function(t) 1)+
                  timecov(x = utility_list_odegree,  minimum = 11,maximum=18, transform = function(t) 1)+
                  timecov(x = utility_list_odegree,  minimum = 19,transform = function(t) 1) + 
                  timecov(transform=function(t) t),R=100)
mtest3 = btergm(net_list[-1] ~ edges  + mutual + isolates +  nodecov('Meetings_Attended') +
      ttriple +
    transitiveties + ctriple + 
  timecov(transform = function(t) t)+ 
    timecov(transform = function(t) t^2)+ 
    timecov(transform = function(t) t^3)+
  nodeofactor('Mandatory') + #nodeofactor('Utility') + 
    edgecov(utility_list_odegree) + 
    timecov(x = utility_list_odegree,  minimum = 8,maximum=10, transform = function(t) 1)+
    timecov(x = utility_list_odegree,  minimum = 11,maximum=18, transform = function(t) 1)+
    timecov(x = utility_list_odegree,  minimum = 19,transform = function(t) 1),R=100) 
mtest3@boot$R = sum(rowSums(is.na(mtest3@boot$t))==0)
mtest3@boot$t = mtest3@boot$t[rowSums(is.na(mtest3@boot$t))==0,]


num_meetings_attended$Any = ifelse(num_meetings_attended$Meetings_Attended==0,0,1)

emp = matrix(0,nrow=804,ncol=804)

ifelse()

num_meetings_attended7 = filter(num_meetings_attended,Interval==7)
head(num_meetings_attended7,23)

no_att = lapply(2:length(net_list), function(x)
abs(( (num_meetings_attended$Any[num_meetings_attended$Interval==x])[match(network.vertex.names(net_list[[x]]),
                                                                     num_meetings_attended$Name[num_meetings_attended$Interval==x])] %o%
    (num_meetings_attended$Any[num_meetings_attended$Interval==x])[match(network.vertex.names(net_list[[x]]),
                                                                       num_meetings_attended$Name[num_meetings_attended$Interval==x])])-1))

test_no_att = no_att[[10]]
modr = ergm(net_list[[10]]~edges + offset(edgecov(test_no_att)),offset.coef=-Inf)

modoff = ergm(net_list[[10]]~edges + mutual + offset(edgecov(test_no_att)),offset.coef=-Inf)

modnoff = ergm(net_list[[10]]~edges + mutual)
library(btergm)
spec = btergm(net_list[-1] ~ edges + mutual + isolates +nodecov('Meetings_Attended') +
                nodeofactor('Mandatory') +
                nodeocov('Utility') +          
                timecov(transform = function(t) t)+
                timecov(utility_list_odegree,function(t) t),R=100,returndata=T)
spec_check = glm(Y ~ .-1, family = "binomial", data = spec)
summary(spec_check)
round(cor(spec),2)

rowSums(utility_list_odegree[[1]])
colSums(utility_list_odegree[[1]])

net_list[-1] ~ edges  + mutual + isolates +  nodecov('Meetings_Attended') +
  gwidegree(gwid_decay,fixed=T) +  
  gwesp(gwesp_decay,fixed=T) + 
  timecov(transform = function(t) t)+ timecov(transform = function(t) t^2)+ timecov(transform = function(t) t^3)+
  nodeofactor('Mandatory') + #nodeofactor('Utility') 
  

summary(spec)

gof_modoff = gof(modoff,statistic=c(rocpr))
gof_modr = gof(modr,statistic=c(rocpr))
gof_modnoff = gof(modnoff,statistic=c(rocpr))

plot(gof_modoff)
plot(gof_modr)
plot(gof_modr$`Tie prediction`$pr@y.values[[1]]~gof_modr$`Tie prediction`$pr@x.values[[1]])
points(gof_modoff$`Tie prediction`$pr@y.values[[1]]~gof_modoff$`Tie prediction`$pr@x.values[[1]],col='red')
points(gof_modnoff$`Tie prediction`$pr@y.values[[1]]~gof_modnoff$`Tie prediction`$pr@x.values[[1]],col='blue')
?plot.network
plot.network(net_list[[10]],displayisolates = FALSE)

plot(h_gof_list[[1]],type='p')
lapply(h_gof_list,function(x) x$`Tie prediction`$auc.pr)
gof_modof
summary(modoff)
modoff@coef

x=30
network::get.vertex.attribute(net_list[[1]],attrname = 'Prior_Meetings_Attended')
num_meetings_attended$Any[num_meetings_attended$Interval==x][match(network.vertex.names(net_list[[x]]),
                                                                   num_meetings_attended$Name[num_meetings_attended$Interval==x])]


abs((num_meetings_attended$Any[num_meetings_attended$Interval==x][match(network.vertex.names(net_list[[x]]),
                                                                        num_meetings_attended$Name[num_meetings_attended$Interval==x])] %o%
       num_meetings_attended$Any[num_meetings_attended$Interval==x][match(network.vertex.names(net_list[[x]]),num_meetings_attended$Name[num_meetings_attended$Interval==x])]
  
  
  
op = num_meetings_attended7$Any %o% num_meetings_attended7$Any
abs(op[1:23,1:23] - 1)


for (i in 1:nrow(emp))
{
     for (j in 1:ncol(emp))
     {
      if ((network.vertex.names(net_list[[7]])[i] %in%    
         num_meetings_attended$Name[num_meetings_attended$Interval==7&num_meetings_attended$None]) |
        (network.vertex.names(net_list[[7]])[j] %in%    
         num_meetings_attended$Name[num_meetings_attended$Interval==7&num_meetings_attended$None]))
       {emp[i,j] = 1}
     }
     }
  
num_meetings_attended$None[match(network.vertex.names(net_list[[7]]),num_meetings_attended$Name[num_meetings_attended$Interval==7])]


match(network.vertex.names(net_list[[7]]),num_meetings_attended$Name[num_meetings_attended$Interval==7])]

rtest = btergm(net_list[-1]~edges+mutual, R=100)
rtest_gof = gof(rtest,statistic=c(rocpr))
plot(rtest_gof,type='p')

h_gof_list[[1]]$`Tie prediction`$pr@x.values[[30]]
h_gof_list[[1]]$`Tie prediction`$pr@y.values[[30]]
plot(h_gof_list[[1]])
plot(h_gof_list[[1]],type='p')

plot(mtest)
mtest4 = btergm(net_list[-1] ~ edges  + mutual + isolates +  nodecov('Meetings_Attended') +
                  ttriple +
                  transitiveties + ctriple,R=100)
                
tgof = gof(mtest3,nsim=100,statistic=c(rocpr))
tgof2 = gof(mtest4,nsim=100,statistic=c(rocpr))

plot(tgof)
plot(tgof2)
test8 = ergm(net_list[[5]] ~ edges  + mutual + gwesp(0.25,fixed=T) + nodecov('Meetings_Attended'))
gof8 = btergm::gof(test8,statistic=c(rocpr))

plot(gof8$`Tie prediction`$pr@y.values[[1]]~gof8$`Tie prediction`$pr@x.values[[1]],type='l')
plot(gof8$`Tie prediction`$pr@y.values[[1]]~gof8$`Tie prediction`$pr@x.values[[1]],add=T)

                    ttriple +
                    transitiveties + ctriple)               
                    
                
                  timecov(x = utility_list_odegree,function(t) t),
                
                
                R=100)
summary(mtest1)
