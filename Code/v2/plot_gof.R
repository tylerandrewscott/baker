rm(list=ls())
library(statnet)
library(btergm)
library(tidyverse)
library(knitr)
library(texreg)
library(forcats)
load('Scratch/temp_btergm_results2.RData')


#rm(list=ls()[!grepl('h_[a-z]{3}_list',ls())])
gc(reset = TRUE)

library(texreg)
library(broom)

library(texreg)
library(broom)
m1@boot$R = sum(rowSums(is.na(m1@boot$t))==0)
m1@boot$t = m1@boot$t[rowSums(is.na(m1@boot$t))==0,]
m2@boot$R = sum(rowSums(is.na(m2@boot$t))==0)
m2@boot$t = m2@boot$t[rowSums(is.na(m2@boot$t))==0,]

names(m1@coef) = fct_recode(names(m1@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]")
summary(m2)
names(m2@coef) = fct_recode(names(m2@coef),
                            `Edges` = "edges" ,
                            `GWESP (a = 2)` = "gwesp.fixed.2"    ,                        
                            `GWID (a = 2)` = "gwidegree"      ,                          
                            `Meetings attended` = "nodecov.Meetings_Attended",
                            `Mandatory (sender)` = "nodeofactor.Mandatory.1"  ,  
                            `Utility (sender)` = "edgecov.utility_list_odegree[[i]]",
                            `Phase 2` = "edgecov.timecov1[[i]]",
                            `Phase 3` = "edgecov.timecov2[[i]]",
                            `Phase 4` = "edgecov.timecov3[[i]]",
                            `Tie stability` = "edgecov.autoreg_list_prior[[i]]",
                            `Stability * Phase 2` = "edgecov.timecov4.autoreg_list_prior[[i]]",
                            `Stability * Phase 3` = "edgecov.timecov5.autoreg_list_prior[[i]]",
                            `Stability * Phase 4` = "edgecov.timecov6.autoreg_list_prior[[i]]")

# gof_m1 = gof(m1,statistics=c(rocpr), nsim = 50,mcmc=TRUE,MCMC.interval=12000,
#            parallel='multicore',ncpus=8)
gof_m1 = h_gof_list[[1]]
par(mar = c(2.2, 2, 0.3, 1) + 0.1,mgp=c(1.2,.5,0),mfrow=c(1,1))
plot(gof_m1$`Tie prediction`$pr,lwd=1,avg='none',rgraph=FALSE,col='grey40')
plot(gof_m1$`Tie prediction`$pr,type='p',add=T,pch=19,cex=.3,lwd=1,col='grey30')
plot(gof_m1$`Tie prediction`$pr.rgraph,add=T,col='grey80',lwd=1)
legend(x = 0.1,0.4,legend = c('Model 1 (per time period)','Random graph (per time period)'),
        lty=c(1,1),col=c('grey40','grey80'),pch=c(19,NA))

par(mfrow=c(2,2))
plot(h_gof_list[[1]]$`Edge-wise shared partners`)
plot(h_gof_list[[1]]$`Edge-wise shared partners`)
plot(h_gof_list[[1]]$Indegree)
plot(h_gof_list[[1]]$Outdegree)








gof2_m1 = gof(m1,statistics=c(ideg,odeg,geodesic,esp), nsim = 50,mcmc=TRUE,MCMC.interval=12000,
              parallel='multicore',ncpus=8)

par(mar = c(2.2, 2, 0.3, 1) + 0.1,mgp=c(1.2,.5,0),mfrow=c(2,2))
plot(h_gof_list[[2]],main=NULL, median.lwd = .5,mean.lwd=.5)


gof_m2 = h_gof_list[[2]]
par(mar = c(2.2, 2, 0.3, 1) + 0.1,mgp=c(1.2,.5,0),mfrow=c(1,1))
plot(gof_m2$`Tie prediction`$pr,lwd=1,avg='none',rgraph=FALSE,col='grey40')
plot(gof_m2$`Tie prediction`$pr,type='p',add=T,pch=19,cex=.3,lwd=1,col='grey30')
plot(gof_m2$`Tie prediction`$pr.rgraph,add=T,col='grey80',lwd=1)
legend(x = 0.1,0.3,legend = c('Model 1 (per time period)','Random graph (per time period)'),
       lty=c(1,1),col=c('grey40','grey80'),pch=c(19,NA))


gof2_m1 = gof(m1,statistics=c(istar,ostar,geodesic,esp), nsim = 50,mcmc=TRUE,MCMC.interval=12000,
             parallel='multicore',ncpus=8)
plot(gof3_m1)

ggplot() + 
geom_line(aes(x=gof_m1$`Tie prediction`$roc.rgraph@x.values,
              y=gof_m1$`Tie prediction`$roc.rgraph@y.values))


gc(reset = T)
#gdata::ll(unit='MB')



plot(test$Indegree$stats)
test = test$`Dyad-wise shared partners`
plot(test$Indegree)
test$stats
plot(test$`Dyad-wise shared partners`,)
summary(net_list[[11]]~istar(1))
summary(net_list[[10]]~ideg)
gof2 = gof(m1,statistics=c(istar,kstar), nsim = 50,mcmc=TRUE,
           parallel='multicore',ncpus=8)

par(mfrow=c(1,1))
plot(gof2$`Incoming k-star`,mean.lty = 4)
plot.boxplot <- function(x, relative = TRUE, transform = function(x) x, 
                         xlim = NULL, main = x$label, xlab = x$label, ylab = "Frequency", 
                         border = "darkgray", boxplot.lwd = 0.8, outline = FALSE, median = TRUE, 
                         median.col = "black", median.lty = "solid", median.lwd = 2, mean = TRUE, 
                         mean.col = "black", mean.lty = "dashed", mean.lwd = 1, ...) {
  
  
gof3 = gof(m1,statistics=c(rocpr), nsim = 50,mcmc=TRUE,
           parallel='multicore',ncpus=8,MCMC.interval=10000)
plot(gof3)
gof2$`Incoming k-star`
gof3 = gof(m1,statistics=c(rocpr), nsim = 50,mcmc=TRUE,
           parallel='multicore',ncpus=8)

net_test = lapply(tie_within_attendees,function(x) as.network(x,directed=T,matrix.type='adjacency'))
form1
tmod = btergm(net_list[-1]~edges+edges +  nodecov("Meetings_Attended") + 
                ttriple + ctriple + transitiveties  +
               # gwidegree(gwid_decay, fixed = T) + gwodegree(gwid_decay, fixed = T) + 
                nodeofactor("Mandatory") + edgecov(utility_list_odegree) + 
                timecov(transform = function(t) t),R=100)
tgof = gof(tmod,statistic=c(rocpr),nsim=50,mcmc=TRUE,parallel='multicore',ncpus=8,MCMC.interval=10000)
plot(tgof)

summary(tmod)
summary(tmod)



summary(tmod)
gof3 = gof(m1,statistics=c(rocpr), nsim = 50,mcmc=TRUE,
           parallel='multicore',ncpus=8,target=net_test)

btergm::plot.boxplot(gof2$Indegree)
plot.boxplot <- function(x, relative = TRUE, transform = function(x) x, 
                         xlim = NULL, main = x$label, xlab = x$label, ylab = "Frequency", 
                         border = "darkgray", boxplot.lwd = 0.8, outline = FALSE, median = TRUE, 
                         median.col = "black", median.lty = "solid", median.lwd = 2, mean = TRUE, 
                         mean.col = "black", mean.lty = "dashed", mean.lwd = 1, ...)


  c(bottom, left, top, right) 



par(mar = c(2.2, 2, 0.3, 1) + 0.1,mgp=c(1.2,.5,0),mfrow=c(1,1))
gof3$`Tie prediction`$pr@y.values
plot(gof3$`Tie prediction`$pr,main=NULL,type='p')
plot(gof3$`Tie prediction`$pr.rgraph,main=NULL,col='grey',add=T,type='p')


gof1$Outdegree$stats


gof(net_list[[10]], covariates, coef, target = NULL,
    nsim = 100, mcmc = FALSE, MCMC.interval = 1000,
    MCMC.burnin = 10000, parallel = c("no", "multicore", "snow"),
    ncpus = 1, cl = NULL, statistics = c(dsp, esp, deg, ideg,
                                         geodesic, rocpr, walktrap.modularity),
    verbose = TRUE)


form1





nsim = 100
mygof <- function(model, number) {
  gf <- gof(model, nsim = nsim, statistics = c(nsp, b1deg, b2deg, geodesic,
                                               b1star, b2star, pr), ncpus = cores, parallel = "multicore") 
  103
  temp <- gf[1:6]
  class(temp) <- "gof"
  pdf(paste0("gof.", number, ".pdf"), width = 9, height = 6)
  plot(temp)
  dev.off()
  return(gf)
} 


?btergm::gofplot
h_gof_list[[1]]$Outdegree$stats
par(mfrow=c(2,2))
test = h_gof_list[[1]]$`Outdegree`$stats
head(test)
library(ggplot2)
ggplot(test[-1,],aes(y=median,x=2:nrow(test))) + geom_path()

head(test)
ggplot(test,aes(y=median,x=1:nrow(test))) + geom_path()
ggplot(test,aes(y=median,x=1:nrow(test))) + geom_path()
?btergm::gof


library(xergm)

temp = m1
str(temp@response)
str(temp)

temp@data$networks = temp@data$networks[seq(5,25,5)]

net_list = net_list[seq(5,25,5)]
gof(temp,nsim=10)

m1@data$networks[c()]
## S4 method for signature 'network'



gof(object, target = NULL, formula = getformula(object), 
    nsim = 100, MCMC.interval = 1000, MCMC.burnin = 10000, 
    parallel = c("no", "multicore", "snow"), ncpus = 1, cl = NULL, 
    statistics = c(dsp, esp, deg, ideg, geodesic, rocpr, 
                   walktrap.modularity), verbose = TRUE, ...)




test = btergm::gof(m1,
              target = as.sociomatrix(net_list[[5]]),statistics=c(triad.directed))


test = btergm::gof(object=lapply(seq(5,25,5),function(x) as.sociomatrix(net_list[[x]])),target = ,
           statistics=c(triad.directed),matrix.type=='adjacency')





test = gof(m1,statistic=c(triad.directed))
test
