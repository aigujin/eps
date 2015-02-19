rm(list=ls())
setwd('~/Dropbox/workspace/Projects/EPS/')

library(ProjectTemplate)
load.project()
#require(labelRank)
baselines <- c('true','naive','default');methods<-c('raw','diff','random','roll.sd');delta<-1L;rank.parameters <- c(n=100L,diff.lag=1L,sd.lag=8L,roll.p=4L)
#source('~/Dropbox/workspace/Projects/Nbr/lib/mdlp.rank.R')

### State var. data: ~118 sec
#system.time(source('~/Dropbox/workspace/Projects/BL-strategies/munge/02-state-variables.R'))

### ~ 407 sec
#system.time(source('munge/getting.data.R'))


system.time(source('~/Dropbox/workspace/Projects/BL-strategies/munge/01-data.q.ret.R'))

### Predicting ~ 2400 sec
sel.vvs <- vvs.names[c(1:5)]

system.time(source('src/predicting.R'))

eps.accu[,mean(value),by=.(variable)]

ggplot(eps.accu[,mean(value),by=.(q.id,variable)],aes(x=as.Date(q.id),y=V1,group=variable,color=variable))+geom_line(size=0.5,alpha=0.7)+geom_smooth(method='loess',se=F,size=1)+theme_bw()
