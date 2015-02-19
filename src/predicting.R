## this is predicting script
print(sel.vvs)

core.dt <- complete.dt[,true:=rank][,.(q.id,Stock,Broker,true)]

eps.tr <- acast(unique(core.dt,by=c('q.id','Broker','Stock')),q.id~Broker~Stock,value.var = 'true')

broker.vvs <- acast(melt(unique(complete.dt[,broker.vvs.f(Est,act.eps),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)


stocks <- sort(intersect(intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]]),unique(unlist(lapply(market.list,function(m){m$stock.names})))))

stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(eps.tr)[1])

vvs.names <- dimnames(stock.vvs)[[3]]
cach('vvs.names')
#system.time(baseline.rankings <- baseline.rankings.f(eps.tr[,,stocks],4))
#dimnames(baseline.rankings)[[1]] <- dimnames(eps.tr)[[1]]
#pred.r<- predict.ranking.script.f(methods,stocks,stock.vvs,eps.tr[,,stocks],rank.parameters)

#pred.r <- nominal.ranking.script.f(methods,stocks,stock.vvs,baseline.rankings[,,,'true'],rank.parameters,8)


#all.rankings <- abind(baseline.rankings[,,stocks,],aperm(pred.r,c(2,1,4,3)),along=4)


#ranked.eps.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

#eps.accu <- melt(dcast.data.table(ranked.eps.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,methods)],id.vars=c('q.id','Stock'),na.rm=T)

#cache('ranked.eps.dt')

#rm(core.dt,broker.vvs,eps.tr,stocks,stock.vvs,pred.r,all.rankings)

#accuracy <- apply(all.rankings,c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
#apply(accuracy,1,mean,na.rm=T)


###rolling experiments

roll.baselines <- roll.baselines.f(eps.tr[,,stocks],rank.parameters[[4]])
system.time(pred.r <- roll.ranking.f(methods,stocks,stock.vvs[,,sel.vvs],eps.tr[,,stocks],rank.parameters))

all.rankings <- abind(aperm(roll.baselines,c(4,1,2,3)),aperm(pred.r,c(2,1,4,3)),along=4)
dimnames(all.rankings)[[4]] <- c(baselines,methods)
dimnames(all.rankings)[[1]] <- rollapply(dimnames(eps.tr)[[1]],rank.parameters[[4]]+1,last)

ranked.eps.dt <- setkey(setnames(data.table(reshape2::melt(all.rankings)),c('q.id','Broker','Stock','Method','rank'))[,q.id:=as.yearqtr(q.id)],q.id,Stock,Broker,Method)

eps.accu <- melt(dcast.data.table(ranked.eps.dt,q.id+Stock+Broker~Method,value.var='rank')[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c(baselines,methods)],id.vars=c('q.id','Stock'),na.rm=T)
#accuracy <- apply(baseline.rankings[,,stocks,],c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })

#
# 
# 
# ### mac book: Ranking algorithm ~  2 hours 43 min
# ### new imac ~ 4300 seconds (1 hour and 10 min)
# #load('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/cache/array.all.vvs.RData')
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/lib/aux.functions.R')
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/munge/02-state-variables.R')
# delta <- 1
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/munge/01-data.q.ret.R')
# 
# 
# rank.parameters <- c(n=10,diff.lag=1,sd.lag=8)
# methods<-c('raw','1diff','random','roll.sd')
# 
# ### mac book: Ranking algorithm ~  2 hours 43 min
# ### new imac ~ 4300 seconds (1 hour and 10 min)
# #load('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/cache/array.all.vvs.RData')
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/lib/aux.functions.R')
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/munge/02-state-variables.R')
# delta <- 1
# source('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/munge/01-data.q.ret.R')
# 
# broker.vvs.f <- function(forecasts, actual)
# {
#         sq.error <- (mean(actual) - mean(forecasts))^2
#         dispersion <- var(forecasts)
#         num.forcst <- length(forecasts)
#         sq.error[is.na(sq.error)] <- 0
#         uncertainty <- sq.error+dispersion
#         inf.assym <- 1-(sq.error - dispersion/num.forcst )/((1-1/num.forcst)*dispersion+sq.error)
#         list(uncertainty=uncertainty,assym=inf.assym,dispersion=dispersion)
# }
# 
# broker.vvs <- acast(melt(unique(complete.dt[,broker.vvs.f(Est,act.eps),by=list(fis.q,Stock)],by=c('fis.q','Stock')),id.vars = c('fis.q','Stock'),measure.vars = c('uncertainty','assym','dispersion')),fis.q~Stock~variable)
# 
# eps.tr <- acast(unique(complete.dt,by=c('q.id','Broker','Stock')),q.id~Broker~Stock,value.var = 'rank')
# 
# rank.parameters <- c(n=10,diff.lag=1,sd.lag=8)
# methods<-c('raw','1diff','random','roll.sd')
# stocks=intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]])
# baseline.rankings <- baseline.rankings.f(eps.tr[,,stocks],1)
# stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(eps.tr)[1])
# 
# system.time(pred.r<- predict.ranking.script.f(methods,stock.vvs,eps.tr[,,stocks],rank.parameters))
# 
# system.time(pred.r<- predict.ranking.script.f(methods,stocks,stock.vvs,eps.tr[,,stocks],rank.parameters))
# 
# dimnames(pred.r)[[2]] <- dimnames(eps.tr)[[1]][4:84]
# all.rankings <- abind(baseline.rankings[3:83,,,],aperm(pred.r,c(2,1,4,3)),along=4)
# ProjectTemplate::cache('all.rankings')
# # 
#accuracy <- apply(all.rankings,c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
# 
#apply(accuracy,1,mean,na.rm=T)
# dt.accuracy <- setnames(data.table(melt(accuracy,na.rm=T)),c('Method','q.id','Stock','Accuracy'))[,meanAcc:=mean(Accuracy),by=list(q.id,Method)]
# ggplot(unique(dt.accuracy,by=c('Method','q.id')),aes(x=as.Date(as.yearqtr(q.id)),y=meanAcc,group=Method,color=Method))+geom_line()+geom_point()+theme_bw()+geom_smooth(method='loess',se=F,size=1)+ggtitle('Average per period ranking accuracy: values and loess smoothing (EPS ranking)')+xlab('Quarters')+ylab('meanAccuracy')+scale_color_brewer(palette='Set1')
# ggsave('graphs/accuracy.plot.pdf',h=8,w=11)
# ###baselines for BL trade; length of 43 quarters,start at 42
# #load('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/cache/market.set.RData')
# stocks=intersect(dimnames(pred.r)[[3]],market.set[,Stock])
# bl.baseline.rankings <- baseline.rankings.f(eps.tr[,,stocks],42)
# bl.all.rankings <- abind(bl.baseline.rankings[3:42,,,],aperm(pred.r[,42:81,stocks,],c(2,1,3,4)),along=4)
# 
# ProjectTemplate::cache('bl.all.rankings')
# bl.accuracy <- apply(bl.all.rankings,c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
# 
# apply(bl.accuracy,1,mean,na.rm=T)
# 
# bl.dt.accuracy <- setnames(data.table(melt(bl.accuracy,na.rm=T)),c('Method','q.id','Stock','Accuracy'))[,meanAcc:=mean(Accuracy),by=list(q.id,Method)]
# ggplot(unique(bl.dt.accuracy,by=c('Method','q.id')),aes(x=q.id,y=meanAcc,group=Method,color=Method))+geom_line()+geom_point()+theme_bw()+geom_smooth(method='loess',se=F,size=1)+ggtitle('Average per period ranking accuracy: values and loess smoothing (EPS ranking)')+xlab('Quarters')+ylab('meanAccuracy')+scale_color_brewer(palette='Set1')
# ggsave('graphs/bl.accuracy.plot.pdf',h=8,w=11)
