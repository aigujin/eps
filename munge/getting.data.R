sectors<-c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','IT','Materials','Telecom Services','Utilities')

###Get EPS data and convert it to DT
eps.dt.tmp <- rbindlist(mclapply(sectors,function(sec)
{
  stocks <- list.files(paste(data.stock.dir(sec),'EPS',sep='/'))
  dt <- lapply(stocks,function(i){
    stock.dt <- fread(paste(data.stock.dir(sec),'EPS',i,sep='/'))
    stock.dt[,Stock:=strsplit(i,'.csv')]})
  sec.dt <- rbindlist(dt)
  sec.dt[,Sector:=sec]
},mc.cores=4))

library(lubridate)

eps.dt <- unique(setkey(setnames(na.omit(eps.dt.tmp),5,'EstPeriod')[,fis.q:=as.yearqtr(paste(str_sub(EstPeriod,4,7),paste('Q',str_sub(EstPeriod,1,1),sep=''),sep=' '))][,year:=format(as.Date(fis.q),format='%Y')][,EstDate:=as.Date(EstDate,origin='1899-12-30')][,q.id:=fis.q],EstDate,Broker,Stock))

cache('eps.dt')
###average means by different varaiables, per sector
#eps.dt[,.N,by=list(q.id,Sector,Broker)][,descriptive.f(N,F),by=Sector][V1=='mean']
##Core brokers
eps.core.dt <- unique(setkey(unique(eps.dt,fromLast=T,by=c('Stock','Broker','q.id'))[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,clean.s:=.N>=3,by=list(q.id,Stock)][(clean.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)],q.id,Stock,Sector),fromLast=T,by=c('q.id','Broker','Stock'))


#ggplot(eps.core.dt,aes(x=as.Date(fis.q),y=Est,group=as.Date(fis.q)))+geom_boxplot()

### Getting actual EPS. Fiscal quarters are from DS db
act.eps.tmp <- setkey(rbindlist(mclapply(sectors,function(sec){
  vvs <- c('act.eps.csv','stock.prices.csv')
  dt <- do.call(cbind,lapply(vvs,function(i){
    
    vvs.m <- read.csv2(paste(data.stock.dir(sec),'Acc.Variables',i,sep='/'),sep=';',header=T)
    vvs.dt <- data.table(vvs.m)[,q.id:=.I]
  setkey(melt(vvs.dt,id.vars='q.id',variable.name = 'Stock',value.name = strsplit(i,'.csv')[[1]]),q.id,Stock)
    }))
dt[,unique(colnames(dt)),with=F][,Sector:=sec]
},mc.cores=4)),q.id,Stock,Sector)


act.eps.dates <- setkey(rbindlist(mclapply(sectors,function(sec){
  vvs <- c('act.eps.date.csv')
  dt <- do.call(cbind,lapply(vvs,function(i){
    
    vvs.m <- read.csv2(paste(data.stock.dir(sec),'Acc.Variables',i,sep='/'),sep=';',header=T)
    vvs.dt <- data.table(vvs.m)[,q.id:=.I]
  setkey(melt(vvs.dt,id.vars='q.id',variable.name = 'Stock',value.name = strsplit(i,'.csv')[[1]]),q.id,Stock)
    }))
dt[,unique(colnames(dt)),with=F][,Sector:=sec]
},mc.cores=4))[,epsDate:=as.Date(act.eps.date,format='%d-%m-%Y')][,repQ:=as.yearqtr(epsDate)],Stock,repQ)

### EPS are for fiscal quarters, but dates for actEPS are in calendar quarter. Need to reconcile
load(project.dir('EPS/fisc.year.ds.RData'))
### fina end fiscal year for each stock
fis.ye <- setkey(data.table(melt(fisc.year.ds[1:84,]))[,':='(fs.end.q=as.yearqtr(as.Date(value)))][,Stock:=Var2],Stock,fs.end.q)

### Since we know the fiscal year-end (fiscal Q4), we can manualy backward assign  the rest of the fiscal quarters(Q3,Q2,Q1)

fis.q <- setkey(setnames(unique(fis.ye)[,as.yearqtr(fs.end.q-seq(0,0.75,0.25)),by=list(Stock,fs.end.q)],'V1','calQ')[,fis.q:=as.yearqtr(paste(format(fs.end.q,'%Y'),c('Q4','Q3','Q2','Q1'))),by=list(Stock,fs.end.q)],Stock,fis.q)


act.dt <- setkey(na.omit(setkey(na.omit(fis.q)[act.eps.dates,allow.cartesian=T],q.id,Stock,Sector)[act.eps.tmp,allow.cartesian=T]),fis.q,Stock,Sector)


complete.dt <- unique(na.omit(act.dt[eps.core.dt,allow.cartesian=T])[,rank:={pmafe<-abs(Est-act.eps)/mean(abs(Est-act.eps));rank(pmafe)},by=list(fis.q,Stock)][,':='(daysEPS=epsDate-EstDate,PE=stock.prices/Est,EP=Est/stock.prices)][,year:=format(as.Date(fis.q),format='%Y')][,q.id:=fis.q],by=c('Broker','Stock','q.id'))

cache('complete.dt')

rm(act.dt,fis.q,fis.ye,act.eps.dates,act.eps.tmp,eps.core.dt,eps.dt,eps.dt.tmp,fisc.year.ds)

#ggplot(complete.dt,aes(x=as.Date(q.id),y=EP,group=as.Date(q.id)))+geom_boxplot()+theme_bw()

#eps.tr <- acast(unique(complete.dt,by=c('q.id','Broker','Stock')),q.id~Broker~Stock,value.var = 'rank')

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
#   sq.error <- (mean(actual) - mean(forecasts))^2
#   dispersion <- var(forecasts)
#   num.forcst <- length(forecasts)
#   sq.error[is.na(sq.error)] <- 0
#   uncertainty <- sq.error+dispersion
#   inf.assym <- 1-(sq.error - dispersion/num.forcst )/((1-1/num.forcst)*dispersion+sq.error)
#   list(uncertainty=uncertainty,assym=inf.assym,dispersion=dispersion)
# }
# 
# broker.vvs <- acast(melt(unique(complete.dt[,broker.vvs.f(Est,act.eps),by=list(fis.q,Stock)],by=c('fis.q','Stock')),id.vars = c('fis.q','Stock'),measure.vars = c('uncertainty','assym','dispersion')),fis.q~Stock~variable)
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
# #ProjectTemplate::cache('all.rankings')
# # 
# accuracy <- apply(all.rankings,c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
# 
# apply(accuracy,1,mean,na.rm=T)
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
