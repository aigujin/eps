### loadding data into work sapce

rm(list=ls())
setwd('~/Dropbox/workspace/Projects/EPS/')

library(ProjectTemplate)
load.project()
require(labelRank)
baselines <- c('true','naive','default');methods<-c('raw','1diff','random','roll.sd');delta<-1;n=1000;diff.lag=1;sd.lag=8;roll.p=20

### Declaring the variables use AAPL as an example stock
core.dt <- complete.dt[,true:=rank][,.(q.id,Stock,Broker,true)]

eps.tr <- acast(unique(core.dt,by=c('q.id','Broker','Stock')),q.id~Broker~Stock,value.var = 'true')

broker.vvs <- acast(melt(unique(complete.dt[,broker.vvs.f(Est,act.eps),by=list(q.id,Stock)],by=c('q.id','Stock')),id.vars = c('q.id','Stock'),measure.vars = c('uncertainty','assym','dispersion')),q.id~Stock~variable)

stocks <- sort(intersect(dimnames(eps.tr)[[3]],dimnames(array.all.vvs)[[2]]))

system.time(baseline.rankings <- baseline.rankings.f(eps.tr[,,stocks],1))
dimnames(baseline.rankings)[[1]] <- dimnames(eps.tr)[[1]]

stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(eps.tr)[1])
s='AAPL'

rank.data=eps.tr[,,s]
x <- acast(melt(setnames(data.table(melt(stock.vvs[,s,])),c('q.id','vvs','value'))[,state.vvs.f(value,diff.lag,sd.lag),by=vvs][,q.num:=1:.N,by=vvs],id.vars=c('vvs','q.num')),q.num~vvs~variable)[,,3]
rownames(x) <- dimnames(eps.tr)[[1]]
t=4
test.data <- x[t,]
y <- rescale(cor(t(rank.data[1:(t-1),]), use = "p"),from=c(-1,1))

cor.dt <- setkey(setnames(data.table(melt(y)),c('q.id','cor.q','cor')),q.id)

require(scales)
w <- n^((1:(t - 1))/(t - 1) - 1)

mu.f <- function(data,correlations){
        sum(data * correlations, na.rm = T)/sum(correlations, na.rm = T)
}

sdev.f <- function(mu,data,correlations){
        sqrt(sum(correlations * (data - mu)^2, na.rm = T)/sum(correlations, na.rm = T))}

lr.model.dt <- function(y,x,w){        
        w.dt <- setkey(data.table(q.id=dimnames(y)[[1]][1:length(w)],w),q.id)
        data.dt <- setkey(setnames(data.table(melt(x)),c('q.id','vvs','value')),q.id)
        cor.dt <- setkey(setnames(data.table(melt(y)),c('q.id','cor.q','cor')),q.id)
        cond.p <- setkey(na.omit(cor.dt[w.dt][data.dt,allow.cartesian=T][,mu:=mu.f(value,cor),by=.(vvs,cor.q)][,sdev:=sdev.f(mu,value,cor),by=.(vvs,cor.q)])[,.(cor.q,vvs,mu,sdev)],cor.q)
        priors <- setkey(unique(na.omit(cor.dt[w.dt][data.dt,allow.cartesian=T][,priors:=weighted.mean(cor,w,na.rm=T),by=.(cor.q)]),by='cor.q')[,.(cor.q,priors)],cor.q)
        
        setkey(unique(cond.p[priors,allow.cartesian=T],by=c('cor.q','vvs')),vvs)
}


model.dt <- lr.model.dt(y,x[1:(t-1),],w)

predict.f <- function(model,y,test){
        test.dt <- data.table(vvs=names(test),melt(test,value.name='test'),key='vvs')
        pred.model <- test.dt[model][,prob:=-log(dnorm(test,mean=mu,sd=sdev)),by=vvs][,sum(-log(priors),sum(prob)),by=cor.q]
        y[which.min(pred.model[,V1]),]
}

system.time(pred.r <- rankings.time.corrected.gw.cont(rank.data,x,n))

pred.dt <- predict.f(model.dt,rank.data[1:(t-1),],x[t,])

system.time(pred.r<- predict.ranking.script.f(methods,s,stock.vvs,baseline.rankings[,,,'true'],rank.parameters))


ggplot(data.table(melt(correlations)),aes(x=as.Date(as.yearqtr(Var1)),y=value,group=Var1))+geom_point()
weights <- n^((1:(t - 1))/(t - 1) - 1)

vvs <- (1:13)[-c(10,11,13)]
model <- lr.model(correlations, data[1:(t - 1),vvs ], (weights))

ggplot(data.table(id=1:length(model[[1]]),melt(model[[1]])),aes(x=id,y=value))+geom_line()
ggplot(data.table(id=1:length(model$cond$mean),melt(model$cond$mean)),aes(x=Var1,y=value))+geom_line()+facet_grid(~Var2)
ggplot(data.table(id=1:length(model$cond$sdev),melt(model$cond$sdev)),aes(x=Var1,y=value))+geom_line()+facet_grid(~Var2)


pred.r <- pred.cont(rank.data[1:(t - 1), ], model, as.numeric(data[t, vvs]))
cor(pred.dt,rank.data[t, ],use='p')

varib.pred <- pred.cont(rank.data[1:(t - 1), ], model, as.numeric(data[t, vvs ]))
ggplot(varib.pred,aes(x=1:(t-1),y=cond.prod))+geom_line()

pred.cont <- function(rank.data, model, test.data) {
        #prod <- (model$priors)
        cond.prod <- sapply(1:nrow(rank.data), function(r) {
                cond <- sapply(1:length(test.data), function(i) {
                        dnorm(test.data[i], mean = model$cond$mean[r, i], 
                              sd = model$cond$sdev[r, i])
                })
                
                sum(-log(cond), na.rm = T)
        })
        #data.table(melt(cond.prod))
        #prod <- -log(model$priors)-log(cond.prod)
        #data.table(prod)
        data.table(-log(model$priors),cond.prod)
        #rank.data[which.min(prod), ]
}
pred.cont <- function(rank.data, model, test.data) {
        prod <- -log(model$priors) + sapply(1:nrow(rank.data), function(r) {
                cond <- sapply(1:length(test.data), function(i) {
                        dnorm(test.data[i], mean = model$cond$mean[r, i], 
                              sd = model$cond$sdev[r, i])
                })
                # cond[is.infinite(cond)]<-NA cond<-round(cond,5)
                # cond[which(cond==0)]<-NA cond
                sum(-log(cond), na.rm = T)
        })
        rank.data[which.min(prod), ]
        #data.table(prod)
}




lr.model <- function(correlations, data, weights) {
        priors <- sapply(1:ncol(correlations), function(r) {
                weighted.mean(correlations[r, ], weights, na.rm = T)})
        attributes <- 1:ncol(data)
        mu <- weights * t(sapply(1:nrow(correlations), function(r) {
                sapply(attributes, function(x) {
                        sum(data[, x] * correlations[r, ], na.rm = T)/sum(correlations[r, ], na.rm = T)})
        }))
        sigma <- weights * t(sapply(1:nrow(correlations), function(r) {
                sapply(attributes, function(x) {
sqrt(sum(correlations[r, ] * (data[, x] - mu[r, x])^2, na.rm = T)/sum(correlations[r, ], na.rm = T))})
                
        }))
        conditionals <- list(mean = mu, sdev = sigma)
        list(priors = priors, cond = conditionals)
}






nbr.generic <- function(corr,data, test.data, weights,rank.data) {
        #correlations <- rescale(cor(t(rank.data), use = "p"),from=c(-1,1))
        model <- lr.model(corr, data, weights)
        
        pred.cont(rank.data, model, as.numeric(test.data))
        #model <- lr.model.dt(corr, data, weights)
        #predict.f(model,rank.data, test.data)
}

rankings.time.corrected.gw.cont <- function(rank.data, data,n) {
        corr <- rescale(cor(t(rank.data), use = "p"),from=c(-1,1))
        sapply(4:nrow(rank.data), function(i) {
                weights <- n^((1:(i - 2))/(i - 2) - 1)
                predict.rank <- nbr.generic(corr[2:(i - 1),2:(i - 1)], data[1:(i - 2), ], data[i - 1, ], weights,rank.data[2:(i - 1),])
                
                if (length(predict.rank) == 0) {
                        rep(NA, length(rank.data[i - 1, ]))
                } else {
                        predict.rank
                }
        })
}

rankings.time.corrected.gw.cont <- function(rank.data, data,n) {
        corr <- rescale(cor(t(rank.data), use = "p"),from=c(-1,1))
        sapply(3:nrow(rank.data), function(i) {
                weights <- n^((1:(i - 1))/(i - 1) - 1)
                predict.rank <- nbr.generic(corr[1:(i - 1),1:(i - 1)], data[1:(i - 1), ], test.data=data[i, ], weights,rank.data[1:(i - 1),])
                
                if (length(predict.rank) == 0) {
                        rep(NA, length(rank.data[i - 1, ]))
                } else {
                        predict.rank
                }
        })
}





