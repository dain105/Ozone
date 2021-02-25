data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
na.data1<-data[!is.na(data$pcp),]
zero<-rep(0,dim(na.data)[1])
na.data<-data[is.na(data$pcp),]
na.data$pcp<-zero
replaced.pcp<-rbind(na.data,na.data1)
dim(replaced.pcp)
replaced.pcp.data<-replaced.pcp[order(replaced.pcp[,3],replaced.pcp[,2]), ]
replaced.pcp.data<-replaced.pcp.data[,2:18]
sum(is.na(replaced.pcp.data$pcp))


# 
# # Bayesian spaital model을 이용한 보간법
# library(spBayes)
# library(spTimer)
# library(mgcv) #for fittin additive model  
# library(maps) #for drawing map
# library(akima) #for interpolation 
# library(plyr)
# library(stringr)
# library(XML)
# n.samples=1000
# burn.in <- 0.3*n.samples
# coords <-as.matrix(pcp.fit[,16:17])#asos
# dim(coords)
# sum(is.na(pcp.fit$pcp))
# 
# 
# 
# priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
#              "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
# starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
# tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)
# 
# bat.fm <-spLM(formula=pcp~air.lon+air.lat,
#               coords=coords, 
#               priors=priors, 
#               tuning=tuning,
#               starting=starting, 
#               cov.model="exponential", 
#               n.samples=n.samples, 
#               verbose=TRUE,
#               data=data.frame(pcp.fit))
# 
# 
# 
# m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
# #summary(window(m.1$p.beta.recover.samples))
# #pcp.val<-unique(cbind(data$asos.lon,data$asos.lat))
# pcp.val<-cbind(data$asos.lon,data$asos.lat,data$pcp)
# colnames(pcp.val)<-c("lon","lat","pcp")
# pcp.val<-pcp.val[is.na(pcp.val[,3]),1:2]
# pred.coords <- pcp.val[,1:2]
# pred.covars <- cbind(rep(1,dim(pcp.val)[1]),pcp.val)
# # new.data<-cbind(data$air.lon,data$air.lat,data$pcp)
# # colnames(new.data)<-c("air.lon","air.lat","pcp")
# 
# 
# predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
# p.median.pcp<-rep(0,dim(predicted$p.y.predictive.samples)[1])
# for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
#   p.median.pcp[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}
# str(p.median.pcp)
# replaced.pcp<-cbind(data[is.na(data$pcp),1:17],p.median.pcp)
