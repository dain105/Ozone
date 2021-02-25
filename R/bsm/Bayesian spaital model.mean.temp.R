data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
na.data<-data[is.na(data$mean.temp),]
summary(data)

for(i in 369:552){
  selected.date<-unique(data$date)
  mean.temp.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
mean.temp.fit<-mean.temp.fit[!is.na(mean.temp.fit$mean.temp),]
dim(mean.temp.fit)


# Bayesian spaital model을 이용한 보간법
library(spBayes)
library(spTimer)
library(mgcv) #for fittin additive model  
library(maps) #for drawing map
library(akima) #for interpolation 
library(plyr)
library(stringr)
library(XML)
n.samples=1000
burn.in <- 0.3*n.samples
coords <-as.matrix(mean.temp.fit[,16:17])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=mean.temp~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(mean.temp.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(mean.tempow(m.1$p.beta.recover.samples))
#mean.temp.val<-unique(cbind(data$asos.lon,data$asos.lat))
mean.temp.val<-cbind(data$asos.lon,data$asos.lat,data$mean.temp)
colnames(mean.temp.val)<-c("lon","lat","mean.temp")
mean.temp.val<-mean.temp.val[is.na(mean.temp.val[,3]),1:2]
pred.coords <- mean.temp.val[,1:2]
pred.covars <- cbind(rep(1,dim(mean.temp.val)[1]),mean.temp.val)
# new.data<-cbind(data$air.lon,data$air.lat,data$mean.temp)
# colnames(new.data)<-c("air.lon","air.lat","mean.temp")


predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
mean.temp<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
mean.temp[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}

replaced.mean.temp<-cbind(data[is.na(data$mean.temp),1:5],mean.temp,data[is.na(data$mean.temp),7:18])
new.data<-data[!is.na(data$mean.temp),]
replaced.mean.temp.data<-rbind(replaced.mean.temp,new.data)
replaced.mean.temp.data<-replaced.mean.temp.data[order(replaced.mean.temp.data[,3],replaced.mean.temp.data[,2]), ]
replaced.mean.temp.data<-replaced.mean.temp.data[,2:18]
dim(replaced.mean.temp.data)
sum(is.na(replaced.mean.temp.data$mean.temp))
