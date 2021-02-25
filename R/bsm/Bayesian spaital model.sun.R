data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
summary(data)
na.data<-data[is.na(data$sun),]

for(i in 369:552){
  selected.date<-unique(data$date)
  sun.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
sun.fit<-sun.fit[!is.na(sun.fit$sun),]
dim(sun.fit)


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
coords <-as.matrix(sun.fit[,16:17])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=sun~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(sun.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(window(m.1$p.beta.recover.samples))
#sun.val<-unique(cbind(data$asos.lon,data$asos.lat))
sun.val<-cbind(data$asos.lon,data$asos.lat,data$sun)
colnames(sun.val)<-c("lon","lat","sun")
sun.val<-sun.val[is.na(sun.val[,3]),1:2]
pred.coords <- sun.val[,1:2]
pred.covars <- cbind(rep(1,dim(sun.val)[1]),sun.val)
# new.data<-cbind(data$air.lon,data$air.lat,data$sun)
# colnames(new.data)<-c("air.lon","air.lat","sun")


predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
sun<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
  sun[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}

replaced.sun<-cbind(data[is.na(data$sun),1:11],sun,data[is.na(data$sun),13:18])
new.data<-data[!is.na(data$sun),]
replaced.sun.data<-rbind(replaced.sun,new.data)
replaced.sun.data<-replaced.sun.data[order(replaced.sun.data[,3],replaced.sun.data[,2]), ]
replaced.sun.data<-replaced.sun.data[,2:18]
dim(replaced.sun.data)
sum(is.na(replaced.sun.data$sun))

setwd("S:/Users/Dain/연구/졸업논문/data/final data")
write.csv(replaced.sun.data, "replaced.sun.data.csv", append = FALSE)

