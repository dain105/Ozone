data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
summary(data)
na.data<-data[is.na(data$max.temp),]

for(i in 369:552){
  selected.date<-unique(data$date)
  max.temp.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
max.temp.fit<-max.temp.fit[!is.na(max.temp.fit$max.temp),]
dim(max.temp.fit)


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
coords <-as.matrix(max.temp.fit[,16:17])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=max.temp~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(max.temp.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(window(m.1$p.beta.recover.samples))
#max.temp.val<-unique(cbind(data$asos.lon,data$asos.lat))
max.temp.val<-cbind(data$asos.lon,data$asos.lat,data$max.temp)
colnames(max.temp.val)<-c("lon","lat","max.temp")
max.temp.val<-max.temp.val[is.na(max.temp.val[,3]),1:2]
pred.coords <- t(as.matrix(max.temp.val))
pred.covars <- cbind(t,t(as.matrix(max.temp.val)))
# new.data<-cbind(data$air.lon,data$air.lat,data$max.temp)
# colnames(new.data)<-c("air.lon","air.lat","max.temp")
predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
max.temp<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
  max.temp[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}

replaced.max.temp<-cbind(data[is.na(data$max.temp),1:7],max.temp,data[is.na(data$max.temp),9:18])
new.data<-data[!is.na(data$max.temp),]
replaced.max.temp.data<-rbind(replaced.max.temp,new.data)
replaced.max.temp.data<-replaced.max.temp.data[order(replaced.max.temp.data[,3],replaced.max.temp.data[,2]), ]
replaced.max.temp.data<-replaced.max.temp.data[,2:18]
dim(replaced.max.temp.data)
sum(is.na(replaced.max.temp.data$max.temp))

setwd("S:/Users/Dain/연구/졸업논문/data/final data")
write.csv(replaced.max.temp.data, "replaced.max.temp.data.csv", append = FALSE)

