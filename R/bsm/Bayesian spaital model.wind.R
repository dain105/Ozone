data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
summary(data)
na.data<-data[is.na(data$wind),]

for(i in 369:552){
  selected.date<-unique(data$date)
  wind.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
wind.fit<-wind.fit[!is.na(wind.fit$wind),]
dim(wind.fit)


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
coords <-as.matrix(wind.fit[,16:17])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=wind~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(wind.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(window(m.1$p.beta.recover.samples))
#wind.val<-unique(cbind(data$asos.lon,data$asos.lat))
wind.val<-cbind(data$asos.lon,data$asos.lat,data$wind)
colnames(wind.val)<-c("lon","lat","wind")
wind.val<-wind.val[is.na(wind.val[,3]),1:2]
pred.coords <- t(as.matrix(wind.val))
pred.covars <- cbind(t,t(as.matrix(wind.val)))
# new.data<-cbind(data$air.lon,data$air.lat,data$wind)
# colnames(new.data)<-c("air.lon","air.lat","wind")
predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
wind<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
  wind[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}

replaced.wind<-cbind(data[is.na(data$wind),1:9],wind,data[is.na(data$wind),11:18])

new.data<-data[!is.na(data$wind),]
replaced.wind.data<-rbind(replaced.wind,new.data)
replaced.wind.data<-replaced.wind.data[order(replaced.wind.data[,3],replaced.wind.data[,2]), ]
replaced.wind.data<-replaced.wind.data[,2:18]
dim(replaced.wind.data)
sum(is.na(replaced.wind.data$wind))

# setwd("S:/Users/Dain/연구/졸업논문/data/final data")
# write.csv(replaced.wind.data, "replaced.wind.data.csv", append = FALSE)

