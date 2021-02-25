
data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")

for(i in 369:552){
  selected.date<-unique(data$date)
  o3.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
o3.fit<-o3.fit[!is.na(o3.fit$o3),]
dim(o3.fit)


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
coords <-as.matrix(o3.fit[,14:13])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=o3~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(o3.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(window(m.1$p.beta.recover.samples))
o3.val<-unique(cbind(data$asos.lon,data$asos.lat))
colnames(o3.val)<-c("lon","lat")
pred.coords <- unique(cbind(data$asos.lon,data$asos.lat))
dim(pred.coords)
pred.covars <- cbind(rep(1,dim(o3.val)[1]),o3.val)
predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
str(predicted)
p.median<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
  p.median[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}
p.median
