data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")
summary(data)
na.data<-data[is.na(data$humid),]

for(i in 369:552){
  selected.date<-unique(data$date)
  humid.fit<-data[data$date==selected.date[i],]} # aws+asos+농진청
humid.fit<-humid.fit[!is.na(humid.fit$humid),]
dim(humid.fit)


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
coords <-as.matrix(humid.fit[,16:17])#asos
dim(coords)

priors<-list("beta.Flat","phi.Unif"=c(2,3.5),
             "sigma.sq.IG"=c(2,0.5),"tau.sq.IG"=c(2,0.5))
starting <- list("phi"=3, "sigma.sq"=150, "tau.sq"=10)
tuning <-list("phi"=0.2, "sigma.sq"=0.2, "tau.sq"=0.2)

bat.fm <-spLM(formula=humid~air.lon+air.lat,
              coords=coords, 
              priors=priors, 
              tuning=tuning,
              starting=starting, 
              cov.model="exponential", 
              n.samples=n.samples, 
              verbose=TRUE,
              data=data.frame(humid.fit))



m.1 <- spRecover(bat.fm, start=burn.in, verbose=FALSE)
#summary(window(m.1$p.beta.recover.samples))
#humid.val<-unique(cbind(data$asos.lon,data$asos.lat))
humid.val<-cbind(data$asos.lon,data$asos.lat,data$humid)
colnames(humid.val)<-c("lon","lat","humid")
humid.val<-humid.val[is.na(humid.val[,3]),1:2]
pred.coords <- humid.val[,1:2]
pred.covars <- cbind(rep(1,dim(humid.val)[1]),humid.val)
# new.data<-cbind(data$air.lon,data$air.lat,data$humid)
# colnames(new.data)<-c("air.lon","air.lat","humid")


predicted <-spPredict(m.1, pred.covars=pred.covars, pred.coords=pred.coords)
humid<-rep(0,dim(predicted$p.y.predictive.samples)[1])
for (j in 1:dim(predicted$p.y.predictive.samples)[1]){
  humid[j]<-quantile(predicted$p.y.predictive.samples[j,],c(0.5))}

replaced.humid<-cbind(data[is.na(data$humid),1:10],humid,data[is.na(data$humid),12:18])
new.data<-data[!is.na(data$humid),]
replaced.humid.data<-rbind(replaced.humid,new.data)
replaced.humid.data<-replaced.humid.data[order(replaced.humid.data[,3],replaced.humid.data[,2]), ]
replaced.humid.data<-replaced.humid.data[,2:18]
dim(replaced.humid.data)
sum(is.na(replaced.wind.data$wind))

setwd("S:/Users/Dain/연구/졸업논문/data/final data")
write.csv(replaced.humid.data, "replaced.humid.data.csv", append = FALSE)

