library(maps)
library(mapdata)

data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/no.NA.data.csv")
# data$y<-substr(data$date,1,4)
# colnames(meanBy)<-c("y","air.code","o3")
# meanBy<-meanBy[order(meanBy[,1]),]
gp.mean<-read.csv("S:/Users/Dain/연구/졸업논문/결과/gp.median.csv")
gp.mean<-gp.mean[,2:34]

new.data<-matrix(NA,184*33*3,ncol=3)
year<-(as.numeric(substr(data$date,1,4)))
new.data[,1]<-as.numeric(year)
new.data[,2]<-data$air.station
for(i in 1:33){
new.data[((552*i)-551):(552*i),3]<-gp.mean[,i]
}
new.data<-as.data.frame(new.data)
str(new.data)
colnames(new.data)<-c("year","air.station","o3")

air.lo<-read.csv("S:/Users/Dain/연구/졸업논문/data/lon.csv",head=T)
colnames(air.lo)<-c("air.station","address","ad","lon","lat")
str(air.lo)
meanBy1<-merge(new.data,air.lo,by="air.station")
meanBy2<-meanBy1[,c(-5)]
meanBy3<-meanBy2[order(meanBy2$year),]
meanBy4<-subset(meanBy3,meanBy3$y==2016)
w1<-c(0,2016,0.051,"a",125.6372, 37.4819)
# w2<-c(0,2016,0.044,"a",125.6372, 37.4819)
# w3<-c(0,2016,0.044,"a",131.84839, 37.5063668)
w4<-c(0,2016,0.06,"a",131.84839, 37.5063668)
meanBy4<-rbind(meanBy4,w1,w4)

tail(meanBy4,4)


str(meanBy4)
length(unique(data$air.station))
meanBy<-aggregate(x=meanBy4$o3,by=list(meanBy4$air.station),FUN="mean",na.rm=T)
head(meanBy4)
str(meanBy4)
meanBy4$lon<-as.numeric(meanBy4$lon)
meanBy4$lat<-as.numeric(meanBy4$lat)
meanBy4$o3<-as.numeric(meanBy4$o3)
tail(meanBy4)

library(sp)
library(gstat)
gg<-meanBy4[,c(5,6)]
head(gg)
gg2<-gg
coordinates(gg2)<-c("lon","lat")
grd<-as.data.frame(spsample(gg2, "regular",n=50000))
names(grd)<-c("lon","lat")
coordinates(grd)<-c("lon","lat")
gridded(grd)=TRUE
fullgrid(grd)<-TRUE

library(raster)

mse.idw <-gstat::idw(meanBy4$o3~1, gg2, newdata=grd, idp=2)
r<- raster(mse.idw)
r

plot(r,xlim=c(125,130),ylim=c(34,40),main="Model based interpolation of\n annual average of daily ozone level in 2016",cex.main=0.8)

map(database='worldHires',region='South Korea', add=T)

