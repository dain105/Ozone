#a=2014년 ,b=2015년, c=2016년 
setwd("S:/Users/Dain/연구/졸업논문/data/2014")
#315관측소*90일*24시간
a1<-read.csv("201401.csv")
#315관측소*91일*24시간
a2<-read.csv("201402.csv")
a2<-subset(a2, a2[,3]!="운정")
#315관측소*92일*24시간
a3<-read.csv("201403.csv")
a3<-subset(a3, a3[,3]!="운정")
#315관측소*92일*24시간
a4<-read.csv("201404.csv")
a4<-subset(a4, a4[,3]!="운정")
setwd("S:/Users/Dain/연구/졸업논문/data/2015")
#316관측소*90일*24시간
b1<-read.csv("201501.csv",sep=",")
b1<-subset(b1, b1[,3]!="운정")
dim(b1)
length(unique(b1[,3]))
#317관측소*91일*24시간
b2<-read.csv("201502.csv",sep=",")
#317관측소*92일*24시간
b3<-read.csv("201503.csv",sep=",")
#316관측소*92일*24시간
b4<-read.csv("201504.csv",sep=",")
b4<-subset(b4, b4[,3]!=("운정"))
b4<-subset(b4, b4[,3]!=("요촌동"))

setwd("S:/Users/Dain/연구/졸업논문/data/2016")
#320관측소*91일*24시간
c1<-read.csv("201601.csv")
#320관측소*91일*24시간
c2<-read.csv("201602.csv")
c2<-subset(c2, c2[,3]!="진천읍")
#321관측소*92일*24시간
c3<-read.csv("201603.csv")
#322관측소*92일*24시간
c4<-read.csv("201604.csv")
c4<-subset(c4, c4[,3]!="이곡동")
##########merge by year###########
a<-rbind(a1[,c(3,2,4,5,8,12)],a2[,c(3,2,4,5,8,12)],a3[,c(3,2,4,5,8,12)],a4[,c(3,2,4,5,8,12)])
b<-rbind(b1[,c(3,2,4,5,8,12)],b2[,c(3,2,4,5,8,12)],b3[,c(3,2,4,5,8,12)],b4[,c(3,2,4,5,8,12)])
b1<-subset(b,substr(b[,3],5,8)!="0229")
b1<-names(table(b[,2])==8760)[table(b[,2])==8760]
b<- b[b[,2] %in% b1,]
dim(b)
c<-rbind(c1[,c(3,2,4,5,8,12)],c2[,c(3,2,4,5,8,12)],c3[,c(3,2,4,5,8,12)],c4[,c(3,2,4,5,8,12)])
c<-subset(c,substr(c[,3],5,8)!="0229")
c1<-names(table(c[,2])==8760)[table(c[,2])==8760]
c<- c[c[,2] %in% c1,]
dim(c)


#2014
aa<-matrix(NA,365,ncol=24)
a1<-names(table(a[,2])==8760)[table(a[,2])==8760]
a2<-a[a[,2] %in% a1,]
a2<-a2[order(a2[,2],a2[,3]),]
sum(is.na(a2$O3))

for(j in 1:315){
  for(i in 1:365){
    a3<-as.matrix(a2[((8760*j)-8759):(8760*j),c(2,5)])
    aa<-matrix(a3[,2],ncol=24,byrow=T)
    as.matrix(assign(paste("aa",j, sep=""),aa))
  }}
aaa<-do.call(cbind, lapply( paste0("aa", 1:315) , get))

dim(a2)
sum(is.na(aaa1))

dailyAverage.2014<-matrix(NA,365,ncol=1)
for(i in 1:365){
  dailyAverage.2014[i,]<-mean(aaa[i,],na.rm=T)
}
dailyAverage.2014
str(aaa)



#2015
b1<-names(table(b[,2])==8760)[table(b[,2])==8760]
b2<-b[b[,2] %in% b1,]
b2<-b2[order(b2[,2],b2[,3]),]
sum(is.na(b2$O3))

for(j in 1:316){
  for(i in 1:365){
    b3<-b2[((8760*j)-8759):(8760*j),5]
    bb<-matrix(b3,ncol=24,byrow=T)
    as.matrix(assign(paste("bb",j, sep=""),bb))
  }}
bbb<-do.call(cbind, lapply( paste0("bb", 1:316) , get))

dailyAverage.2015<-matrix(NA,365,ncol=1)
for(i in 1:365){
  dailyAverage.2015[i,]<-mean(bbb[i,],na.rm=T)
}
dailyAverage.2015



#2016
c1<-names(table(c[,2])==8760)[table(c[,2])==8760]
c2<-c[c[,2] %in% c1,]
c2<-c2[order(c2[,2],c2[,3]),]
sum(is.na(c2$O3))

for(j in 1:320){
  for(i in 1:365){
    c3<-c2[((8760*j)-8759):(8760*j),5]
    cc<-matrix(c3,ncol=24,byrow=T)
    as.matrix(assign(paste("cc",j, sep=""),cc))
  }}
ccc<-do.call(cbind, lapply( paste0("cc", 1:320) , get))

dailyAverage.2016<-matrix(NA,365,ncol=1)
for(i in 1:365){
  dailyAverage.2016[i,]<-mean(ccc[i,],na.rm=T)
}
dailyAverage.2016
dailyAverage<-rbind(dailyAverage.2014,dailyAverage.2015,dailyAverage.2016)

hist(dailyAverage, freq = TRUE, main = "Daily ozone concentration",cex.main=2,cex.lab=1.5)
hist(sqrt(dailyAverage),  prob= T, main = "SQRT daily ozone concentration",cex.main=2,cex.lab=1.5)
lines(density(sqrt(dailyAverage)),col=4,lwd=2)
hist(log(dailyAverage), freq = TRUE, main = "Log daily ozone concentration",cex.main=2,cex.lab=1.5)



qqnorm(sqrt(dailyAverage),cex.main=2,cex.lab=1.5)
qqline(sqrt(dailyAverage),col=2,lwd=2)
#연 평균 ozone 농ㄷ
year.average<-matrix(NA,365,ncol=3)
year.average[,1]<-(dailyAverage[1:365,])
year.average[,2]<-(dailyAverage[366:730,])
year.average[,3]<-(dailyAverage[731:1095,])
colnames(year.average)<-c("2014","2015","2016")

year.average
merge(year.average,air.lo,by=)