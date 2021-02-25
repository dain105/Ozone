data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/no.NA.data.csv")

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

air.station<-unique(data$air.station)
data1<- as.data.frame(a[a[,2] %in% air.station,])
data1<-data1[order(data1[,2],data1[,3]), ]

o3.4th.2014<-matrix(NA,33,ncol=1)
  for( j in 1:33){
  aa<-a[((8760*j)-8759):(8760*j),]
  aa<-subset(aa,aa[,5]!='NA')
  a1<-aa[order(aa[,5]), ]
  a2<-tail(a1,4)
  o3.4th.2014[j,]<-a2[1,5]
  }


data1<- as.data.frame(b[b[,2] %in% air.station,])
data1<-data1[order(data1[,2],data1[,3]), ]

o3.4th.2015<-matrix(NA,33,ncol=1)
for( j in 1:33){
  aa<-b[((8760*j)-8759):(8760*j),]
  aa<-subset(aa,aa[,5]!='NA')
  a1<-aa[order(aa[,5]), ]
  a2<-tail(a1,4)
  o3.4th.2015[j,]<-a2[1,5]
}


data1<- as.data.frame(c[c[,2] %in% air.station,])
data1<-data1[order(data1[,2],data1[,3]), ]

o3.4th.2016<-matrix(NA,33,ncol=1)
for( j in 1:33){
  aa<-c[((8760*j)-8759):(8760*j),]
  aa<-subset(aa,aa[,5]!='NA')
  a1<-aa[order(aa[,5]), ]
  a2<-tail(a1,4)
  o3.4th.2016[j,]<-a2[1,5]
}

#ppm단위를 ppb로 변경
o3.4th.2014<-1000*o3.4th.2014
o3.4th.2015<-1000*o3.4th.2015
o3.4th.2016<-1000*o3.4th.2016



plot(o3.4th.2014, type="o",cex=0.5, pch=15,col=1,ylab="",xlab="",xaxt="n",ylim=c(40,130))
par(new=T)
plot(o3.4th.2015, type="o",cex=0.5, pch=20,col=2,ylab="",xlab="",xaxt="n",ylim=c(40,130))
par(new=T)
plot(o3.4th.2016, type="o",cex=0.5, pch=18,col=4,xaxt="n",ylim=c(40,130), xlab="Air station number",
     ylab='ozone(ppb)',cex.lab=1, main="Annual 4th hightes ozone concentration",cex.main=2,cex.lab=1.5)
axis(1,at=1:33, lab=(1:33),cex.axis=0.7,cex.axis=1)
par(new=T)
legend(x=4, y=130,cex=0.9, pt.cex =1, box.lty ="blank",
       c("2014", "2015", "2016"),
       col=c(1,2,4), 
       pch=c(15,20,18))
