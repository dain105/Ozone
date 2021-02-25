#a=2014년 ,b=2015년, c=2016년 
setwd("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/2014")
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
setwd("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/2015")
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
           
setwd("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/2016")
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
#2014년 일 중 오존 농도가 가장 높은 시간 찾기(8시간의 평균)
# professor#d.data3
# load(file="S:/Users/Dain/연구/졸업논문/data/airdata.rda")
me1<-matrix(NA,114975,ncol=17)#365일*24-8시간
for(j in 1:114975){
 for(i in 1:17){
   aa<-a[((24*j)-23):(24*j),]
   a1<-aa[i:(7+i),]
   a1<-na.omit(a1)
   me1[j,i] <-mean(a1[,5])
 }}
#addmargins(table(max.col(me1)))

#2015년 일 중 오존 농도가 가장 높은 시간 찾기(8시간의 평균)
me2<-matrix(NA,115340,ncol=17)#365일*24-8시간
for(j in 1:115340){
 for(i in 1:17){
   bb<-b[((24*j)-23):(24*j),]
   b1<-bb[i:(7+i),]
   b1<-na.omit(b1)
   me2[j,i] <-mean(b1[,5])
 }}
# addmargins(table(max.col(me2)))
 
#2016년 일 중 오존 농도가 가장 높은 시간 찾기(8시간의 평균)
me3<-matrix(NA,116800,ncol=17)#365일*24-8시간
for(j in 1:116800){
 for(i in 1:17){
   cc<-c[((24*j)-23):(24*j),]
   c1<-cc[i:(7+i),]
   c1<-na.omit(c1)
   me3[j,i] <-mean(c1[,5])
 }}

# setwd("S:/Users/Dain/연구/졸업논문/data/final data")
# write.csv(me1, "ozone.8h.2015.csv")
# write.csv(me2, "ozone.8h.2016.csv")
# write.csv(me3, "ozone.8h.2017.csv")



# mean.2015<-matrix(NA,17,ncol=1)
# for(i in 1:17){
#   mean.2015[i,]<-mean(me1[,i],na.rm =T)
#   }
# mean.2016<-matrix(NA,17,ncol=1)
# for(i in 1:17){
#   mean.2016[i,]<-mean(me2[,i],na.rm =T)
# }
# mean.2017<-matrix(NA,17,ncol=1)
# for(i in 1:17){
#   mean.2017[i,]<-mean(me3[,i],na.rm =T)
# }
# 
# mean<-t(cbind(mean.2015,mean.2016,mean.2017))
# setwd("S:/Users/Dain/연구/졸업논문/data/final data")
# write.csv(mean, "mean.csv")

# addmargins(table(max.col(me3)))

#extract max ozon data row(12:00~19:00)
#2014 data set
a[,3]<-as.matrix(substr(a[,3],1,8))#날짜 수정
a_1<-unique(a[,c(1,2,3,6)])
a_2<-subset(a,a[,4]>11&a[,4]<20)
air.2014<-cbind(a_1,me1[,12])
colnames(air.2014)<-c("ad","code","date","address","o3")
#2015 data set
b[,3]<-as.matrix(substr(b[,3],1,8))#날짜 수정
b_1<-unique(b[,c(1,2,3,6)])
b_2<-subset(b,b[,4]>11&b[,4]<20)
air.2015<-cbind(b_1,me2[,12])
dim(me2)
colnames(air.2015)<-c("ad","code","date","address","o3")
#2016 data set
c[,3]<-as.matrix(substr(c[,3],1,8))#날짜 수정
c_1<-unique(c[,c(1,2,3,6)])
c_2<-subset(c,c[,4]>11&c[,4]<20)
air.2016<-cbind(c_1,me3[,12])
colnames(air.2016)<-c("ad","code","date","address","o3")
air.data<-rbind(air.2014,air.2015,air.2016)

air.lo<-read.csv("S:/Users/Dain/연구/졸업논문/data/lon.csv",head=T)
colnames(air.lo)<-c("code","address","ad","lon","lat")
merge.2014<-unique(merge(air.2014,air.lo[,c(1,4:5)],by="code"))
merge.2015<-unique(merge(air.2015,air.lo[,c(1,4:5)],by="code"))
merge.2016<-unique(merge(air.2016,air.lo[,c(1,4:5)],by="code"))
air.data<-rbind(merge.2014,merge.2015,merge.2016)
head(air.data)
#merge air data with lon+lat
air.data$date<-as.Date(air.data$date,format="%Y%m%d")
#air.data without island   NA=:5304
x1<-as.matrix(grep("울릉",air.data[,4]))
x2<-as.matrix(grep("제주",air.data[,4]))
x3<-as.matrix(grep("백령",air.data[,4]))
x<-rbind(x1,x2,x3)
air.data<-air.data[-x,]

# #airmonitoring site map
# library(ggmap)
# p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="toner-lite", source = "stamen")
# ggmap(p)+ geom_point(data=air.data1, aes(x=lon, y=lat),size=1, color="red", alpha=0.7)


#ASOS data and Location
ASOS.lo<-read.csv("S:/Users/Dain/연구/졸업논문/data/asos.lo.csv")
ASOS<-read.csv("S:/Users/Dain/연구/졸업논문/data/ASOS/ASOS.csv")
#extract unique row of ASOS locations
ASOS.lo<-ASOS.lo[!duplicated(ASOS.lo[,1]),]
ASOS.lo<-ASOS.lo[,c(1,4,6,7,8)]

#merge ASOS with ASOS locatioms by location code
ASOS<-(merge(ASOS,ASOS.lo,by="지점",all.x=T))
colnames(ASOS)<-c("code","date","mean.temp","min.temp","max.temp","pcp","wind","humid","sun","address","lat","lon","alt")

library(spTimer)
uni.asos<-ASOS[!duplicated(ASOS[,1]),]
uni.asos<-na.exclude(uni.asos[,c(1,11,12)])
uni.air<-air.data[!duplicated(air.data[,1]),]
uni.air<- na.exclude(uni.air[,c(1,6,7)])

#plot(air data, ASOS)
library(ggmap)
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="toner-lite", source = "stamen")
ggmap(p)+ geom_point(data=uni.air, aes(x=lon, y=lat),size=1, color="black",pch=18, alpha=0.7)+geom_point(data=uni.asos, aes(x=lon, y=lat), size=1, color="black", alpha=0.7) 

#calculate minimum distance of ASOS and air monitoring site 
vec.dist <-matrix(NA,90,ncol=3)
for (i in 1:90){
  dist<-spT.geo.dist(as.numeric(uni.asos[i,c("lon","lat")]),as.data.frame(uni.air[,c("lon","lat")]))
  new<-cbind(uni.air$code,dist)
  head(new)
  new1<-subset(new,new[,2]==min(new[,2]))
  vec.dist[i,1]<-uni.asos[i,1]
  vec.dist[i,2]<-new1[1,1]
  vec.dist[i,3]<-new1[1,2]
}
colnames(vec.dist)<-c("asos.code","air.station","dist")
vec.dist<-subset(vec.dist,vec.dist[,3]<5)
vec.dist1<-merge(vec.dist,ASOS.lo,by.x="asos.code",by.y="지점")
vec.dist2<-merge(vec.dist1,air.lo,by.x="air.station",by.y="code")
vec.dist2<-vec.dist2[,c(-8,-9)]
dim(vec.dist2)
colnames(vec.dist2)<-c("air.station","asos.station","dist","site","asos.lat","asos.lon","asos.alt","air.lon","air.lat")

#map
library(ggmap)
p<-get_map(location = c(127.5, 35.8), zoom =7, maptype="toner-lite", source = "stamen")
ggmap(p,legend="top")+geom_point(data=vec.dist2, aes(x=air.lon, y=air.lat),size=2, color="red",pch=8, alpha=0.7)+geom_point(data=vec.dist2, aes(x=asos.lon, y=asos.lat), size=2, color="blue", alpha=0.5) 

#merge ASOS data and air monitoring data by min distance
data1<-merge(ASOS,vec.dist2,by.x="code",by.y="asos.station")
data1[,2]<-as.Date(data1[,2])
data1<-data1[order(data1[,1],data1[,2]), ]
data2<-subset(data1,data1$dist<5)
data3<-merge(data2,air.data[,c(3,1,5)],by.x=c("date","air.station"),by.y=c("date","code"))
head(data3)
length(unique(vec.dist2$asos.station))#36



#Integrated database
data<-data3[,c(1,2,3,11,4,5,6,7,8,9,10,17,18,19,20,21,22)]
dim(data)
# dataa<-names(table(data[,2])==1095)[table(data[,2])==1095]
# data<- data[data[,2] %in% dataa,]
data.d<-substr(data$date,6,10)
data.d1<-subset(data.d,data.d>="04-15"&data.d<="10-15")
data<- as.data.frame(data[substr(data$date,6,10) %in% data.d1,])
data1<-data[order(data[,2],data[,1]), ]


# setwd("S:/Users/Dain/연구/졸업논문/data/final data")
# write.csv(data1, "data.csv")


#f.data
# load(file="S:/Users/Dain/연구/졸업논문/data/fdata.rda")
# f.data

#  
#  library(sp)
#  library(gstat)
#  air.data<-as.data.frame(air.data)
#  coordinates(data) = ~asos.lon+asos.lat
#  coordinates(air.data) = ~lon+lat
# # pred.alt <- idw(asos.alt~1,data, newdata=air.data, idp=2)
#  alt<-cbind(pred.alt@coords, pred.alt@data$var1.pred)
#  u.alt<-unique(alt)
#  colnames(u.alt)<-c("lon","lat","alt")
######################################################################################

library(spTimer)
spatial.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.002)
# spatial.decay<-spT.decay(distribution=Unif(0.01,0.02),npoints=5)
# spatial.decay<-spT.decay(distribution="FIXED", value=0.006)

priors<-spT.priors(model="AR",inv.var.prior=Gamm(2,1),
                   beta.prior=Norm(0,10^4))
# priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
#                    beta.prior=Norm(0,10^4))

dim(data)
table(data$air.station)
#184일 3년 33개 관측소

data_gp <- spT.Gibbs(formula=o3 ~ asos.lon + asos.lat,
                    time.data=spT.time(t.series=184, segment=3),
                    data=data1,
                    model="AR",
                    coords=~asos.lon+asos.lat,
                    priors=priors,
                    nItr=13000,
                    nBurn=3000,
                    cov.fnc="exponential",
                    report=100,
                    #distance.method="geodetic:km",
                    spatial.decay=spatial.decay,
                    scale.transform = "SQRT",
                    #spatial.decay=spT.decay(distribution="FIXED", value=para.spatial[i]),
                    tol.dist=0.01)
  


