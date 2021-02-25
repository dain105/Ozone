ASOS.lo<-read.csv("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/asos.lo.csv")

air.lo<-read.csv("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/lon.csv",head=T)
colnames(air.lo)<-c("code","address","ad","lon","lat")


library(spTimer)
uni.asos<-ASOS.lo[!duplicated(ASOS.lo[,c(1,3,4)]),]
colnames(uni.asos)<-c("code","address","lat","lon","alt")
dim(uni.asos)
uni.air<-air.lo[!duplicated(air.lo[,c(1,4,5)]),]


library(ggmap)
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="toner-lite", source = "stamen")
ggmap(p)+ geom_point(data=uni.air, aes(x=lon, y=lat),size=2,pch=8, color="red",alpha=0.7)+geom_point(data=uni.asos, aes(x=lon, y=lat) ,size=2, color="blue",alpha=0.7) 


data<-read.csv("S:/Users/Dain/no.NA.data.csv")
uni.asos<-unique(data[,c(15,14)])
uni.air<-unique(data[,c(17,18)])

ggmap(p)+ geom_point(data=data, aes(x=air.lon, y=air.lat), size=3, pch=8, color="red", alpha=0.7)+ geom_point(data=data, aes(x=asos.lon, y=asos.lat), size=2, color="blue", alpha=0.7)
