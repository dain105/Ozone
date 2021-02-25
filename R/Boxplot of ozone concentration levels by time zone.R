setwd("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/final data")
ozone.8h.2014<-read.csv("ozone.8h.2014.csv")
ozone.8h.2015<-read.csv("ozone.8h.2015.csv")
ozone.8h.2016<-read.csv("ozone.8h.2016.csv")
ozone.8h<-rbind(ozone.8h.2014,ozone.8h.2015,ozone.8h.2016)
colnames(ozone.8h)<-c("x","0h~8h","1h~9h","2h~10h","3h~11h","4h~12h","5h~13h","6h~14h","7h~15h","8h~16h","9h~17h","10h~18h","11h~19h","12h~20h","13h~21h","14h~22h","15h~23h","16h~24h")

boxplot(ozone.8h[,2:18],boxwex = 0.5,pch = "-",xlab="hour",ylab="ozone")


o3.3year<-matrix(NA, 114975,ncol=3)
o3.3year[,1]<-ozone.8h.2014[,12]
o3.3year[,2]<-ozone.8h.2015[1:114975,12]
o3.3year[,3]<-ozone.8h.2016[1:114975,12]
colnames(o3.3year)<-c("2014","2015","2016")
boxplot(o3.3year,boxwex = 0.5,pch = "-",xlab="year",ylab="ozone")
