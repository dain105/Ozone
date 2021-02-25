data<-read.csv("S:/Users/Dain/연구/졸업논문/data/final data/data.csv")


data$mean.temp<-replaced.mean.temp.data$mean.temp
data$max.temp<-replaced.max.temp.data$max.temp
data$pcp<-replaced.pcp.data$pcp
data$wind<-replaced.wind.data$wind
data$humid<-replaced.humid.data$humid
data$sun<-replaced.sun.data$sun
data$o3<-replaced.o3.data$o3

sum(is.na(data))

setwd("S:/Users/Dain/연구/졸업논문/data/final data")
write.csv(data, "no.NA.data.csv")
