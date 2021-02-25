library(spTimer)
data<-read.csv("E:/Dain/연구/졸업논문/data/final data/no.NA.data.csv")
data$o3<-data$o3*1000
asos.station<-unique(data$code)
head(data)
#########AR#############
AR.mtx<-matrix(NA,50,ncol=7)
seed<-matrix(NA,50,ncol=3)
set.seed(1234)
for(i in 1:50){
  seed[i,]<-sample(1:30,3,replace = FALSE)
}
set.seed(1234)
for(i in 1:50){
  set.seed(seed[i,])
  validation<-sample(asos.station, 6, replace = FALSE)
  train<-as.data.frame(data[!(data$code) %in% validation,])
  test<-as.data.frame(data[data$code %in% validation,])

  priors<-spT.priors(model="AR",inv.var.prior=Gamm(2,1),
                     beta.prior=Norm(0,10^4))
 
  pred.coords<-unique(cbind(test$air.lon,test$air.lat))
  #184일 3년 33개 관측소
  set.seed(1234)
train_AR <- spT.Gibbs(formula=o3 ~max.temp+wind+humid+sun,
                       time.data=spT.time(t.series=184, segment=3),
                       data=train,
                       model="AR",
                       coords=~asos.lon+asos.lat,
                       priors=priors,
                       nItr=6000,
                       nBurn=1000,
                       cov.fnc="exponential",
                       report=5000,
                       #distance.method="geodetic:km",
                       #spatial.decay=spatial.decay,
                       scale.transform = "SQRT",
                       spatial.decay=spT.decay(distribution="FIXED", value=0.088),
                       tol.dist=0.01)
  # 
  # plot(data_AR)
  # summary(data_AR)
 pred.AR <- predict(train_AR, newdata=test, newcoords=pred.coords, type="spatial", tol.dist=0.01)
  AR.mtx[i,]<-spT.validation(as.numeric(paste(test$o3)),c(pred.AR$Median))
  
  
setwd("E:/Dain/연구/졸업논문/결과/결과2/validation/AR")
write.table(AR.mtx,"AR.mtx.txt",sep=",")
# ar.mean<-pred.AR$Mean
# ar.median<-pred.AR$Median
# write.csv(ar.mean,"ar.mean.csv")
# write.csv(ar.median,"ar.median.csv")
#summary save
# as.data.frame(assign(paste("summary",i, sep=""),capture.output(summary(train_AR))))
#   write(get(paste0('summary',i)),paste0("summary",i,'.txt'),append = FALSE)  
#parameter save
  # as.data.frame(assign(paste("parameter",i, sep=""),capture.output(train_AR$parameter)))
  # write(get(paste0('parameter',i)),paste0("parameter",i,'.txt'),append = FALSE)  
     }
  
################################GP#####################
gp.mtx<-matrix(NA,50,ncol=7)
seed<-matrix(NA,50,ncol=3)
set.seed(1234)
for(i in 1:50){
  seed[i,]<-sample(1:30,3,replace = FALSE)
}
set.seed(1234)
for(i in 1:50){
  set.seed(seed[i,])
  validation<-sample(asos.station, 6, replace = FALSE)
  train<-as.data.frame(data[!(data$code) %in% validation,])
  test<-as.data.frame(data[data$code %in% validation,])
  priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
                     beta.prior=Norm(0,10^4))
  pred.coords<-unique(cbind(test$air.lon,test$air.lat))
  
  set.seed(1234)
  train_gp <- spT.Gibbs(formula=o3 ~max.temp+wind+humid+sun,
                        time.data=spT.time(t.series=184, segment=3),
                        data=train,
                        model="GP",
                        coords=~asos.lon+asos.lat,
                        priors=priors,
                        nItr=6000,
                        nBurn=1000,
                        cov.fnc="exponential",
                        report=5000,
                        #distance.method="geodetic:km",
                        #spatial.decay=spatial.decay,
                        scale.transform = "SQRT",
                        spatial.decay=spT.decay(distribution="FIXED", value=0.013),
                        tol.dist=0.01)
  
  pred.gp <- predict(train_gp, newdata=test, newcoords=pred.coords, type="spatial", tol.dist=0.01)
  library(coda)
  # plot(data_gp)
  # summary(data_gp)
  # autocorr.diag(as.mcmc(data_gp))
  # gp.mean<-pred.gp$Mean
 #  # gp.median<-pred.gp$Median
  
 # gp.mean<-pred.gp$Mean
 # gp.median<-pred.gp$Median
 
  # autocorr.diag(as.mcmc(data_gp))
  gp.mtx[i,]<-spT.validation(as.numeric(paste(test$o3)),c(pred.gp$Median))
  setwd("E:/Dain/연구/졸업논문/결과/결과2/validation/GP")
  # write.csv(gp.mean,"gp.mean.csv")
  # write.csv(gp.mean,"gp.median.csv")
  write.table(gp.mtx,"gp.mtx.txt",sep=",")
  #summary save
  # as.data.frame(assign(paste("summary",i, sep=""),capture.output(summary(train_gp))))
  # write(get(paste0('summary',i)),paste0("summary",i,'.txt'),append = FALSE)  
  #parameter save
  # as.data.frame(assign(paste("parameter",i, sep=""),capture.output(train_gp$parameter)))
  # write(get(paste0('parameter',i)),paste0("parameter",i,'.txt'),append = FALSE)  
}



# #sample1
# library(ggmap)
# air<-as.data.frame(cbind(unique(data1$air.lon),unique(data1$air.lat)))
# colnames(air)<-c("air.lon","air.lat")
# asos<-as.data.frame(cbind(unique(data1$asos.lon),unique(data1$asos.lat)))
# colnames(asos)<-c("asos.lon","asos.lat")
# 
# p<-get_map(location = c(127.5, 35.8), zoom =7, maptype="toner-lite", source = "stamen")
# ggmap(p,legend="top")+geom_point(data=air, aes(x=air.lon, y=air.lat),size=5, color="red",pch=8, alpha=0.7)+geom_point(data=asos, aes(x=asos.lon, y=asos.lat), size=4, color="blue", alpha=0.7)+theme(legend.position = "top") 
# 
# #sample2
# library(ggmap)
# air<-as.data.frame(cbind(unique(data2$air.lon),unique(data2$air.lat)))
# colnames(air)<-c("air.lon","air.lat")
# asos<-as.data.frame(cbind(unique(data2$asos.lon),unique(data2$asos.lat)))
# colnames(asos)<-c("asos.lon","asos.lat")
# 
# p<-get_map(location = c(127.5, 35.8), zoom =7, maptype="toner-lite", source = "stamen")
# ggmap(p,legend="top")+geom_point(data=air, aes(x=air.lon, y=air.lat),size=5, color="red",pch=8, alpha=0.7)+geom_point(data=asos, aes(x=asos.lon, y=asos.lat), size=4, color="blue", alpha=0.7)+theme(legend.position = "top") 
# 
# #sample3
# air<-as.data.frame(cbind(unique(data3$air.lon),unique(data3$air.lat)))
# colnames(air)<-c("air.lon","air.lat")
# asos<-as.data.frame(cbind(unique(data3$asos.lon),unique(data3$asos.lat)))
# colnames(asos)<-c("asos.lon","asos.lat")
# 
# p<-get_map(location = c(127.5, 35.8), zoom =7, maptype="toner-lite", source = "stamen")
# ggmap(p,legend="top")+geom_point(data=air, aes(x=air.lon, y=air.lat),size=5, color="red",pch=8, alpha=0.7)+geom_point(data=asos, aes(x=asos.lon, y=asos.lat), size=4, color="blue", alpha=0.7)+theme(legend.position = "top") 


