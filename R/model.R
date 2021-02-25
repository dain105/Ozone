data<-read.csv("C:/Users/USER/Desktop/Dain/연구/졸업논문/data/final data/no.NA.data.csv")
head(data)
summary(lm(o3~max.temp+wind+humid+sun, data=data))

# library(ggmap)
#  air<-as.data.frame(cbind(unique(data$air.lon),unique(data$air.lat)))
#  colnames(air)<-c("air.lon","air.lat")
#  asos<-as.data.frame(cbind(unique(data$asos.lon),unique(data$asos.lat)))
#  colnames(asos)<-c("asos.lon","asos.lat")
#  p<-get_map(location = c(127.5, 35.8), zoom =7, maptype="toner-lite", source = "stamen")
#  ggmap(p,legend="top")+geom_point(data=air, aes(x=air.lon, y=air.lat),size=2, color="red",pch=8, alpha=0.7)+geom_point(data=asos, aes(x=asos.lon, y=asos.lat), size=2, color="blue", alpha=0.7)+theme(legend.position = "top") +scale_color_manual(name = "Year", labels = c(1940, 1960), values = c(1,2))
# 
# cor(data[data$code==119, "o3"], data[data$code==108, "o3"])

pred.coords<-unique(cbind(data$air.lon,data$air.lat))
library(spTimer)
spatial.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.002)
# spatial.decay<-spT.decay(distribution=Unif(0.01,0.02),npoints=5)
# spatial.decay<-spT.decay(distribution="FIXED", value=0.006)


#########AR#############
priors<-spT.priors(model="AR",inv.var.prior=Gamm(2,1),
                   beta.prior=Norm(0,10^4))


#184일 3년 33개 관측소
# para.spatial<-seq(0.008,0.05,0.002)
# AR.mtx <- matrix(rep(NA, 7*length(para.spatial)),ncol=7)
set.seed(12343)
data_AR <- spT.Gibbs(formula=o3 ~ max.temp+wind+humid+sun,
                     time.data=spT.time(t.series=184, segment=3),
                     data=data,
                     model="AR",
                     coords=~asos.lon+asos.lat,
                     priors=priors,
                     nItr=100,
                     nBurn=10,
                     cov.fnc="exponential",
                     report=100,
                     spatial.decay=spT.decay(distribution=Gamm(2,1), tuning=0.08),
                     scale.transform = "SQRT",
                     tol.dist=0.01)
# plot(data_AR)
data_AR$PMCC
spT.pCOVER()


str(as.vector(data$o3))

summary(data_AR)
pred.AR <- predict(data_AR, newdata=data, newcoords=pred.coords, type="spatial", tol.dist=0.01)
summary(data_AR)
fitted(data_AR)
data$AR$model
# AR.median<-pred.AR$Median
# AR.mean<-pred.AR$Mean
# setwd("S:/Users/Dain/연구/졸업논문/결과/trace plot/사전분포/AR")
# write.csv(AR.mean,"AR.mean.csv")
AR.mtx<-spT.validation(as.numeric(paste(data$o3)),c(pred.AR$Median))


plot(AR.mtx[,3], axes=F, type="o", xlab="spatial decay parameter", ylab="RMSE")
axis(1, at=1:length(para.spatial), para.spatial)
axis(2)
box()
################################GP#####################
priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
                   beta.prior=Norm(0,10^4))

#184일 3년 33개 관측소


para.spatial<-seq(0.0001,0.03,0.001)
gp.mtx <- matrix(rep(NA, 7*length(para.spatial)),ncol=7)
set.seed(12343)

data_gp <- spT.Gibbs(formula=o3 ~ max.temp+wind+humid,
                     time.data=spT.time(t.series=184, segment=3),
                     data=data,
                     model="GP",
                     coords=~asos.lon+asos.lat,
                     priors=priors,
                     nItr=13000,
                     nBurn=3000,
                     cov.fnc="exponential",
                     #report=5000,
                     #distance.method="geodetic:km",
                     #spatial.decay=spatial.decay,
                     scale.transform = "SQRT",
                     spatial.decay=spT.decay(distribution=Gamm(2,1), tuning=0.08),
                     tol.dist=0.01)

pred.gp <- predict(data_gp, newdata=data, newcoords=pred.coords, type="spatial", tol.dist=0.01)
 library(coda)
plot(data_gp)
# summary(data_gp)
# autocorr.diag(as.mcmc(data_gp))
# gp.mean<-pred.gp$Mean
# gp.median<-pred.gp$Median
# setwd("S:/Users/Dain/연구/졸업논문/결과/trace plot/사전분포/GP")
# write.csv(gp.mean,"gp.mean.csv")
# autocorr.diag(as.mcmc(data_gp))
gp.mtx<-spT.validation(as.numeric(paste(data$o3)),c(pred.gp$Median))

plot(gp.mtx[,3], axes=F, type="o", xlab="spatial decay parameter", ylab="RMSE")
axis(1, at=1:length(para.spatial), para.spatial)
axis(2)
box()
##############################GPP############################
priors<-spT.priors(model="GPP",inv.var.prior=Gamm(2,1),
                   beta.prior=Norm(0,10^4))

knots<-spT.grid.coords(Longitude=c(max(data$asos.lon),
                                   min(data$asos.lon)),
                       Latitude=c(max(data$asos.lat),
                                  min(data$asos.lat)), 
                       by=c(4,4))
#184일 3년 33개 관측소
para.spatial<-seq(0.005,0.008,0.0005)
para.spatial<-seq(0.03,0.1,0.005)
para.spatial<-seq(0.03,0.04,0.0005)

GPP.mtx <- matrix(rep(NA, 7*length(para.spatial)),ncol=7)
set.seed(12343)
for (i in 1:length(para.spatial)){
data_GPP <- spT.Gibbs(formula=o3 ~ mean.temp+min.temp+max.temp+wind+humid+sun,
                     time.data=spT.time(t.series=184, segment=3),
                     data=data,
                     model="GPP",
                     coords=~asos.lon+asos.lat,
                     knots.coords=knots,
                     priors=priors,
                     nItr=100,
                     nBurn=10,
                     cov.fnc="exponential",
                     report=100,
                     #distance.method="geodetic:km",
                     #spatial.decay=spatial.decay,
                     scale.transform = "SQRT",
                     spatial.decay=spT.decay(distribution="FIXED", value=para.spatial[i]),
                     tol.dist=0.01)

# plot(data_GPP)
# summary(data_GPP)
# autocorr.diag(as.mcmc(data_GPP))
pred.GPP <- predict(data_AR, newdata=data, newcoords=pred.coords, type="spatial", tol.dist=0.01)
# GPP.median<-pred.GPP$Median
# GPP.mean<-pred.GPP$Mean
# setwd("S:/Users/Dain/연구/졸업논문/결과/trace plot/사전분포/GPP")
# write.csv(GPP.median,"GPP.median.csv")
# autocorr.diag(as.mcmc(data_GPP))
GPP.mtx[i,]<-spT.validation(as.numeric(paste(data$o3)),c(pred.GPP$Median))
}

plot(GPP.mtx[,3], axes=F, type="o", xlab="spatial decay parameter", ylab="RMSE")
axis(1, at=1:length(para.spatial), para.spatial)
axis(2)
box()
