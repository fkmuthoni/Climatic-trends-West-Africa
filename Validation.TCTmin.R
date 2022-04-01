##############################################################
#Extract & Validate Tmin
##############################################################
#Import gauged Tmin
Tmin.gauge.wa = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/TminGhana.csv', header = T, sep=',')
Tmin.gauge.wa.longDF <- Tmin.gauge.wa %>% gather(Day, Tmin, X1:X31)
Tmin.gauge.wa.longDF.monthly = as.data.frame(with(Tmin.gauge.wa.longDF, tapply(Tmin, list(Station, YearMonth), mean,na.rm=T))) 
Tmin.gauge.wa.longDF.monthly$Station=row.names(Tmin.gauge.wa.longDF.monthly)
Tmin.gauge.wa.longDF.monthly.long = Tmin.gauge.wa.longDF.monthly %>% gather(YearMonth, Tmin,1:360, na.rm=T)                                                                                 
Tmin.gauge.wa.longDF.monthly.long$StationYearMonth =paste0(Tmin.gauge.wa.longDF.monthly.long$Station, Tmin.gauge.wa.longDF.monthly.long$YearMonth) 

#Import monthly gauge data from GlobalSummary of Month (GSO)#
GGSO1722890Tmin = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/GSO1722890GhanaBeninTogoMaliIvorycoastBFTmin.csv', sep=',', header=T)
##Merge Ghana Met gauge & GSO monthly
Tmin.gauge.wa.longDF.monthly.long= rbind(Tmin.gauge.wa.longDF.monthly.long, GGSO1722890Tmin)

#Extract Tmin grid values from points

names (terraclimTmin.bk)= date.chirps.mon.yrmonth
TCWATmin.extract<-data.frame(raster::extract(terraclimTmin.bk,stationsgpsWASPF, method = 'simple'))
TCWATmin.extract$Station = stationsgpsWA$Station
TCWATmin.extract.longDF = TCWATmin.extract %>% gather(YearMonth, Tmin, X198101:X201712, na.rm = T)
TCWATmin.extract.longDF$YearMonth = gsub('X', '', TCWATmin.extract.longDF$YearMonth)
TCWATmin.extract.longDF$StationYearMonth =paste0(TCWATmin.extract.longDF$Station,TCWATmin.extract.longDF$YearMonth)

#####Merge Gauge and TerraClimate Data
#Merge gauge and Tmin
Tmin.gauge.merge<-merge.data.frame(Tmin.gauge.wa.longDF.monthly.long,TCWATmin.extract.longDF, by= 'StationYearMonth')
write.table(Tmin.gauge.merge,"E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/Tmin.gauge.merge.csv", sep=",", col.names=T,row.names=T, quote=T, na="")
Tmin.gauge.merge = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/Tmin.gauge.merge.csv', header=T, sep=',')

#####Statistical Evaluation of gauge vs TC Tmin####

##############################################################
####plot hydroGoF validation stastistics (r, me,PBIAS, NSE, R2)####
########################
#Tmin.gauge.compare.esa71.longDF.merge.indp.nz = subset(Tmin.gauge.compare.esa71.longDF.merge.indp, subset=Tmin.gauge.compare.esa71.longDF.merge.indp$Gauge <= 600)
with(Tmin.gauge.merge, ggof(obs=Gauge,sim=TerraClimate, ftype="o", FUN=mean, ylab='mm', xlab= 'Time Series', main= 'Tmin Gauge vs TerraClimate'))
with(Tmin.gauge.merge,KGE(sim=TerraClimate, obs=Gauge, s=c(1,1,1), na.rm=TRUE, method="2009",out.type="full"))

#scatterplot(Tmin~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots='xy', xlim=c(0, 400),data=Tmin.gauge.merge, subset=Tmin.gauge.merge$Station.x=="Babile")
TminvalidWA=scatterplot(TerraClimate~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots=F, xlim=c(10, 35),ylim=c(10, 35), main = 'Tmin', data=Tmin.gauge.merge)
text(22.3, 33, labels= "KGE= 0.83, r = 0.84, ?? = 0.99 and ?? = 0.94, RMSE=1.33, n= 7499", cex= 1.1, col='red')

#### Scatter plots Tmin ggplot####
Tmin.gauge.merge.sp = ggscatter(Tmin.gauge.merge, x = "Gauge", y = "TerraClimate",
                            add = "reg.line",              
                            add.params = list(color = "red"),
                            #color = "blue" 
                            shape = 2) 

Tmin.gauge.merge.sp = Tmin.gauge.merge.sp+
  coord_cartesian(xlim=c(10, 35),ylim=c(10, 35))+
  labs(subtitle = "Tmin")+
  annotate("text", label = "KGE= 0.85, r = 0.87, ?? = 0.99, ?? = 0.93 \n RMSE=1.31, n= 8668", x = 22, y = 33,size = 3, fontface="bold", colour = "red")
