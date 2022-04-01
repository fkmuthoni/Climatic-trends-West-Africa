##########################################################################################
####Validate TerraClimate (Tmin/Tmax/PPT) and CHIRPS-v2 Grids with gauge data####
##########################################################################################
#Make date time series
date.chirps.mon <- seq.Date(as.Date("1981-01-01"), as.Date("2017-12-01"), by='month')

#remove dash (-) between date vectors
#date.chirps.mon.yrmonth = gsub('//-', '', date.chirps.mon)
date.chirps.mon.yrmonth = gsub('-', '', date.chirps.mon)
date.chirps.mon.yrmonth = stri_sub(date.chirps.mon.yrmonth, from = 0L, to = 6L)

#date.chirps.mon.dc <- decimal_date(date.chirps.mon)
date.chirps.yr <- seq.Date(as.Date("1981-01-01"), as.Date("2017-12-01"), by='year')
#date.chirps.yr.dc <- decimal_date(date.chirps.yr)
#date.chirps.yr.dc <-seq(from = 1981.1, length.out = 37, by = 1)
chiprs.annual.names<-paste0('chirps', date.chirps.yr.dc)


#Import GPS positions of gauge stations
stationsgpsWA= read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/GPS_stations_.csv')
stationsgpsWASPF=stationsgpsWA[,3:4]
coordinates(stationsgpsWASPF) = ~Long+Lat
crs(stationsgpsWASPF)=c('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

#Import gauged rainfall
 rainfall.gauge.wa = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/PPT-Daily_Ghana.csv', header = T, sep=',')
 rainfall.gauge.wa.longDF <- rainfall.gauge.wa %>% gather(Day, Rain, X1:X31)
 rainfall.gauge.wa.longDF.monthly = as.data.frame(with(rainfall.gauge.wa.longDF, tapply(Rain, list(Station, YearMonth), sum,na.rm=T))) 
 rainfall.gauge.wa.longDF.monthly$Station=row.names(rainfall.gauge.wa.longDF.monthly)
 rainfall.gauge.wa.longDF.monthly.long = rainfall.gauge.wa.longDF.monthly %>% gather(YearMonth, Rain,1:361, na.rm=T)                                                                                 
 rainfall.gauge.wa.longDF.monthly.long$StationYearMonth =paste0(rainfall.gauge.wa.longDF.monthly.long$Station, rainfall.gauge.wa.longDF.monthly.long$YearMonth) 
 
 #Import monthly gauge data from GlobalSummary of Month (GSO)#
 GSO1722890GhBeToMaIvPPT = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/GSO1722890GhanaBeninTogoMaliIvorycoastBFPPT.csv', sep=',', header=T)
 ##Merge Ghana Met gauge & GSOM climatologies
 rainfall.gauge.wa.longDF.monthly.long= rbind(rainfall.gauge.wa.longDF.monthly.long, GSO1722890GhBeToMaIvPPT)
 
 #Extract grid values from points
#Extract TCppt
 
 names (terraclimppt.bk)= date.chirps.mon.yrmonth
 TCWA.extract<-data.frame(raster::extract(terraclimppt.bk,stationsgpsWASPF, method = 'simple'))
 TCWA.extract$Station = stationsgpsWA$Station
 TCWA.extract.longDF = TCWA.extract %>% gather(YearMonth, Rain, X198101:X201712, na.rm = T)
 TCWA.extract.longDF$YearMonth = gsub('X', '', TCWA.extract.longDF$YearMonth)
 TCWA.extract.longDF$StationYearMonth =paste0(TCWA.extract.longDF$Station,TCWA.extract.longDF$YearMonth)
 

#Extract Chirpsv2
names (chirps.wa.brick19812016)= date.chirps.mon.yrmonth
chirpsWA.extract<-data.frame(raster::extract(chirps.wa.brick19812016,stationsgpsWASPF, method = 'simple'))
chirpsWA.extract$Station = stationsgpsWA$Station
chirpsWA.extract.longDF = chirpsWA.extract %>% gather(YearMonth, Rain, X198101:X201712, na.rm = T)
chirpsWA.extract.longDF$YearMonth = gsub('X', '', chirpsWA.extract.longDF$YearMonth)
chirpsWA.extract.longDF$StationYearMonth =paste0(chirpsWA.extract.longDF$Station,chirpsWA.extract.longDF$YearMonth)
#####Merge Gauge and CHirps rainfall
#Merge gauge and CHIRPS
chirps.gauge.merge<-merge.data.frame(rainfall.gauge.wa.longDF.monthly.long,chirpsWA.extract.longDF, by= 'StationYearMonth')
write.table(chirps.gauge.merge,"E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/chirpsgaugemergeWA.csv", sep=",", col.names=T,row.names=T, quote=T, na="")
chirps.gauge.merge = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/chirpsgaugemergeWA.csv', header=T, sep=',')

#Merge gauge and TerraClimate
TCgauge.merge<-merge.data.frame(rainfall.gauge.wa.longDF.monthly.long,TCWA.extract.longDF, by= 'StationYearMonth')
write.table(TCgauge.merge,"E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/TCgaugemergeWA.csv", sep=",", col.names=T,row.names=T, quote=T, na="")
TCgauge.merge = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/TCgaugemergeWA.csv', header=T, sep=',')

##################################Scater plots for gauge vs CHIRPS rain per station
#scatterplot(CHIRPS~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots='xy', xlim=c(0, 400),data=chirps.gauge.merge, subset=chirps.gauge.merge$Station.x=="Babile")
#CHIRPS-v2
CHIRPSvalidWA=scatterplot(CHIRPS~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots=F, xlim=c(0, 600), ylim=c(0, 600),main = 'CHIRPS Rainfall', data=chirps.gauge.merge)
text(300, 570, labels= "KGE= 0.84, r = 0.88, ?? = 0.97 and ?? = 0.90, RMSE=43.68, n= 4390", cex= 1.1, col='red')

#TC 
scatterplot(TerraClimate~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots=F, xlim=c(0, 600), ylim=c(0, 600),main = 'TC Rainfall',data=TCgauge.merge)
text(300, 550, labels="KGE=0.67, r=0.72, ??=0.93 and ??=0.83, RMSE=65.53, n=4141", cex= 1.1, col='red')

##############################################################
#plot hydroGoF validation ststistics (r, me,PBIAS, NSE, R2)
########################
#chirps.gauge.compare.esa71.longDF.merge.indp.nz = subset(chirps.gauge.compare.esa71.longDF.merge.indp, subset=chirps.gauge.compare.esa71.longDF.merge.indp$Gauge <= 600)
#ggof
with(chirps.gauge.merge, ggof(obs=Gauge,sim=CHIRPS, ftype="o", FUN=mean, ylab='mm', xlab= 'Time Series'))
with(TCgauge.merge, ggof(obs=Gauge,sim=TerraClimate, ftype="o", FUN=mean, ylab='mm', xlab= 'Time Series'))

#KGE
with(chirps.gauge.merge,KGE(obs=Gauge,sim=CHIRPS,  s=c(1,1,1), na.rm=TRUE, method="2009",out.type="full"))
with(TCgauge.merge,KGE(sim=TerraClimate, obs=Gauge, s=c(1,1,1), na.rm=TRUE, method="2009",out.type="full"))


#### Scatter plots TC-PPT ggplot####
chirps.gauge.merge.sp = ggscatter(chirps.gauge.merge, x = "Gauge", y = "CHIRPS",add = "reg.line", add.params = list(color = "red"), xlab = "Gauge Rainfall (mm)", ylab = "CHIRPS Rainfall (mm)", shape = 7) 
chirps.gauge.merge.sp = chirps.gauge.merge.sp+
  coord_cartesian(xlim=c(0, 600),ylim=c(0, 600))+
  grids(linetype = "dashed")+
  geom_abline(linetype = "dashed", color = "blue", size = 1)+
  labs(subtitle = "CHIRPS-v2 Rainfall")+
  annotate("text", label = "KGE= 0.84, r = 0.88, ?? = 0.97, ?? = 0.90 \n RMSE = 44.14, n= 4505", x = 300, y = 580,size = 3, fontface="bold", colour = "red")

#### Scatter plots CHIRPS-v2 ggplot####
TCgauge.merge.sp = ggscatter(TCgauge.merge, x = "Gauge", y = "TerraClimate",add = "reg.line", add.params = list(color = "red"),xlab = "Gauge Rainfall (mm)", ylab = "TC Rainfall (mm)", shape = 7)

TCgauge.merge.sp = TCgauge.merge.sp+
  coord_cartesian(xlim=c(0, 600),ylim=c(0, 600))+
  grids(linetype = "dashed")+
  geom_abline(linetype = "dashed", color = "blue", size = 1)+
  labs(subtitle = "TC Rainfall")+
  annotate("text", label = "KGE=0.68, r=0.74, ?? = 0.92 and ?? = 0.83 \n RMSE=64.12, n=4536", x = 300, y = 580,size = 3, fontface="bold", colour = "red")

#### Scatter plots TC-Tmax ggplot####
sp.Tmax.TCGauge = ggscatter(Tmax.gauge.merge, x = "Gauge", y = "TerraClimate", add = "reg.line", add.params = list(color = "red"), xlab = "Gauge Tmax (\u00b0C)", ylab = "TC-Tmax (\u00b0C)",shape = 7) 

sp.Tmax.TCGauge = sp.Tmax.TCGauge+
  coord_cartesian(xlim=c(25, 45),ylim=c(25, 45))+
  grids(linetype = "dashed")+
  geom_abline(linetype = "dashed", color = "blue", size = 1)+
  labs(subtitle = "TC Tmax")+
  annotate("text", label = "KGE= 0.95, r = 0.96, ?? = 0.99, ?? = 0.97 \n RMSE=0.92, n= 9497", x = 33, y = 44,size = 3, fontface="bold", colour = "red")

####CHIRTmax ggplot####

sp.chirTmax.TCGauge = ggscatter(chirTmax.gauge.merge, x = "Gauge", y = "CHIRTmax",add = "reg.line",add.params = list(color = "red"), xlab = "Gauge Tmax (\u00b0C)", ylab = "CHIRTSMax-Tmax (\u00b0C)", shape = 7)

sp.chirTmax.TCGauge = sp.chirTmax.TCGauge+
  coord_cartesian(xlim=c(25, 45),ylim=c(25, 45))+
  grids(linetype = "dashed")+
  geom_abline(linetype = "dashed", color = "blue", size = 1)+
  labs(subtitle = "CHIRTmax")+
  annotate("text", label = "KGE= 0.92, r = 0.97, ?? = 0.99, ?? = 0.93 \n RMSE=0.88, n= 8505", x = 33, y = 44,size = 3, fontface="bold", colour = "red")

#### Scatter plots TC-Tmin ggplot####
Tmin.gauge.merge.sp = ggscatter(Tmin.gauge.merge, x = "Gauge", y = "TerraClimate",add = "reg.line",add.params = list(color = "red"), xlab = "Gauge Tmin (\u00b0C)", ylab = "TC-Tmin (\u00b0C)", shape = 2) 

Tmin.gauge.merge.sp = Tmin.gauge.merge.sp+
  coord_cartesian(xlim=c(10, 35),ylim=c(10, 35))+
  labs(subtitle = "TC Tmin")+
  grids(linetype = "dashed")+
  geom_abline(linetype = "dashed", color = "blue", size = 1)+
  annotate("text", label = "KGE= 0.85, r = 0.87, ?? = 0.99, ?? = 0.93 \n RMSE=1.31, n= 8668", x = 22, y = 33,size = 3, fontface="bold", colour = "red")

####Arrange many panels####
ggarrange(chirps.gauge.merge.sp, TCgauge.merge.sp, sp.chirTmax.TCGauge, sp.Tmax.TCGauge,'',Tmin.gauge.merge.sp, labels = c("a", "b", "c","d",'', "e"),ncol = 2, nrow = 3)

