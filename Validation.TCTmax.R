##############################################################
#Extract & Validate Tmax
##############################################################
#Import gauged Tmax
Tmax.gauge.wa = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/Tmax_Ghana.csv', header = T, sep=',')
Tmax.gauge.wa.longDF <- Tmax.gauge.wa %>% gather(Day, Tmax, X1:X31)
Tmax.gauge.wa.longDF.monthly = as.data.frame(with(Tmax.gauge.wa.longDF, tapply(Tmax, list(Station, YearMonth), mean,na.rm=T))) 
Tmax.gauge.wa.longDF.monthly$Station=row.names(Tmax.gauge.wa.longDF.monthly)
Tmax.gauge.wa.longDF.monthly.long = Tmax.gauge.wa.longDF.monthly %>% gather(YearMonth, Tmax,1:360, na.rm=T)                                                                                 
Tmax.gauge.wa.longDF.monthly.long$StationYearMonth =paste0(Tmax.gauge.wa.longDF.monthly.long$Station, Tmax.gauge.wa.longDF.monthly.long$YearMonth) 

#Import monthly gauge data from GlobalSummary of Month (GSO)#
GGSO1722890Tmax = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/GSO1722890GhanaBeninTogoMaliIvorycoastBFTmax.csv', sep=',', header=T)
##Merge Ghana Met gauge & GSO monthly
Tmax.gauge.wa.longDF.monthly.long= rbind(Tmax.gauge.wa.longDF.monthly.long, GGSO1722890Tmax)

####Extract TC-Tmax grid values from points####
names (terraclimTmax.bk)= date.chirps.mon.yrmonth
TCWATmax.extract<-data.frame(raster::extract(terraclimTmax.bk,stationsgpsWASPF, method = 'simple'))
TCWATmax.extract$Station = stationsgpsWA$Station
TCWATmax.extract.longDF = TCWATmax.extract %>% gather(YearMonth, Tmax, X198101:X201712, na.rm = T)
TCWATmax.extract.longDF$YearMonth2 = gsub('X', '', TCWATmax.extract.longDF$YearMonth)
TCWATmax.extract.longDF$StationYearMonth =paste0(TCWATmax.extract.longDF$Station,TCWATmax.extract.longDF$YearMonth2)

#####Merge Gauge and TC Data
#Merge gauge and Tmax
Tmax.gauge.merge<-merge.data.frame(Tmax.gauge.wa.longDF.monthly.long,TCWATmax.extract.longDF, by= 'StationYearMonth')
write.table(Tmax.gauge.merge,"E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/Tmaxgaugemerge.csv", sep=",", col.names=T,row.names=T, quote=T, na="")
Tmax.gauge.merge = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/Tmaxgaugemerge.csv', header=T, sep=',')

####Extract CHIRTMax grid values from points####
names (chirtmax.wa)= date.chirtmax.mon.yrmonth
chirtmaxwa.extract<-data.frame(raster::extract(chirtmax.wa,stationsgpsWASPF, method = 'simple'))
chirtmaxwa.extract$Station = stationsgpsWA$Station
chirtmaxwa.extract.longDF = chirtmaxwa.extract %>% gather(YearMonth, Tmax, X198301:X201612, na.rm = T)
chirtmaxwa.extract.longDF$YearMonth2 = gsub('X', '', chirtmaxwa.extract.longDF$YearMonth)
chirtmaxwa.extract.longDF$StationYearMonth =paste0(chirtmaxwa.extract.longDF$Station,chirtmaxwa.extract.longDF$YearMonth2)

#####Merge Gauge and CHIRTMax Data
#Merge gauge and Tmax
chirTmax.gauge.merge<-merge.data.frame(Tmax.gauge.wa.longDF.monthly.long,chirtmaxwa.extract.longDF, by= 'StationYearMonth')
write.table(chirTmax.gauge.merge,"E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/chirTmaxgaugemerge.csv", sep=",", col.names=T,row.names=T, quote=T, na="")
chirTmax.gauge.merge = read.csv('E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/Ghana/chirTmaxgaugemerge.csv', header=T, sep=',')

##############################################################
####plot hydroGoF validation ststistics (r, me,PBIAS, NSE, R2)####
########################
#TC
with(Tmax.gauge.merge, ggof(obs=Gauge,sim=TerraClimate, ftype="o", FUN=mean, ylab='mm', xlab= 'Time Series'))
with(Tmax.gauge.merge,KGE(sim=TerraClimate, obs=Gauge, s=c(1,1,1), na.rm=TRUE, method="2009",out.type="full"))

##CHIRTmax
with(chirTmax.gauge.merge, ggof(obs=Gauge,sim=CHIRTmax, ftype="o", FUN=mean, ylab='mm', xlab= 'Time Series'))
with(chirTmax.gauge.merge,KGE(sim=CHIRTmax, obs=Gauge, s=c(1,1,1), na.rm=TRUE, method="2009",out.type="full"))

##################################Scater plots for gauge vs Tmax Tmax per station
#scatterplot(Tmax~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots='xy', xlim=c(0, 400),data=Tmax.gauge.merge, subset=Tmax.gauge.merge$Station.x=="Babile")
TmaxvalidWA=scatterplot(TerraClimate~Gauge, reg.line=lm, lwd=2, lty=1, smooth=F, boxplots=F, xlim=c(25, 45),ylim=c(25, 45),main= 'Tmax', data=Tmax.gauge.merge)
text(34.8, 45, labels= "KGE= 0.95, r = 0.96, ?? = 0.99 and ?? = 0.97, RMSE=0.94, n= 7648", cex= 1.1, col='red')

#### Scatter plots Tmax ggplot####
##TC
sp.Tmax.TCGauge = ggscatter(Tmax.gauge.merge, x = "Gauge", y = "TerraClimate",
                            add = "reg.line",               # Add regression line
                            add.params = list(color = "red"),
                            #color = "blue" # Color by groups "cyl"
                            shape = 7) # Change point shape by groups "cyl"

sp.Tmax.TCGauge = sp.Tmax.TCGauge+
  coord_cartesian(xlim=c(25, 45),ylim=c(25, 45))+
  labs(subtitle = "Tmax")+
  annotate("text", label = "KGE= 0.95, r = 0.96, ?? = 0.99, ?? = 0.97 \n RMSE=0.92, n= 9497", x = 33, y = 44,size = 3, fontface="bold", colour = "red")
##CHIRTmax

sp.chirTmax.TCGauge = ggscatter(chirTmax.gauge.merge, x = "Gauge", y = "CHIRTmax",
                            add = "reg.line",               # Add regression line
                            add.params = list(color = "red"),
                            #color = "blue" # Color by groups "cyl"
                            shape = 7) # Change point shape by groups "cyl"

sp.chirTmax.TCGauge = sp.chirTmax.TCGauge+
  coord_cartesian(xlim=c(25, 45),ylim=c(25, 45))+
  grids(linetype = "dashed")+
  labs(subtitle = "CHIRTmax")+
  annotate("text", label = "KGE= 0.92, r = 0.97, ?? = 0.99, ?? = 0.93 \n RMSE=0.88, n= 8505", x = 33, y = 44,size = 3, fontface="bold", colour = "red")

####Arrange many panels####
ggarrange(chirps.gauge.merge.sp, TCgauge.merge.sp, sp.Tmax.TCGauge, sp.chirTmax.TCGauge, Tmin.gauge.merge.sp, labels = c("a", "b", "c","d", "d"),ncol = 2, nrow = 3)


