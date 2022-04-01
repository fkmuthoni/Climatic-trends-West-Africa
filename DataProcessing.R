####Generate precipitation and temperature trends for West africa####
###################################################
#Load required packages
##################################################################################
#require(BiodiversityR)
#BiodiversityRGUI()
#require (ncdf4)
#require(ncdf)
#require (RNetCDF)
require(raster)
#require (maptools)
require(modifiedmk)
require (fields)
#library(rgdal)
library(rasterVis)
#library(MODIS)
#library(MODISTools)
library(sp)
require (shapefiles)
#require(greenbrown)
#require (gdalUtils)
#require(rgeos)
#require(bfastSpatial)
require(stringi)
require(stringr)
#require(lubridate)
require(colorRamps)
require(RColorBrewer)
require(grDevices)
#require (classInt)
#require(data.table)
library(tidyr)
library("tidyverse")
#library(dplyr)
#require(plyr)
#require (vegan)
require(doParallel)
#require(parallel)
#require(foreach)
#require(rtsa)
#require(gtools)
#require(RStoolbox)
#require (tibble)
#require(remote)
#require(sinkr)
#require(devtools)
#devtools::install_github("e-sensing/sits")
#require(sits)
#library (dtwclust)
#require(clValid)
#require(compositions)
require(EcoGenetics)
source('ecotheilsen_modified.R')
require(hydroGOF)
require(multipanelfigure)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(grid)
require(cartography)
library(sf)
require(reshape)
require(FSA)
require(PMCMR)
#require(PMCMRplus)
#library(Hmisc)
library(grid)
require(NbClust)
#********************************************************************

##Set working directory##
setwd("E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED")

#load("E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/.RData")

#*************************************************
####Make clusters for multi-core processing####
#beginCluster(7, type='SOCK')
#endCluster()

############################################################################
####Process gridded data West Africa####
################################################################################################################
####Import countries shp####
countries.wa.shp<-shapefile('E:/Francis_IITA/GIS_RS/GIS/Admin_political/Africa/AR-WA/AR_WA7.shp')
#demo.mali.ghana = shapefile('E:/Francis_IITA/GIS_RS/GPS/AR-ESA/Africa/AR_Ghana_Mali_Demo_sites.shp')
#demo.mali.ghana = shapefile('E:/Francis_IITA/GIS_RS/GPS/AR-WA/AR-Demo-Ghana-Mali.shp')

#Import AEZ layer##
#aez.wa = shapefile('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/Aez_clipWA6.shp')
aezWArst = raster('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/aezWA6rst.tif')
aezWArst.fun=function(x) { x[x==10] <- NA; return(x)}
aezWArst <- calc(aezWArst, aezWArst.fun)
aezWArst.tc = raster('aezWArstTC.tif')#AEZ resampled to TC resolution
###############################################################################################################################################################################################
#Import mask layers for water, protected areas and urban areas
###############################################################################################################################################################################################
#Import water mask
#water.mask.wa = raster('E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Water Mask/watermaskWA-clss.tif')
watermaskWa = raster('watermaskWA.tif')
watermaskWa.fun=function(x) { x[x>0.7] <- NA; return(x)}
watermaskWa <- calc(watermaskWa, watermaskWa.fun)
watermaskWa = projectRaster(watermaskWa,terraclimppt.annual.37yrs[[1]], method= 'ngb')
watermaskWa = (watermaskWa *0)+1
##Import protected areas and urban masks
pa.wa.mask = 
urban.wa.mask =
###############################################################################################################################################################################################
#Process CHIRPS rainfall
###############################################################################################################################################################################################
chirps.wa.brick19812016<-stack('E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Climate/chirps.wa.19812016.tif')
chirps.names.37 = seq.Date(as.Date("1981-01-01"), as.Date("2017-12-31"), by='year')
#names(chirps.wa.19812016)<-chirps.names
#chirps.wa.brick19812016.5yrs<-chirps.wa.brick19812016[[385:444]]
#Aggregate monthly to annual automatically
#Annual for last 5 years time series
chirps.wa.brick19812016.5yrs.sum.indices<-rep(1:5,each=12)
chirps.wa.brick19812016.5yrs.annual<-stackApply(chirps.wa.brick19812016.5yrs, chirps.wa.brick19812016.5yrs.sum.indices, fun = sum)

#Annual for 37 years time series
chirps.wa.brick19812016.annual.sum.indices<-rep(1:37,each=12)
chirps.wa.brick19812016.annual.37<-stackApply(chirps.wa.brick19812016, chirps.wa.brick19812016.annual.sum.indices, fun = sum)
names(chirps.wa.brick19812016.annual.37)= as.character(seq(1981, 2017, by=1))
writeRaster(chirps.wa.brick19812016.annual.37, 'E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Climate/chirpswaannual-37.tif' )

##Compute long-term mean rainfall
chirps.wa.ltm = mean(chirps.wa.brick19812016.annual.37)
chirps.wa.ltm= mask(chirps.wa.ltm, countries.wa.shp)
writeRaster(chirps.wa.ltm, 'E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Climate/chirpswaltm37.tif' )

#***********************************************************************************************************************************************#
####Barplot of annual PPT per AEZ####
chirps.wa.brick19812016.annual.37.tc = projectRaster(chirps.wa.brick19812016.annual.37, aezWArst.tc)
names(chirps.wa.brick19812016.annual.37.tc)= as.character(seq(1981, 2017, by=1))
chirps.wa.brick19812016.annual.37.tc.zonal =zonal(chirps.wa.brick19812016.annual.37.tc, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/zonalchirpsannual.csv")
chirps.wa.brick19812016.annual.37.tc.zonal.df = as.data.frame(chirps.wa.brick19812016.annual.37.tc.zonal)
chirpswa.annual37.longDF =chirps.wa.brick19812016.annual.37.tc.zonal.df %>% gather(Year, Rainfall, X1981:X2017, na.rm = T)
chirpswa.annual37.longDF.avg = chirpswa.annual37.longDF %>%
                               group_by(zone, Year) %>%
                              summarise(Rainfall = mean(Rainfall))
chirpswa.annual37.longDF.avg.df=as.data.frame(chirpswa.annual37.longDF.avg)
chirpswa.annual37.longDF.avg.df <- within (chirpswa.annual37.longDF.avg.df, {
                                  zone <- as.factor(zone)
                                  })
                                    
####Process and plot line grapgh for mean temperature (Tmax+Tmin/2)

TcTmean = overlay(terraclimTmax.bk, terraclimTmin.bk, fun=function(x,y){return((x+y)/2)})
TcTmean.37<-stackApply(TcTmean, chirps.wa.brick19812016.annual.sum.indices, fun = mean)
names(TcTmean.37)= as.character(seq(1981, 2017, by=1))

TcTmean.annual.37.zonal =zonal(TcTmean.37, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/zonalchirpsannual.csv")
TcTmean.annual.37.zonal.df = as.data.frame(TcTmean.annual.37.zonal)
TcTmean.annual37.longDF =TcTmean.annual.37.zonal.df %>% gather(Year, Tmean, X1981:X2017, na.rm = T)
TmeanlongDF.avg = TcTmean.annual37.longDF %>%
  group_by(zone, Year) %>%
  summarise(Tmean = mean(Tmean))
TmeanlongDF.avg.df=as.data.frame(TmeanlongDF.avg)
TmeanlongDF.avg.df = within (TmeanlongDF.avg.df, {
  zone <- as.factor(zone)
})

 ####Plot bar with bar + line####

chirps.Tmean = cbind(chirpswa.annual37.longDF.avg.df,TmeanlongDF.avg.df )
chirps.Tmean = chirps.Tmean[,-c(4,5)]

#Bar
chirps.bp = ggplot(chirps.Tmean , aes(x = Year, y = Rainfall, fill= zone))+
  geom_bar(stat = "identity", position = position_dodge(width =0.9))+
  scale_x_discrete(labels=as.character(seq(1981, 2017, by=1)))+
  theme(axis.text.x= element_text(face = "bold", color = "black",size = 10, angle=90, hjust = 0, vjust = 0.5)) +
  theme(axis.text.y= element_text(face = "bold", color = "black",size = 10, angle=0, hjust = 0, vjust = 0.5)) +
  theme(axis.title.y= element_text(face = "bold", color = "black",size = 12))+
  theme(axis.title.x= element_text(face = "bold", color = "black",size = 12))+
  theme(legend.title = element_text(face = "bold", size = 12),legend.text = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0, 1800, by=200))
#ggsci::scale_fill_jco()
#ggsci::scale_fill_lancet()
#chirps.bp


#Line with groups
ggline = ggplot(chirps.Tmean, aes(x=Year, y=Tmean, group = zone, colour = zone)) +
  geom_line() +
  geom_point( size=4, shape=21)+
  ylab(expression("Mean Temperature ("~degree~"C)"))+
  theme(axis.text.x= element_text(face = "bold", color = "black",size = 10, angle=90, hjust = 1, vjust = 0.5))+
  theme(axis.text.y= element_text(face = "bold", color = "black",size = 10, angle=0, hjust = 0, vjust = 0.5))+
  theme(axis.title.y= element_text(face = "bold", color = "black",size = 12))+
  theme(axis.title.x= element_text(face = "bold", color = "black",size = 12))+
  theme(legend.title = element_text(face = "bold", size = 12),legend.text = element_text(size = 12))+
  scale_x_discrete(labels=as.character(seq(1981, 2017, by=1)))

ggarrange(chirps.bp, ggline, labels = c("a", "b"), ncol = 1, nrow = 2)

#***********************************************************************************************************************************************#
#Subset monthly time series
chirps.wa.jan<-subset(chirps.wa.brick19812016,seq(1, 444,12))
chirps.wa.feb<-subset(chirps.wa.brick19812016,seq(2, 444,12))
chirps.wa.mar<-subset(chirps.wa.brick19812016,seq(3, 444,12))
chirps.wa.apr<-subset(chirps.wa.brick19812016,seq(4, 444,12))
chirps.wa.may<-subset(chirps.wa.brick19812016,seq(5, 444,12))
chirps.wa.jun<-subset(chirps.wa.brick19812016,seq(6, 444,12))
chirps.wa.jul<-subset(chirps.wa.brick19812016,seq(7, 444,12))
chirps.wa.aug<-subset(chirps.wa.brick19812016,seq(8, 444,12))
chirps.wa.sep<-subset(chirps.wa.brick19812016,seq(9, 444,12))
chirps.wa.oct<-subset(chirps.wa.brick19812016,seq(10,444,12))
chirps.wa.nov<-subset(chirps.wa.brick19812016,seq(11,444,12))
chirps.wa.dec<-subset(chirps.wa.brick19812016,seq(12,444,12))
#Aggregate to monthly means

chirps.wa.monthly.mean.jan<-mean(subset(chirps.wa.brick19812016,(seq(1, 444,12))))
chirps.wa.monthly.mean.feb<-mean(subset(chirps.wa.brick19812016,(seq(2, 444,12))))
chirps.wa.monthly.mean.mar<-mean(subset(chirps.wa.brick19812016,(seq(3, 444,12))))
chirps.wa.monthly.mean.apr<-mean(subset(chirps.wa.brick19812016,(seq(4, 444,12))))
chirps.wa.monthly.mean.may<-mean(subset(chirps.wa.brick19812016,(seq(5, 444,12))))
chirps.wa.monthly.mean.jun<-mean(subset(chirps.wa.brick19812016,(seq(6, 444,12))))
chirps.wa.monthly.mean.jul<-mean(subset(chirps.wa.brick19812016,(seq(7, 444,12))))
chirps.wa.monthly.mean.aug<-mean(subset(chirps.wa.brick19812016,(seq(8, 444,12))))
chirps.wa.monthly.mean.sep<-mean(subset(chirps.wa.brick19812016,(seq(9, 444,12))))
chirps.wa.monthly.mean.oct<-mean(subset(chirps.wa.brick19812016,(seq(10,444,12))))
chirps.wa.monthly.mean.nov<-mean(subset(chirps.wa.brick19812016,(seq(11,444,12))))
chirps.wa.monthly.mean.dec<-mean(subset(chirps.wa.brick19812016,(seq(12,444,12))))


#*********************************************************************************************************************************************#
####Stack Monthly mean Rainfall rasters (12)####
chirps.monthlyMean=brick(chirps.wa.monthly.mean.jan,chirps.wa.monthly.mean.feb,chirps.wa.monthly.mean.mar,chirps.wa.monthly.mean.apr,chirps.wa.monthly.mean.may,
                         chirps.wa.monthly.mean.jun,chirps.wa.monthly.mean.jul,chirps.wa.monthly.mean.aug,chirps.wa.monthly.mean.sep,chirps.wa.monthly.mean.oct,
                         chirps.wa.monthly.mean.nov,chirps.wa.monthly.mean.dec)
names(chirps.monthlyMean)=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
writeRaster(chirps.monthlyMean, format='GTiff', 'chirps.monthlyMean.tif')
chirps.monthlyMean = brick('chirps.monthlyMean.tif')

####barplot for monthly mean rainfal climatology per AEZ in WA####

chirps.monthlyMean.tc = projectRaster(chirps.monthlyMean, aezWArst.tc)
names(chirps.monthlyMean.tc)=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

chirps.MonMean.zonal =as.data.frame(zonal(chirps.monthlyMean.tc, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE))
chirps.MonMean.zonal.longDF =chirps.MonMean.zonal %>% gather(Month, Rainfall, Jan:Dec, na.rm = T)
chirps.MonMean.zonal.longDF <- within (chirps.MonMean.zonal.longDF,{zone <- as.factor(zone)})
chirps.MonMean.zonal.longDF$Month <- factor(chirps.MonMean.zonal.longDF$Month,levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
####Plot bar with barplot for monthly rainfal####

chirps.bp = ggplot(chirps.MonMean.zonal.longDF , aes(x = Month, y = Rainfall, fill=zone))+
  geom_bar(stat = "identity", position = position_dodge(width =1))+
  #scale_x_discrete(labels=as.character(seq(1981, 2017, by=1)))+
  theme(axis.text.x= element_text(face = "bold", color = "black",size = 10, angle=0, hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.y= element_text(face = "bold", color = "black",size = 10, angle=0, hjust = 0.5, vjust = 0.5)) +
  theme(axis.title.y= element_text(face = "bold", color = "black",size = 12))+
  theme(axis.title.x= element_text(face = "bold", color = "black",size = 12, hjust = 0.5, vjust = 0.5))+
  theme(legend.title = element_text(face = "bold", size = 12),legend.text = element_text(size = 12))+
  labs(y='Rainfall (mm)')+
  scale_y_continuous(breaks = seq(0, 270, by=15))
chirps.bp

#*********************************************************************************************************************************************#
####Line plot for Mean Temperature climatology in WA####
TcTmean = overlay(terraclimTmax.bk, terraclimTmin.bk, fun=function(x,y){return((x+y)/2)})
TcTmean.37<-stackApply(TcTmean, chirps.wa.brick19812016.annual.sum.indices, fun = mean)
names(TcTmean.37)= as.character(seq(1981, 2017, by=1))

TcTmean.annual.37.zonal =zonal(TcTmean.37, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/zonalchirpsannual.csv")
TcTmean.annual.37.zonal.df = as.data.frame(TcTmean.annual.37.zonal)
TcTmean.annual37.longDF =TcTmean.annual.37.zonal.df %>% gather(Year, Tmean, X1981:X2017, na.rm = T)
TmeanlongDF.avg = TcTmean.annual37.longDF %>%
  group_by(zone, Year) %>%
  summarise(Tmean = mean(Tmean))
TmeanlongDF.avg.df=as.data.frame(TmeanlongDF.avg)
TmeanlongDF.avg.df = within (TmeanlongDF.avg.df, {
  zone <- as.factor(zone)
})

#*********************************************************************************************************************************************#
#Aggregate to monthly cv
##*******************************************************************************************************************************************
chirps.wa.monthly.cv.jan<-cv(subset(chirps.wa.brick19812016.5yrs,seq(1, 444,12)))
chirps.wa.monthly.cv.feb<-cv(subset(chirps.wa.brick19812016.5yrs,seq(2, 444,12)))
chirps.wa.monthly.cv.mar<-cv(subset(chirps.wa.brick19812016.5yrs,seq(3, 444,12)))
chirps.wa.monthly.cv.apr<-cv(subset(chirps.wa.brick19812016.5yrs,seq(4, 444,12)))
chirps.wa.monthly.cv.may<-cv(subset(chirps.wa.brick19812016.5yrs,seq(5, 444,12)))
chirps.wa.monthly.cv.jun<-cv(subset(chirps.wa.brick19812016.5yrs,seq(6, 444,12)))
chirps.wa.monthly.cv.jul<-cv(subset(chirps.wa.brick19812016.5yrs,seq(7, 444,12)))
chirps.wa.monthly.cv.aug<-cv(subset(chirps.wa.brick19812016.5yrs,seq(8, 444,12)))
chirps.wa.monthly.cv.sep<-cv(subset(chirps.wa.brick19812016.5yrs,seq(9, 444,12)))
chirps.wa.monthly.cv.oct<-cv(subset(chirps.wa.brick19812016.5yrs,seq(10,444,12)))
chirps.wa.monthly.cv.nov<-cv(subset(chirps.wa.brick19812016.5yrs,seq(11,444,12)))
chirps.wa.monthly.cv.dec<-cv(subset(chirps.wa.brick19812016.5yrs,seq(12,444,12)))

#Calculate annual mean over entire 37 year time series

chirps.wa.ltm.5yrs <- calc(chirps.wa.brick19812016.5yrs.annual, mean)
#chirps.wa.ltm.5yrs <-mask(chirps.wa.ltm,countries.wa.shp)
chirps.cv.std.5yrs <- calc(chirps.wa.brick19812016.5yrs.annual, cv)

##############################################################################
#Process Process Topographical layers - SRTM DEM + slope
##############################################################################
####
#demsre3a.wa<-raster('E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Topography/DEMSRE3aWA_1km.tif')
#demsre3a.wa[demsre3a.wa < 0] <- NA
#names(demsre3a.wa)= 'DEM'
#slope.wa = raster('E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Topography/SLPSRTE3aWA_1km.tif')
terraclimDEM.wa = raster('E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/input/Topography/terraclimDEMwa.tif')

##############################################################################
#Process Terraclimate Tmax data
##############################################################################
## Set Output directory
outdir.terraclimTmax.monthly.tmax<- ('outdir.terraclimTmax.monthly.nc/tmax/')
#Set input directory for chirps rasters
inputdir.terraclimTmax.monthly <- ('H:/TerraClimate/Tmax')

## list rasters in input directory
files.terraclimTmax.monthly.nc <- list.files(inputdir.terraclimTmax.monthly, pattern='*.nc', full.names = T, recursive = T)
#files.terraclimTmax.monthly.nc.5yrs=files.terraclimTmax.monthly.nc[33:37]
#Import raster names
## Loop to import rasters, crop and stack the img files

for (i in 1:length(files.terraclimTmax.monthly.nc)){
  terraclimTmax.monthly.nc <-brick(files.terraclimTmax.monthly.nc[i], varname="tmax")
  terraclimTmax.monthly.nc.clip<-projectRaster(terraclimTmax.monthly.nc, terraclimDEM.wa)
  terraclimTmax.monthly.nc.crop<-crop(terraclimTmax.monthly.nc.clip, terraclimDEM.wa)
  terraclimTmax.monthly.nc.crop<-mask(terraclimTmax.monthly.nc.crop,terraclimDEM.wa)
  writeRaster(terraclimTmax.monthly.nc.crop, paste0(outdir.terraclimTmax.monthly.tmax, i, '.tif', sep = ''), format = 'GTiff', bylayer=T, suffix='names', overwrite =TRUE)
}

list.terraclimTmax.grd<-list.files(outdir.terraclimTmax.monthly.tmax, pattern='*.tif', full.names = T, recursive = T)
list.terraclimTmax.grd<-mixedsort(list.terraclimTmax.grd)
terraclimTmax<-stack(list.terraclimTmax.grd)
terraclimTmax.bk<-brick(terraclimTmax)
writeRaster(terraclimTmax.bk, 'terraclimTmaxbk', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmax.bk<-brick('terraclimTmaxbk.tif')

#tcTmaxwa.stack=brick('terraclimTmax.wa.tif')
#names(terraclimTmax.wa.monthly.mean.stack)=c('TmaxJan','TmaxFeb','TmaxMar','TmaxApr','TmaxMay','TmaxJun','TmaxJul','TmaxAug','TmaxSep','TmaxOct','TmaxNov','TmaxDec')
levelplot(terraclimTmax[[1]])+
  layer(sp.polygons(countries.wa.shp, border='red'))

###***********************************************************************
#Aggregate monthly to annual automatically

#Separate monthly 

tcTmaxwa.jan<-subset(terraclimTmax.bk,(seq(1, 444,12)))
tcTmaxwa.feb<-subset(terraclimTmax.bk,(seq(2, 444,12)))
tcTmaxwa.mar<-subset(terraclimTmax.bk,(seq(3, 444,12)))
tcTmaxwa.apr<-subset(terraclimTmax.bk,(seq(4, 444,12)))
tcTmaxwa.may<-subset(terraclimTmax.bk,(seq(5, 444,12)))
tcTmaxwa.jun<-subset(terraclimTmax.bk,(seq(6, 444,12)))
tcTmaxwa.jul<-subset(terraclimTmax.bk,(seq(7, 444,12)))
tcTmaxwa.aug<-subset(terraclimTmax.bk,(seq(8, 444,12)))
tcTmaxwa.sep<-subset(terraclimTmax.bk,(seq(9, 444,12)))
tcTmaxwa.oct<-subset(terraclimTmax.bk,(seq(10,444,12)))
tcTmaxwa.nov<-subset(terraclimTmax.bk,(seq(11,444,12)))
tcTmaxwa.dec<-subset(terraclimTmax.bk,(seq(12,444,12)))

#Aggregate to monthly means

terraclimTmax.wa.monthly.mean.jan<-mean(subset(terraclimTmax.bk,(seq(1, 444,12))))
terraclimTmax.wa.monthly.mean.feb<-mean(subset(terraclimTmax.bk,(seq(2, 444,12))))
terraclimTmax.wa.monthly.mean.mar<-mean(subset(terraclimTmax.bk,(seq(3, 444,12))))
terraclimTmax.wa.monthly.mean.apr<-mean(subset(terraclimTmax.bk,(seq(4, 444,12))))
terraclimTmax.wa.monthly.mean.may<-mean(subset(terraclimTmax.bk,(seq(5, 444,12))))
terraclimTmax.wa.monthly.mean.jun<-mean(subset(terraclimTmax.bk,(seq(6, 444,12))))
terraclimTmax.wa.monthly.mean.jul<-mean(subset(terraclimTmax.bk,(seq(7, 444,12))))
terraclimTmax.wa.monthly.mean.aug<-mean(subset(terraclimTmax.bk,(seq(8, 444,12))))
terraclimTmax.wa.monthly.mean.sep<-mean(subset(terraclimTmax.bk,(seq(9, 444,12))))
terraclimTmax.wa.monthly.mean.oct<-mean(subset(terraclimTmax.bk,(seq(10,444,12))))
terraclimTmax.wa.monthly.mean.nov<-mean(subset(terraclimTmax.bk,(seq(11,444,12))))
terraclimTmax.wa.monthly.mean.dec<-mean(subset(terraclimTmax.bk,(seq(12,444,12))))
terraclimTmax.wa.monthly.mean.stack=stack(terraclimTmax.wa.monthly.mean.apr,terraclimTmax.wa.monthly.mean.may,terraclimTmax.wa.monthly.mean.jun,terraclimTmax.wa.monthly.mean.jul,
                                          terraclimTmax.wa.monthly.mean.aug,terraclimTmax.wa.monthly.mean.sep,terraclimTmax.wa.monthly.mean.oct,terraclimTmax.wa.monthly.mean.nov, 
                                          terraclimTmax.wa.monthly.mean.dec,terraclimTmax.wa.monthly.mean.jan,terraclimTmax.wa.monthly.mean.feb,terraclimTmax.wa.monthly.mean.mar)
names(terraclimTmax.wa.monthly.mean.stack)=c('TmaxApr','TmaxMay','TmaxJun','TmaxJul','TmaxAug','TmaxSep','TmaxOct','TmaxNov','TmaxDec','TmaxJan','TmaxFeb','TmaxMar')

writeRaster(terraclimTmax.wa.monthly.mean.stack, 'terraclimTmax.wa.mean', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmax.wa.monthly.mean.stack=brick('terraclimTmax.wa.tif')
names(terraclimTmax.wa.monthly.mean.stack)=c('TmaxJan','TmaxFeb','TmaxMar','TmaxApr','TmaxMay','TmaxJun','TmaxJul','TmaxAug','TmaxSep','TmaxOct','TmaxNov','TmaxDec')
levelplot(terraclimTmax.wa.monthly.mean.stack, par.settings=myTheme.ltm.blues)+  
layer(sp.polygons(countries.wa.shp, border='black'))


rm(terraclimTmax.wa.monthly.mean.jan,terraclimTmax.wa.monthly.mean.feb, terraclimTmax.wa.monthly.mean.mar,
         terraclimTmax.wa.monthly.mean.apr,terraclimTmax.wa.monthly.mean.may, terraclimTmax.wa.monthly.mean.jun, 
         terraclimTmax.wa.monthly.mean.jul, terraclimTmax.wa.monthly.mean.aug, terraclimTmax.wa.monthly.mean.sep,
         terraclimTmax.wa.monthly.mean.oct,terraclimTmax.wa.monthly.mean.nov, terraclimTmax.wa.monthly.mean.dec)


#Aggregate to monthly cv

terraclimTmax.cv.jan<-sd(subset(terraclimTmax,(seq(1, 444,12))))
terraclimTmax.cv.feb<-cv(subset(terraclimTmax,(seq(2, 444,12))))
terraclimTmax.cv.mar<-cv(subset(terraclimTmax,(seq(3, 444,12))))
terraclimTmax.cv.apr<-cv(subset(terraclimTmax,(seq(4, 444,12))))
terraclimTmax.cv.may<-cv(subset(terraclimTmax,(seq(5, 444,12))))
terraclimTmax.cv.jun<-cv(subset(terraclimTmax,(seq(6, 444,12))))
terraclimTmax.cv.jul<-cv(subset(terraclimTmax,(seq(7, 444,12))))
terraclimTmax.cv.aug<-cv(subset(terraclimTmax,(seq(8, 444,12))))
terraclimTmax.cv.sep<-cv(subset(terraclimTmax,(seq(9, 444,12))))
terraclimTmax.cv.oct<-cv(subset(terraclimTmax,(seq(10,444,12))))
terraclimTmax.cv.nov<-cv(subset(terraclimTmax,(seq(11,444,12))))
terraclimTmax.cv.dec<-cv(subset(terraclimTmax,(seq(12,444,12))))
terraclimTmax.wa.monthly.cv.stack=stack(terraclimTmax.cv.jan,terraclimTmax.cv.feb,terraclimTmax.cv.mar, terraclimTmax.cv.apr,terraclimTmax.cv.may,terraclimTmax.cv.jun,
                                          terraclimTmax.cv.jul,terraclimTmax.cv.aug,terraclimTmax.cv.sep,terraclimTmax.cv.oct,terraclimTmax.cv.nov, terraclimTmax.cv.dec)
names(terraclimTmax.wa.monthly.cv.stack)=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

writeRaster(terraclimTmax.wa.monthly.cv.stack, 'terraclimTmax.wacv.tif', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmax.wa.monthly.cv = stack('terraclimTmax.wacv.tif')
names(terraclimTmax.wa.monthly.cv)=c('TmaxCVJan','TmaxCVFeb','TmaxCVMar','TmaxCVApr','TmaxCVMay','TmaxCVJun','TmaxCVJul','TmaxCVAug','TmaxCVSep','TmaxCVOct','TmaxCVNov','TmaxCVDec')

rm(terraclimTmax.cv.jan,terraclimTmax.cv.feb,terraclimTmax.cv.mar,terraclimTmax.cv.apr,
          terraclimTmax.cv.may,terraclimTmax.cv.jun,terraclimTmax.cv.jul,terraclimTmax.cv.aug,
          terraclimTmax.cv.sep,terraclimTmax.cv.oct,terraclimTmax.cv.nov,terraclimTmax.cv.dec)
rm(terraclimTmax.wa.monthly.cv.stack)
#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################################################################
#Process Terraclimate Tmin data
##############################################################################
## Set Output directory
outdir.terraclimTmin.monthly.tmin<- ('outdir.terraclimTmax.monthly.nc/tmin/')
#Set input directory for chirps rasters
inputdir.terraclimTmin.monthly.Tmin <- ('H:/TerraClimate/Tmin')

## list rasters in input directory
files.terraclimTmin.monthly.nc <- list.files(inputdir.terraclimTmin.monthly.Tmin, pattern='*.nc', full.names = T, recursive = T)
#files.terraclimTmin.monthly.nc.5yrs=files.terraclimTmin.monthly.nc[33:37]
#Import raster names
## Loop to import rasters, crop and stack the img files

for (i in 1:length(files.terraclimTmin.monthly.nc)){
  terraclimTmin.monthly.nc <-brick(files.terraclimTmin.monthly.nc[i], varname="tmin")
  terraclimTmin.monthly.nc.clip<-projectRaster(terraclimTmin.monthly.nc, terraclimDEM.wa)
  terraclimTmin.monthly.nc.crop<-crop(terraclimTmin.monthly.nc.clip, terraclimDEM.wa)
  terraclimTmin.monthly.nc.crop<-mask(terraclimTmin.monthly.nc.crop,terraclimDEM.wa)
  writeRaster(terraclimTmin.monthly.nc.crop, paste0(outdir.terraclimTmin.monthly.tmin, i, '.tif', sep = ''), format = 'GTiff', bylayer=T, suffix='names', overwrite =TRUE)
}

list.terraclimTmin.grd<-list.files(outdir.terraclimTmin.monthly.tmin, pattern='*.tif', full.names = T, recursive = T)
list.terraclimTmin.grd<-mixedsort(list.terraclimTmin.grd)
terraclimTmin<-stack(list.terraclimTmin.grd)

terraclimTmin.bk<-brick(terraclimTmin)
writeRaster(terraclimTmin.bk, 'terraclimTminbk', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmin.bk<-brick('terraclimTminbk.tif')

levelplot(terraclimTmin[[6]])+
  layer(sp.polygons(countries.wa.shp, border='black'))

#Aggregate monthly to annual automatically
terraclim.monToannual.indices<-rep(1:37,each=12)
terraclimTmin.annual.5yrs<-stackApply(terraclimTmin, chirps.wa.brick19812016.5yrs.sum.indices, fun = mean)

#Aggregate to monthly means

terraclimTmin.wa.monthly.mean.jan<-mean(subset(terraclimTmin,(seq(1, 444,12))))
terraclimTmin.wa.monthly.mean.feb<-mean(subset(terraclimTmin,(seq(2, 444,12))))
terraclimTmin.wa.monthly.mean.mar<-mean(subset(terraclimTmin,(seq(3, 444,12))))
terraclimTmin.wa.monthly.mean.apr<-mean(subset(terraclimTmin,(seq(4, 444,12))))
terraclimTmin.wa.monthly.mean.may<-mean(subset(terraclimTmin,(seq(5, 444,12))))
terraclimTmin.wa.monthly.mean.jun<-mean(subset(terraclimTmin,(seq(6, 444,12))))
terraclimTmin.wa.monthly.mean.jul<-mean(subset(terraclimTmin,(seq(7, 444,12))))
terraclimTmin.wa.monthly.mean.aug<-mean(subset(terraclimTmin,(seq(8, 444,12))))
terraclimTmin.wa.monthly.mean.sep<-mean(subset(terraclimTmin,(seq(9, 444,12))))
terraclimTmin.wa.monthly.mean.oct<-mean(subset(terraclimTmin,(seq(10,444,12))))
terraclimTmin.wa.monthly.mean.nov<-mean(subset(terraclimTmin,(seq(11,444,12))))
terraclimTmin.wa.monthly.mean.dec<-mean(subset(terraclimTmin,(seq(12,444,12))))
terraclimTmin.wa.monthly.mean.stack=stack(terraclimTmin.wa.monthly.mean.jan,terraclimTmin.wa.monthly.mean.feb,terraclimTmin.wa.monthly.mean.mar, terraclimTmin.wa.monthly.mean.apr,terraclimTmin.wa.monthly.mean.may,terraclimTmin.wa.monthly.mean.jun,
                                          terraclimTmin.wa.monthly.mean.jul,terraclimTmin.wa.monthly.mean.aug,terraclimTmin.wa.monthly.mean.sep,terraclimTmin.wa.monthly.mean.oct,terraclimTmin.wa.monthly.mean.nov, terraclimTmin.wa.monthly.mean.dec)
names(terraclimTmin.wa.monthly.mean.stack)=c('TminJan','TminFeb','TminMar','TminApr','TminMay','TminJun','TminJul','TminAug','TminSep','TminOct','TminNov','TminDec')


writeRaster(terraclimTmin.wa.monthly.mean.stack, 'terraclimTmin.wa.tif', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmin.wa.monthly.mean.stack =brick('terraclimTmin.wa.tif')
names(terraclimTmin.wa.monthly.mean.stack)=c('TminJan','TminFeb','TminMar','TminApr','TminMay','TminJun','TminJul','TminAug','TminSep','TminOct','TminNov','TminDec')

rm(terraclimTmin.wa.monthly.mean.jan,terraclimTmin.wa.monthly.mean.feb,terraclimTmin.wa.monthly.mean.mar, terraclimTmin.wa.monthly.mean.apr,terraclimTmin.wa.monthly.mean.may,terraclimTmin.wa.monthly.mean.jun,
   terraclimTmin.wa.monthly.mean.jul,terraclimTmin.wa.monthly.mean.aug,terraclimTmin.wa.monthly.mean.sep,terraclimTmin.wa.monthly.mean.oct,terraclimTmin.wa.monthly.mean.nov, terraclimTmin.wa.monthly.mean.dec)
#rm(terraclimTmin.wa.monthly.mean.stack)
#Aggregate to monthly cv for Tmin

terraclimTmin.cv.jan<-cv(subset(terraclimTmin,(seq(1, 444,12))))
terraclimTmin.cv.feb<-cv(subset(terraclimTmin,(seq(2, 444,12))))
terraclimTmin.cv.mar<-cv(subset(terraclimTmin,(seq(3, 444,12))))
terraclimTmin.cv.apr<-cv(subset(terraclimTmin,(seq(4, 444,12))))
terraclimTmin.cv.may<-cv(subset(terraclimTmin,(seq(5, 444,12))))
terraclimTmin.cv.jun<-cv(subset(terraclimTmin,(seq(6, 444,12))))
terraclimTmin.cv.jul<-cv(subset(terraclimTmin,(seq(7, 444,12))))
terraclimTmin.cv.aug<-cv(subset(terraclimTmin,(seq(8, 444,12))))
terraclimTmin.cv.sep<-cv(subset(terraclimTmin,(seq(9, 444,12))))
terraclimTmin.cv.oct<-cv(subset(terraclimTmin,(seq(10,444,12))))
terraclimTmin.cv.nov<-cv(subset(terraclimTmin,(seq(11,444,12))))
terraclimTmin.cv.dec<-cv(subset(terraclimTmin,(seq(12,444,12))))
terraclimTmin.wa.monthly.cv.stack=stack(terraclimTmin.wa.monthly.cv.jan,terraclimTmin.wa.monthly.cv.feb,terraclimTmin.wa.monthly.cv.mar, terraclimTmin.wa.monthly.cv.apr,terraclimTmin.wa.monthly.cv.may,terraclimTmin.wa.monthly.cv.jun,
                                        terraclimTmin.wa.monthly.cv.jul,terraclimTmin.wa.monthly.cv.aug,terraclimTmin.wa.monthly.cv.sep,terraclimTmin.wa.monthly.cv.oct,terraclimTmin.wa.monthly.cv.nov, terraclimTmin.wa.monthly.cv.dec)

names(terraclimTmin.wa.monthly.cv.stack)=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

writeRaster(terraclimTmin.wa.monthly.cv.stack, 'terraclimTmin.wacv.tif', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimTmin.wa.monthly.cv = stack('terraclimTmin.wacv.tif')
names(terraclimTmin.wa.monthly.cv)=c('TminCVJan','TminCVFeb','TminCVMar','TminCVApr','TminCVMay','TminCVJun','TminCVJul','TminCVAug','TminCVSep','TminCVOct','TminCVNov','TminCVDec')

rm(terraclimTmin.cv.jan,terraclimTmin.cv.feb,terraclimTmin.cv.mar,terraclimTmin.cv.apr,
   terraclimTmin.cv.may,terraclimTmin.cv.jun,terraclimTmin.cv.jul,terraclimTmin.cv.aug,
   terraclimTmin.cv.sep,terraclimTmin.cv.oct,terraclimTmin.cv.nov,terraclimTmin.cv.dec)
#rm(terraclimTmin.wa.monthly.cv.stack)

############################################################################################################
###Extract monthly rainfall

#Extract monthly time series to monthly means

tcTminwa.jan<-subset(terraclimTmin.bk,(seq(1, 444,12)))
tcTminwa.feb<-subset(terraclimTmin.bk,(seq(2, 444,12)))
tcTminwa.mar<-subset(terraclimTmin.bk,(seq(3, 444,12)))
tcTminwa.apr<-subset(terraclimTmin.bk,(seq(4, 444,12)))
tcTminwa.may<-subset(terraclimTmin.bk,(seq(5, 444,12)))
tcTminwa.jun<-subset(terraclimTmin.bk,(seq(6, 444,12)))
tcTminwa.jul<-subset(terraclimTmin.bk,(seq(7, 444,12)))
tcTminwa.aug<-subset(terraclimTmin.bk,(seq(8, 444,12)))
tcTminwa.sep<-subset(terraclimTmin.bk,(seq(9, 444,12)))
tcTminwa.oct<-subset(terraclimTmin.bk,(seq(10,444,12)))
tcTminwa.nov<-subset(terraclimTmin.bk,(seq(11,444,12)))
tcTminwa.dec<-subset(terraclimTmin.bk,(seq(12,444,12)))
#tcTminwa.monthly.stack=stack(tcTminwa.jan,tcTminwa.feb,tcTminwa.mar, tcTminwa.apr,tcTminwa.may,tcTminwa.jun,tcTminwa.jul,tcTminwa.aug,tcTminwa.sep,tcTminwa.oct,tcTminwa.nov, tcTminwa.dec)
#names(terraclimTmin.wa.monthly.mean.stack)=c('TminJan','TminFeb','TminMar','TminApr','TminMay','TminJun','TminJul','TminAug','TminSep','TminOct','TminNov','TminDec')


#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################################################################
#Process Terraclimate PPT data
##############################################################################
## Set Output directory
outdir.terraclimppt.monthly<- ('outdir.terraclimTmax.monthly.nc/ppt/')
#Set input directory for chirps rasters
inputdir.terraclimppt.monthly <- ('H:/TerraClimate/ppt')

## list rasters in input directory
files.terraclimppt.monthly.nc <- list.files(inputdir.terraclimppt.monthly, pattern='*.nc', full.names = T, recursive = T)
#files.terraclimppt.monthly.nc.5yrs<-mixedsort(files.terraclimppt.monthly.nc.5yrs)
#files.terraclimppt.monthly.nc.5yrs=files.terraclimppt.monthly.nc[3:7]
#Import raster names
## Loop to import rasters, crop and stack the img files

for (i in 1:length(files.terraclimppt.monthly.nc)){
  terraclimppt.monthly.nc <-brick(files.terraclimppt.monthly.nc[i], varname="ppt")
  terraclimppt.monthly.nc.clip<-projectRaster(terraclimppt.monthly.nc, terraclimDEM.wa)
  terraclimppt.monthly.nc.crop<-crop(terraclimppt.monthly.nc.clip, terraclimDEM.wa)
  terraclimppt.monthly.nc.crop<-mask(terraclimppt.monthly.nc.crop,terraclimDEM.wa)
  writeRaster(terraclimppt.monthly.nc.crop, paste0(outdir.terraclimppt.monthly, i, '.tif', sep = ''), format = 'GTiff', bylayer=T, suffix='names', overwrite =TRUE)
}

list.terraclimppt.grd<-list.files(outdir.terraclimppt.monthly, pattern='*.tif', full.names = T, recursive = T)
list.terraclimppt.grd<-mixedsort(list.terraclimppt.grd)
terraclimppt<-stack(list.terraclimppt.grd)

terraclimppt.bk<-brick(terraclimppt)
writeRaster(terraclimppt.bk, 'terraclimpptbk', format = 'GTiff', bylayer=F, overwrite =TRUE)
terraclimppt.bk<-brick('terraclimpptbk.tif')
terraclimppt.bk[terraclimppt.bk==0] <- NA

levelplot(terraclimppt[[1]])+
  layer(sp.polygons(countries.wa.shp, border='black'))

#Aggregate monthly to annual automatically
terraclim.monToannual.indices<-rep(1:37,each=12)

terraclimppt.annual.37yrs<-stackApply(terraclimppt, terraclim.monToannual.indices, fun = sum)
writeRaster(terraclimppt.annual.37yrs, 'terraclimpptwa.annual37yrs', format='GTiff', overwrite=T)
rm('terraclimppt.annual.37yrs')
terraclimppt.annual.37yrs = stack('terraclimpptwa.annual37yrs.tif')
terraclimppt.annual.37yrs= mask(terraclimppt.annual.37yrs, countries.wa.shp)
#names(terraclimppt.annual.5yrs)= c('2013','2014','2015','2016','2017')
names(terraclimppt.annual.37yrs)= paste0('ppt', as.character(rep(1981:2017,1)))
levelplot(terraclimppt.annual.37yrs)

######Separate monthly PPT time series

tcpptwa.jan<-subset(terraclimppt.bk,(seq(1, 444,12)))
tcpptwa.feb<-subset(terraclimppt.bk,(seq(2, 444,12)))
tcpptwa.mar<-subset(terraclimppt.bk,(seq(3, 444,12)))
tcpptwa.apr<-subset(terraclimppt.bk,(seq(4, 444,12)))
tcpptwa.may<-subset(terraclimppt.bk,(seq(5, 444,12)))
tcpptwa.jun<-subset(terraclimppt.bk,(seq(6, 444,12)))
tcpptwa.jul<-subset(terraclimppt.bk,(seq(7, 444,12)))
tcpptwa.aug<-subset(terraclimppt.bk,(seq(8, 444,12)))
tcpptwa.sep<-subset(terraclimppt.bk,(seq(9, 444,12)))
tcpptwa.oct<-subset(terraclimppt.bk,(seq(10,444,12)))
tcpptwa.nov<-subset(terraclimppt.bk,(seq(11,444,12)))
tcpptwa.dec<-subset(terraclimppt.bk,(seq(12,444,12)))

#Aggregate monthly means

terraclimppt.wa.monthly.mean.jan<-mean(subset(terraclimppt,(seq(1, 444,12))))
terraclimppt.wa.monthly.mean.feb<-mean(subset(terraclimppt,(seq(2, 444,12))))
terraclimppt.wa.monthly.mean.mar<-mean(subset(terraclimppt,(seq(3, 444,12))))
terraclimppt.wa.monthly.mean.apr<-mean(subset(terraclimppt,(seq(4, 444,12))))
terraclimppt.wa.monthly.mean.may<-mean(subset(terraclimppt,(seq(5, 444,12))))
terraclimppt.wa.monthly.mean.jun<-mean(subset(terraclimppt,(seq(6, 444,12))))
terraclimppt.wa.monthly.mean.jul<-mean(subset(terraclimppt,(seq(7, 444,12))))
terraclimppt.wa.monthly.mean.aug<-mean(subset(terraclimppt,(seq(8, 444,12))))
terraclimppt.wa.monthly.mean.sep<-mean(subset(terraclimppt,(seq(9, 444,12))))
terraclimppt.wa.monthly.mean.oct<-mean(subset(terraclimppt,(seq(10,444,12))))
terraclimppt.wa.monthly.mean.nov<-mean(subset(terraclimppt,(seq(11,444,12))))
terraclimppt.wa.monthly.mean.dec<-mean(subset(terraclimppt,(seq(12,444,12))))
terraclimppt.wa.monthly.mean.stack=stack(terraclimppt.wa.monthly.mean.jan,terraclimppt.wa.monthly.mean.feb,terraclimppt.wa.monthly.mean.mar, terraclimppt.wa.monthly.mean.apr,terraclimppt.wa.monthly.mean.may,terraclimppt.wa.monthly.mean.jun,
                                          terraclimppt.wa.monthly.mean.jul,terraclimppt.wa.monthly.mean.aug,terraclimppt.wa.monthly.mean.sep,terraclimppt.wa.monthly.mean.oct,terraclimppt.wa.monthly.mean.nov, terraclimppt.wa.monthly.mean.dec)
names(terraclimppt.wa.monthly.mean.stack)=c('pptJan','pptFeb','pptMar','pptApr','pptMay','pptJun','pptJul','pptAug','pptSep','pptOct','pptNov','pptDec')
#writeRaster(terraclimppt.wa.monthly.mean.stack, format = 'GTiff', bylayer=F, overwrite =TRUE)
#Aggregate to monthly cv for Terraclim ppt

terraclimppt.cv.jan<-cv(subset(terraclimppt,(seq(1, 444,12))))
terraclimppt.cv.feb<-cv(subset(terraclimppt,(seq(2, 444,12))))
terraclimppt.cv.mar<-cv(subset(terraclimppt,(seq(3, 444,12))))
terraclimppt.cv.apr<-cv(subset(terraclimppt,(seq(4, 444,12))))
terraclimppt.cv.may<-cv(subset(terraclimppt,(seq(5, 444,12))))
terraclimppt.cv.jun<-cv(subset(terraclimppt,(seq(6, 444,12))))
terraclimppt.cv.jul<-cv(subset(terraclimppt,(seq(7, 444,12))))
terraclimppt.cv.aug<-cv(subset(terraclimppt,(seq(8, 444,12))))
terraclimppt.cv.sep<-cv(subset(terraclimppt,(seq(9, 444,12))))
terraclimppt.cv.oct<-cv(subset(terraclimppt,(seq(10,444,12))))
terraclimppt.cv.nov<-cv(subset(terraclimppt,(seq(11,444,12))))
terraclimppt.cv.dec<-cv(subset(terraclimppt,(seq(12,444,12))))
terraclimppt.wa.monthly.cv.stack=stack(terraclimppt.cv.jan,terraclimppt.cv.feb,terraclimppt.cv.mar, terraclimppt.cv.apr,terraclimppt.cv.may,terraclimppt.cv.jun,
                                        terraclimppt.cv.jul,terraclimppt.cv.aug,terraclimppt.cv.sep,terraclimppt.cv.oct,terraclimppt.cv.nov, terraclimppt.cv.dec)
names(terraclimppt.wa.monthly.cv.stack)=c("pptJanCV","pptFebCV","pptMarCV","pptAprCV","pptMayCV","pptJunCV","pptJulCV","pptAugCV","pptSepCV","pptOctCV","pptNovCV","pptDecCV" )
#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#stack climatic layers [PPT, Tmin, Tmin]

climate.wa=stack(terraclimTmin.wa.monthly.mean.stack, terraclimTmin.wa.monthly.mean.stack,terraclimppt.wa.monthly.mean.stack, terraclimppt.annual.5yrs)
writeRaster(climate.wa, 'climate.wa.tif', format='GTiff', overwrite=T)
rm(climate.wa)
climate.wa=stack('climate.wa.tif')
names(climate.wa)=c("TmaxJan", "TmaxFeb","TmaxMar", "TmaxApr", "TmaxMay", "TmaxJun", "TmaxJul", "TmaxAug","TmaxSep" ,
                    "TmaxOct", "TmaxNov","TmaxDec", "TminJan", "TminFeb", "TminMar", "TminApr", "TminMay","TminJun" ,
                    "TminJul", "TminAug","TminSep", "TminOct", "TminNov", "TminDec", "pptJan" , "pptFeb" ,"pptMar"  ,
                    "pptApr" , "pptMay" ,"pptJun" , "pptJul" , "pptAug" , "pptSep" , "pptOct" , "pptNov" ,"pptDec")

#stack climate raw 444 layers[Tmax,Tmin,ppt, DEM, AET]

climate.wa.444=stack(terraclimTmin, terraclimTmax,terraclimppt, terraclimDEM.wa)
writeRaster(climate.wa.444, 'climate.wa.444.tif', format='GTiff', overwrite=T)
rm(climate.wa.444)
climate.wa.444=stack('climate.wa.444.tif')
#climate.wa.444=stack('climate.wa.tif')

#******************************************************************************************************
##Create RasterVis Themes (palletes)
#******************************************************************************************************
# Set color palette
zeroCol <-"#B3B3B3" #
beige<-grDevices::colors(16)
reds.rev <- rev(brewer.pal('YlOrRd', n = 9))
reds <- brewer.pal('YlOrRd', n = 9)
blues <- brewer.pal('Blues', n = 9)
yelowGnBlueYl<-brewer.pal('YlGnBu', n=9)
grdevices.matlablike<-rev(grDevices::topo.colors(20))
matlablike2<-matlab.like2(12)
Gn2Rd<-colorRamps::green2red(100)
#c(reds.rev,zeroCol, blues))
zeroCol <-"#B3B3B3" # 
#c(reds.rev,zeroCol, blues))
myTheme.ltm.zero  <- rasterTheme(region = c(reds.rev, blues))
myTheme.ltm.zero2  <- rasterTheme(region = c(reds.rev,zeroCol,blues))
#myTheme.ltm.zero.beige  <- rasterTheme(region = c(reds.rev,beige, zeroCol, blues))
myTheme.ltm<- rasterTheme(region = c(reds.rev, blues))
myTheme.ltmrev<- rasterTheme(region = c(rev(blues),reds))
myTheme.ltm.reds <- rasterTheme(region = reds)
myTheme.ltm.blues <- rasterTheme(region = c( blues))
myTheme.ltm.blues <- rasterTheme(region = c( blues))
myTheme.ltm.rdylbu <- rasterTheme(region = c(yelowGnBlueYl))
myTheme.ltm.terrain <- rasterTheme(region = myTheme.ltm.rdylbu)
myTheme.ltm.grdvctopo<-rasterTheme(region = grDevices::matlablike)
my.theme.gn2rd<-rasterTheme(region =Gn2Rd)
myTheme.terrain=rasterTheme(region=terrain.colors( n=9))
myTheme.topo=rasterTheme(region=brewer.pal('Blues', n=9))
terrain<-rev(terrain.colors(12))
myThemeblue2red = rasterTheme(region = blue2red (36))
myTheme.matlab = rasterTheme(matlab.like(12))
color.regions(rev('YlOrRd'),"#B3B3B3",'Blues')


