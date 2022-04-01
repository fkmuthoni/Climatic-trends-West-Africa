#######################################################################################################################
##Map magnitude & significance of monthly trends of Tmax 
#####################################################################################################################
#Load required packages
##################################################################################
###Load required packages
##*************************************************
#require(BiodiversityR)
#BiodiversityRGUI()
#require (ncdf4)
#require (RNetCDF)
require(raster)
#require (maptools)
require(modifiedmk)
#require (fields)
#library(rgdal)
library(rasterVis)
library(sp)
require(stringi)
#require(lubridate)
require(colorRamps)
require(RColorBrewer)
require(grDevices)
#require (classInt)
#require(data.table)
library(tidyr)
#library(dplyr)
#require(plyr)
require(EcoGenetics)
source('ecotheilsen_modified.R')
require(hydroGOF)
####********************************************************************

##Set working directory##
setwd("E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED")

#Load monthly time series of Tmax 
terraclimTmax.bk<-brick('terraclimTmaxbk.tif')

#Separate time series for each month
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

####################################################################################################
##Map magnitude & significance of monthly trends of Tmax (start with month when the season start in particular area)
####################################################################################################
###Apr monthly series
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTmaxwa.apr.maskras <- calc(tcTmaxwa.apr, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.apr.maskras)=0
tcTmaxwa.apr = mask(tcTmaxwa.apr, tcTmaxwa.apr.maskras)
tcTmaxwa.apr[tcTmaxwa.apr==0] = NA

TCWaTmaxmmkh3lag.apr = eco.theilsen2(tcTmaxwa.apr,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagapr.slope <- raster("TCWaTmaxmmkh3lagaprslope.tif")
TCWaTmaxmmkh3lagapr.pvalue <- raster("TCWaTmaxmmkh3lagaprpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagapr.p = calc(TCWaTmaxmmkh3lagapr.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopeapr.sig = mask(TCWaTmaxmmkh3lagapr.slope, TCWaTmaxmmkh3lagapr.p)
writeRaster(TCwaTmaxmmkh3lagslopeapr.sig, 'TCwaTmaxmmkh3lagslopeaprsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopeapr.sig)
TCwaTmaxmmkh3lagslopeapr.sig =raster('TCwaTmaxmmkh3lagslopeaprsig.tif')

#Stack overall trend and significant apr trends for rainfall
TCwaTmaxmmkh3lagslopeapr.all.sig = stack(TCWaTmaxmmkh3lagapr.slope,TCwaTmaxmmkh3lagslopeapr.sig)
names(TCwaTmaxmmkh3lagslopeapr.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopeapr.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###May
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTmaxwa.may.maskras <- calc(tcTmaxwa.may, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.may.maskras)=0
tcTmaxwa.may = mask(tcTmaxwa.may, tcTmaxwa.may.maskras)
tcTmaxwa.may[tcTmaxwa.may==0] = NA

TCWaTmaxmmkh3lag.may = eco.theilsen2(tcTmaxwa.may,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagmay.slope <- raster("TCWaTmaxmmkh3lagmayslope.tif")
TCWaTmaxmmkh3lagmay.pvalue <- raster("TCWaTmaxmmkh3lagmaypvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagmay.p = calc(TCWaTmaxmmkh3lagmay.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopemay.sig = mask(TCWaTmaxmmkh3lagmay.slope, TCWaTmaxmmkh3lagmay.p)
writeRaster(TCwaTmaxmmkh3lagslopemay.sig, 'TCwaTmaxmmkh3lagslopemaysig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopemay.sig)
TCwaTmaxmmkh3lagslopemay.sig =raster('TCwaTmaxmmkh3lagslopemaysig.tif')

#Stack overall trend and significant may trends for rainfall
TCwaTmaxmmkh3lagslopemay.all.sig = stack(TCWaTmaxmmkh3lagmay.slope,TCwaTmaxmmkh3lagslopemay.sig)
names(TCwaTmaxmmkh3lagslopemay.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopemay.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###June
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTmaxwa.jun.maskras <- calc(tcTmaxwa.jun, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.jun.maskras)=0
tcTmaxwa.jun = mask(tcTmaxwa.jun, tcTmaxwa.jun.maskras)
tcTmaxwa.jun[tcTmaxwa.jun==0] = NA

TCWaTmaxmmkh3lag.jun = eco.theilsen2(tcTmaxwa.jun,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagjun.slope <- raster("TCWaTmaxmmkh3lagjunslope.tif")
TCWaTmaxmmkh3lagjun.pvalue <- raster("TCWaTmaxmmkh3lagjunpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagjun.p = calc(TCWaTmaxmmkh3lagjun.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopejun.sig = mask(TCWaTmaxmmkh3lagjun.slope, TCWaTmaxmmkh3lagjun.p)
writeRaster(TCwaTmaxmmkh3lagslopejun.sig, 'TCwaTmaxmmkh3lagslopejunsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopejun.sig)
TCwaTmaxmmkh3lagslopejun.sig =raster('TCwaTmaxmmkh3lagslopejunsig.tif')

#Stack overall trend and significant jun trends for rainfall
TCwaTmaxmmkh3lagslopejun.all.sig = stack(TCWaTmaxmmkh3lagjun.slope,TCwaTmaxmmkh3lagslopejun.sig)
names(TCwaTmaxmmkh3lagslopejun.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopejun.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###July
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.jul.maskras <- calc(tcTmaxwa.jul, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.jul.maskras)=0
tcTmaxwa.jul = mask(tcTmaxwa.jul, tcTmaxwa.jul.maskras)
tcTmaxwa.jul[tcTmaxwa.jul==0] = NA

TCWaTmaxmmkh3lag.jul = eco.theilsen2(tcTmaxwa.jul,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagjul.slope <- raster("TCWaTmaxmmkh3lagjulslope.tif")
TCWaTmaxmmkh3lagjul.pvalue <- raster("TCWaTmaxmmkh3lagjulpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagjul.p = calc(TCWaTmaxmmkh3lagjul.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopejul.sig = mask(TCWaTmaxmmkh3lagjul.slope, TCWaTmaxmmkh3lagjul.p)
writeRaster(TCwaTmaxmmkh3lagslopejul.sig, 'TCwaTmaxmmkh3lagslopejulsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopejul.sig)
TCwaTmaxmmkh3lagslopejul.sig =raster('TCwaTmaxmmkh3lagslopejulsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwaTmaxmmkh3lagslopejul.all.sig = stack(TCWaTmaxmmkh3lagjul.slope,TCwaTmaxmmkh3lagslopejul.sig)
names(TCwaTmaxmmkh3lagslopejul.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopejul.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Aug
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.aug.maskras <- calc(tcTmaxwa.aug, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.aug.maskras)=0
tcTmaxwa.aug = mask(tcTmaxwa.aug, tcTmaxwa.aug.maskras)
tcTmaxwa.aug[tcTmaxwa.aug==0] = NA

TCWaTmaxmmkh3lag.aug = eco.theilsen2(tcTmaxwa.aug,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagaug.slope <- raster("TCWaTmaxmmkh3lagaugslope.tif")
TCWaTmaxmmkh3lagaug.pvalue <- raster("TCWaTmaxmmkh3lagaugpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagaug.p = calc(TCWaTmaxmmkh3lagaug.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopeaug.sig = mask(TCWaTmaxmmkh3lagaug.slope, TCWaTmaxmmkh3lagaug.p)
writeRaster(TCwaTmaxmmkh3lagslopeaug.sig, 'TCwaTmaxmmkh3lagslopeaugsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopeaug.sig)
TCwaTmaxmmkh3lagslopeaug.sig =raster('TCwaTmaxmmkh3lagslopeaugsig.tif')

#Stack overall trend and significant aug trends for rainfall
TCwaTmaxmmkh3lagslopeaug.all.sig = stack(TCWaTmaxmmkh3lagaug.slope,TCwaTmaxmmkh3lagslopeaug.sig)
names(TCwaTmaxmmkh3lagslopeaug.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopeaug.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Sept
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.sep.maskras <- calc(tcTmaxwa.sep, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.sep.maskras)=0
tcTmaxwa.sep = mask(tcTmaxwa.sep, tcTmaxwa.sep.maskras)
tcTmaxwa.sep[tcTmaxwa.sep==0] = NA

TCWaTmaxmmkh3lag.sep = eco.theilsen2(tcTmaxwa.sep,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagsep.slope <- raster("TCWaTmaxmmkh3lagsepslope.tif")
TCWaTmaxmmkh3lagsep.pvalue <- raster("TCWaTmaxmmkh3lagseppvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagsep.p = calc(TCWaTmaxmmkh3lagsep.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopesep.sig = mask(TCWaTmaxmmkh3lagsep.slope, TCWaTmaxmmkh3lagsep.p)
writeRaster(TCwaTmaxmmkh3lagslopesep.sig, 'TCwaTmaxmmkh3lagslopesepsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopesep.sig)
TCwaTmaxmmkh3lagslopesep.sig =raster('TCwaTmaxmmkh3lagslopesepsig.tif')

#Stack overall trend and significant sep trends for rainfall
TCwaTmaxmmkh3lagslopesep.all.sig = stack(TCWaTmaxmmkh3lagsep.slope,TCwaTmaxmmkh3lagslopesep.sig)
names(TCwaTmaxmmkh3lagslopesep.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopesep.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Oct
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.oct.maskras <- calc(tcTmaxwa.oct, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.oct.maskras)=0
tcTmaxwa.oct = mask(tcTmaxwa.oct, tcTmaxwa.oct.maskras)
tcTmaxwa.oct[tcTmaxwa.oct==0] = NA

TCWaTmaxmmkh3lag.oct = eco.theilsen2(tcTmaxwa.oct,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagoct.slope <- raster("TCWaTmaxmmkh3lagoctslope.tif")
TCWaTmaxmmkh3lagoct.pvalue <- raster("TCWaTmaxmmkh3lagoctpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagoct.p = calc(TCWaTmaxmmkh3lagoct.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopeoct.sig = mask(TCWaTmaxmmkh3lagoct.slope, TCWaTmaxmmkh3lagoct.p)
writeRaster(TCwaTmaxmmkh3lagslopeoct.sig, 'TCwaTmaxmmkh3lagslopeoctsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopeoct.sig)
TCwaTmaxmmkh3lagslopeoct.sig =raster('TCwaTmaxmmkh3lagslopeoctsig.tif')

#Stack overall trend and significant oct trends for rainfall
TCwaTmaxmmkh3lagslopeoct.all.sig = stack(TCWaTmaxmmkh3lagoct.slope,TCwaTmaxmmkh3lagslopeoct.sig)
names(TCwaTmaxmmkh3lagslopeoct.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopeoct.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Nov
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.nov.maskras <- calc(tcTmaxwa.nov, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.nov.maskras)=0
tcTmaxwa.nov = mask(tcTmaxwa.nov, tcTmaxwa.nov.maskras)
tcTmaxwa.nov[tcTmaxwa.nov==0] = NA

TCWaTmaxmmkh3lag.nov = eco.theilsen2(tcTmaxwa.nov,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagnov.slope <- raster("TCWaTmaxmmkh3lagnovslope.tif")
TCWaTmaxmmkh3lagnov.pvalue <- raster("TCWaTmaxmmkh3lagnovpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagnov.p = calc(TCWaTmaxmmkh3lagnov.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopenov.sig = mask(TCWaTmaxmmkh3lagnov.slope, TCWaTmaxmmkh3lagnov.p)
writeRaster(TCwaTmaxmmkh3lagslopenov.sig, 'TCwaTmaxmmkh3lagslopenovsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopenov.sig)
TCwaTmaxmmkh3lagslopenov.sig =raster('TCwaTmaxmmkh3lagslopenovsig.tif')

#Stack overall trend and significant nov trends for rainfall
TCwaTmaxmmkh3lagslopenov.all.sig = stack(TCWaTmaxmmkh3lagnov.slope,TCwaTmaxmmkh3lagslopenov.sig)
names(TCwaTmaxmmkh3lagslopenov.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopenov.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###DEC
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.dec.maskras <- calc(tcTmaxwa.dec, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.dec.maskras)=0
tcTmaxwa.dec = mask(tcTmaxwa.dec, tcTmaxwa.dec.maskras)
tcTmaxwa.dec[tcTmaxwa.dec==0] = NA

TCWaTmaxmmkh3lag.dec = eco.theilsen2(tcTmaxwa.dec,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagdec.slope <- raster("TCWaTmaxmmkh3lagdecslope.tif")
TCWaTmaxmmkh3lagdec.pvalue <- raster("TCWaTmaxmmkh3lagdecpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagdec.p = calc(TCWaTmaxmmkh3lagdec.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopedec.sig = mask(TCWaTmaxmmkh3lagdec.slope, TCWaTmaxmmkh3lagdec.p)
writeRaster(TCwaTmaxmmkh3lagslopedec.sig, 'TCwaTmaxmmkh3lagslopedecsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopedec.sig)
TCwaTmaxmmkh3lagslopedec.sig =raster('TCwaTmaxmmkh3lagslopedecsig.tif')

#Stack overall trend and significant dec trends for rainfall
TCwaTmaxmmkh3lagslopedec.all.sig = stack(TCWaTmaxmmkh3lagdec.slope,TCwaTmaxmmkh3lagslopedec.sig)
names(TCwaTmaxmmkh3lagslopedec.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopedec.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Jan
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.jan.maskras <- calc(tcTmaxwa.jan, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.jan.maskras)=0
tcTmaxwa.jan = mask(tcTmaxwa.jan, tcTmaxwa.jan.maskras)
tcTmaxwa.jan[tcTmaxwa.jan==0] = NA

TCWaTmaxmmkh3lag.jan = eco.theilsen2(tcTmaxwa.jan,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagjan.slope <- raster("TCWaTmaxmmkh3lagjanslope.tif")
TCWaTmaxmmkh3lagjan.pvalue <- raster("TCWaTmaxmmkh3lagjanpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagjan.p = calc(TCWaTmaxmmkh3lagjan.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopejan.sig = mask(TCWaTmaxmmkh3lagjan.slope, TCWaTmaxmmkh3lagjan.p)
writeRaster(TCwaTmaxmmkh3lagslopejan.sig, 'TCwaTmaxmmkh3lagslopejansig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopejan.sig)
TCwaTmaxmmkh3lagslopejan.sig =raster('TCwaTmaxmmkh3lagslopejansig.tif')

#Stack overall trend and significant jan trends for rainfall
TCwaTmaxmmkh3lagslopejan.all.sig = stack(TCWaTmaxmmkh3lagjan.slope,TCwaTmaxmmkh3lagslopejan.sig)
names(TCwaTmaxmmkh3lagslopejan.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopejan.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Feb
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.feb.maskras <- calc(tcTmaxwa.feb, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.feb.maskras)=0
tcTmaxwa.feb = mask(tcTmaxwa.feb, tcTmaxwa.feb.maskras)
tcTmaxwa.feb[tcTmaxwa.feb==0] = NA

TCWaTmaxmmkh3lag.feb = eco.theilsen2(tcTmaxwa.feb,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagfeb.slope <- raster("TCWaTmaxmmkh3lagfebslope.tif")
TCWaTmaxmmkh3lagfeb.pvalue <- raster("TCWaTmaxmmkh3lagfebpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagfeb.p = calc(TCWaTmaxmmkh3lagfeb.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopefeb.sig = mask(TCWaTmaxmmkh3lagfeb.slope, TCWaTmaxmmkh3lagfeb.p)
writeRaster(TCwaTmaxmmkh3lagslopefeb.sig, 'TCwaTmaxmmkh3lagslopefebsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopefeb.sig)
TCwaTmaxmmkh3lagslopefeb.sig =raster('TCwaTmaxmmkh3lagslopefebsig.tif')

#Stack overall trend and significant feb trends for rainfall
TCwaTmaxmmkh3lagslopefeb.all.sig = stack(TCWaTmaxmmkh3lagfeb.slope,TCwaTmaxmmkh3lagslopefeb.sig)
names(TCwaTmaxmmkh3lagslopefeb.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopefeb.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###March
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTmaxwa.mar.maskras <- calc(tcTmaxwa.mar, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTmaxwa.mar.maskras)=0
tcTmaxwa.mar = mask(tcTmaxwa.mar, tcTmaxwa.mar.maskras)
tcTmaxwa.mar[tcTmaxwa.mar==0] = NA

TCWaTmaxmmkh3lag.mar = eco.theilsen2(tcTmaxwa.mar,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTmaxmmkh3lagmar.slope <- raster("TCWaTmaxmmkh3lagmarslope.tif")
TCWaTmaxmmkh3lagmar.pvalue <- raster("TCWaTmaxmmkh3lagmarpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTmaxmmkh3lagmar.p = calc(TCWaTmaxmmkh3lagmar.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTmaxmmkh3lagslopemar.sig = mask(TCWaTmaxmmkh3lagmar.slope, TCWaTmaxmmkh3lagmar.p)
writeRaster(TCwaTmaxmmkh3lagslopemar.sig, 'TCwaTmaxmmkh3lagslopemarsig.tif',format='GTiff')
rm(TCwaTmaxmmkh3lagslopemar.sig)
TCwaTmaxmmkh3lagslopemar.sig =raster('TCwaTmaxmmkh3lagslopemarsig.tif')

#Stack overall trend and significant mar trends for rainfall
TCwaTmaxmmkh3lagslopemar.all.sig = stack(TCWaTmaxmmkh3lagmar.slope,TCwaTmaxmmkh3lagslopemar.sig)
names(TCwaTmaxmmkh3lagslopemar.all.sig) = c('a','b')
levelplot(TCwaTmaxmmkh3lagslopemar.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

#######################################
#Stack Monthly slopes
TCTmaxmonthlyslopes = stack(TCWaTmaxmmkh3lagapr.slope,TCWaTmaxmmkh3lagmay.slope,TCWaTmaxmmkh3lagjun.slope,TCWaTmaxmmkh3lagjul.slope,TCWaTmaxmmkh3lagaug.slope,TCWaTmaxmmkh3lagsep.slope,TCWaTmaxmmkh3lagoct.slope,TCWaTmaxmmkh3lagnov.slope,TCWaTmaxmmkh3lagdec.slope,TCWaTmaxmmkh3lagjan.slope, TCWaTmaxmmkh3lagfeb.slope,TCWaTmaxmmkh3lagmar.slope)
writeRaster(TCTmaxmonthlyslopes, 'TCTmaxmonthlyslopes.tif', format='GTiff')
TCTmaxmonthlyslopes =stack('TCTmaxmonthlyslopes.tif')
#names(TCTmaxmonthlyslopes)= c('Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar')
names(TCTmaxmonthlyslopes)=c('April','May','June','July','August','September','October','November','December','January','February','March')
TCTmaxmonthlyslopes.trim = trim(TCTmaxmonthlyslopes, padding=0, values=NA)

##define class intervals for plotting
myat.monthly.sigtrend.Tmax =seq(min(minValue(TCTmaxmonthlyslope.sig)),max(maxValue(TCTmaxmonthlyslope.sig)),length.out=12)
myat.monthly.sigtrend.Tmax = round (myat.monthly.sigtrend.Tmax, digits = 2)
#myat.monthly.sigtrend.Tmax= c(-0.047,-0.035,-0.025,-0.015,-0.01,-0.005,0,0.005,0.015,0.025,0.035,0.045,0.055,0.067)
myat.monthly.sigtrend.Tmax= c(-0.046,-0.035,-0.025,-0.015,-0.005,-0.0025,0, 0.005, 0.015,0.026,0.036,0.046,0.056,0.067)
myat.monthly.sigtrend.Tmax.lab= c(-0.046,-0.035,-0.025,-0.015,-0.005,0, 0.005, 0.015,0.026,0.036,0.046,0.056,0.067)
#myat.monthly.sigtrend.Tmax.lab= c(-0.046,-0.035,-0.025,-0.015,0,0.015,0.026,0.036,0.046,0.056,0.067)

#myat.monthly.sigtrend.Tmax= c(-0.045,-0.035,-0.025,-0.015,0,0.015,0.025,0.035,0.045,0.055,0.065, 0.07)

####################################################################################
#########Make custom color palletes and Rastervis Raster Themes for ploting trends
####################################################################################
# Set color palette
zeroCol <-"#B3B3B3" #
beige<-grDevices::colors(16)
reds.rev <- rev(brewer.pal('YlOrRd', n = 9))
reds <- brewer.pal('YlOrRd', n = 9)
blues <- brewer.pal('Blues', n = 9)
yelowGnBlueYl<-brewer.pal('YlGnBu', n=9)
grdeices.matlablike<-rev(grDevices::topo.colors(20))
matlablike2<-matlab.like2(12)
Gn2Rd<-colorRamps::green2red(100)


#Custom RasterVis Themes
myTheme.ltm.zero <- rasterTheme(region = c(reds.rev, blues))
myTheme.ltm.zero2 <- rasterTheme(region = c(reds.rev,zeroCol,blues))
#myTheme.ltm.zero.beige <- rasterTheme(region = c(reds.rev,beige, zeroCol, blues))
myTheme.ltm<- rasterTheme(region = c(reds.rev, blues))
myTheme.ltmrev<- rasterTheme(region = c(rev(blues),reds))
myTheme.ltm.BlRd <- rasterTheme(region = c(rev(blues), (reds)))
myTheme.ltm.reds <- rasterTheme(region = reds)
myTheme.ltm.blues <- rasterTheme(region = c( blues))
myTheme.ltm.rdylbu <- rasterTheme(region = c(yelowGnBlueYl))
myTheme.ltm.grdvctopo<-rasterTheme(region = grdeices.matlablike)
myTheme.matlablike2 <- rasterTheme(region = c(matlablike2))
myTheme.matlablike2rev <- rasterTheme(region = c(rev(matlablike2)))
my.theme.gn2rd<-rasterTheme(region =Gn2Rd)
terrain<-rev(terrain.colors(12))
######################################################################################
##Plot monthly Theil-Sens slopes (showing magnitude of trends)

levelplot(TCTmaxmonthlyslopes,at=myat.monthly.sigtrend.Tmax, par.settings=myTheme.ltm.BlRd , colorkey=list( at=myat.monthly.sigtrend.Tmax,labels=list(cex=1,at=myat.monthly.sigtrend.Tmax), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

#Stack and plot only monthly significant slopes
TCTmaxmonthlyslope.sig = stack(TCwaTmaxmmkh3lagslopeapr.sig,TCwaTmaxmmkh3lagslopemay.sig,TCwaTmaxmmkh3lagslopejun.sig,TCwaTmaxmmkh3lagslopejul.sig,TCwaTmaxmmkh3lagslopeaug.sig,TCwaTmaxmmkh3lagslopesep.sig,TCwaTmaxmmkh3lagslopeoct.sig,TCwaTmaxmmkh3lagslopenov.sig,TCwaTmaxmmkh3lagslopedec.sig,TCwaTmaxmmkh3lagslopejan.sig, TCwaTmaxmmkh3lagslopefeb.sig,TCwaTmaxmmkh3lagslopemar.sig)
writeRaster(TCTmaxmonthlyslope.sig, 'TCTmaxmonthlyslopesig.tif', format='GTiff')
TCTmaxmonthlyslope.sig =stack('TCTmaxmonthlyslopesig.tif')
names(TCTmaxmonthlyslope.sig) = c('April','May','June','July','August','September','October','November','December','January','February','March')

levelplot(TCTmaxmonthlyslope.sig,at=myat.monthly.sigtrend.Tmax, par.settings=myTheme.ltm.BlRd , colorkey=list( at=myat.monthly.sigtrend.Tmax,labels=list(cex=1,at=myat.monthly.sigtrend.Tmax), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

############################################################################################################
####Plot Monthly slopes with hatchedLayers####
############################################################################################################
##
####Convert significant slope raster to SPplolygons####
TCTmax.poly.sign = lapply(as.list(TCTmaxmonthlyslope.sig), rasterToPolygons, dissolve=T)

####Generate hatchedLayer for Theilsen slopes for each month####
#Apr
TCTmax.poly.sign.apr=TCTmax.poly.sign[[1]]
TCTmax.poly.sign.apr =sf::st_as_sf(TCTmax.poly.sign.apr)
TCTmax.poly.sign.apr$area <- (st_area(TCTmax.poly.sign.apr)/1000000)
TCTmax.poly.sign.dsl.apr = TCTmax.poly.sign.apr %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.aprhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.apr, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.aprhl.sp <- as(TCTmax.poly.sign.aprhl, "Spatial")

#May
TCTmax.poly.sign.May=TCTmax.poly.sign[[2]]
TCTmax.poly.sign.May =sf::st_as_sf(TCTmax.poly.sign.May)
TCTmax.poly.sign.May$area <- (st_area(TCTmax.poly.sign.May)/1000000)
TCTmax.poly.sign.dsl.May = TCTmax.poly.sign.May %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Mayhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.May, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Mayhl.sp <- as(TCTmax.poly.sign.Mayhl, "Spatial")

#Jun
TCTmax.poly.sign.Jun=TCTmax.poly.sign[[3]]
TCTmax.poly.sign.Jun =sf::st_as_sf(TCTmax.poly.sign.Jun)
TCTmax.poly.sign.Jun$area <- (st_area(TCTmax.poly.sign.Jun)/1000000)
TCTmax.poly.sign.dsl.Jun = TCTmax.poly.sign.Jun %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Junhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Jun, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Junhl.sp <- as(TCTmax.poly.sign.Junhl, "Spatial")

#Jul
TCTmax.poly.sign.Jul=TCTmax.poly.sign[[4]]
TCTmax.poly.sign.Jul =sf::st_as_sf(TCTmax.poly.sign.Jul)
TCTmax.poly.sign.Jul$area <- (st_area(TCTmax.poly.sign.Jul)/1000000)
TCTmax.poly.sign.dsl.Jul = TCTmax.poly.sign.Jul %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Julhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Jul, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Julhl.sp <- as(TCTmax.poly.sign.Julhl, "Spatial")

#Aug
TCTmax.poly.sign.Aug=TCTmax.poly.sign[[5]]
TCTmax.poly.sign.Aug =sf::st_as_sf(TCTmax.poly.sign.Aug)
TCTmax.poly.sign.Aug$area <- (st_area(TCTmax.poly.sign.Aug)/1000000)
TCTmax.poly.sign.dsl.Aug = TCTmax.poly.sign.Aug %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Aughl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Aug, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Aughl.sp <- as(TCTmax.poly.sign.Aughl, "Spatial")
#Sep
TCTmax.poly.sign.Sep=TCTmax.poly.sign[[6]]
TCTmax.poly.sign.Sep =sf::st_as_sf(TCTmax.poly.sign.Sep)
TCTmax.poly.sign.Sep$area <- (st_area(TCTmax.poly.sign.Sep)/1000000)
TCTmax.poly.sign.dsl.Sep = TCTmax.poly.sign.Sep %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Sephl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Sep, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Sephl.sp <- as(TCTmax.poly.sign.Sephl, "Spatial")
#Oct
TCTmax.poly.sign.Oct=TCTmax.poly.sign[[7]]
TCTmax.poly.sign.Oct =sf::st_as_sf(TCTmax.poly.sign.Oct)
TCTmax.poly.sign.Oct$area <- (st_area(TCTmax.poly.sign.Oct)/1000000)
TCTmax.poly.sign.dsl.Oct = TCTmax.poly.sign.Oct %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Octhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Oct, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Octhl.sp <- as(TCTmax.poly.sign.Octhl, "Spatial")

#Nov
TCTmax.poly.sign.Nov=TCTmax.poly.sign[[8]]
TCTmax.poly.sign.Nov =sf::st_as_sf(TCTmax.poly.sign.Nov)
TCTmax.poly.sign.Nov$area <- (st_area(TCTmax.poly.sign.Nov)/1000000)
TCTmax.poly.sign.dsl.Nov = TCTmax.poly.sign.Nov %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Novhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Nov, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Novhl.sp <- as(TCTmax.poly.sign.Novhl, "Spatial")
#Dec
TCTmax.poly.sign.Dec=TCTmax.poly.sign[[9]]
TCTmax.poly.sign.Dec =sf::st_as_sf(TCTmax.poly.sign.Dec)
TCTmax.poly.sign.Dec$area <- (st_area(TCTmax.poly.sign.Dec)/1000000)
TCTmax.poly.sign.dsl.Dec = TCTmax.poly.sign.Dec %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Dechl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Dec, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Dechl.sp <- as(TCTmax.poly.sign.Dechl, "Spatial")
#Jan
TCTmax.poly.sign.Jan=TCTmax.poly.sign[[10]]
TCTmax.poly.sign.Jan =sf::st_as_sf(TCTmax.poly.sign.Jan)
TCTmax.poly.sign.Jan$area <- (st_area(TCTmax.poly.sign.Jan)/1000000)
TCTmax.poly.sign.dsl.Jan = TCTmax.poly.sign.Jan %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Janhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Jan, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Janhl.sp <- as(TCTmax.poly.sign.Janhl, "Spatial")

#Feb
TCTmax.poly.sign.Feb=TCTmax.poly.sign[[11]]
TCTmax.poly.sign.Feb =sf::st_as_sf(TCTmax.poly.sign.Feb)
TCTmax.poly.sign.Feb$area <- (st_area(TCTmax.poly.sign.Feb)/1000000)
TCTmax.poly.sign.dsl.Feb = TCTmax.poly.sign.Feb %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Febhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Feb, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Febhl.sp <- as(TCTmax.poly.sign.Febhl, "Spatial")

#Mar
TCTmax.poly.sign.Mar=TCTmax.poly.sign[[12]]
TCTmax.poly.sign.Mar =sf::st_as_sf(TCTmax.poly.sign.Mar)
TCTmax.poly.sign.Mar$area <- (st_area(TCTmax.poly.sign.Mar)/1000000)
TCTmax.poly.sign.dsl.Mar = TCTmax.poly.sign.Mar %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmax.poly.sign.Marhl =cartography::hatchedLayer(TCTmax.poly.sign.dsl.Mar, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmax.poly.sign.Marhl.sp <- as(TCTmax.poly.sign.Marhl, "Spatial")

#List spatial lines hatchLayers for each month
TCTmax.sig.hatch = list(TCTmax.poly.sign.aprhl.sp,TCTmax.poly.sign.Mayhl.sp,TCTmax.poly.sign.Junhl.sp,TCTmax.poly.sign.Julhl.sp,TCTmax.poly.sign.Aughl.sp,TCTmax.poly.sign.Sephl.sp,TCTmax.poly.sign.Octhl.sp,TCTmax.poly.sign.Novhl.sp,TCTmax.poly.sign.Dechl.sp,TCTmax.poly.sign.Janhl.sp, TCTmax.poly.sign.Febhl.sp, TCTmax.poly.sign.Marhl.sp)

####Plot hatchedLayers for statistically sign area over the TheilSens slope for each month####

levelplot(TCTmaxmonthlyslopes,at=myat.monthly.sigtrend.Tmax, par.settings=myTheme.ltm.BlRd , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend.Tmax,labels=list(cex=1,at=myat.monthly.sigtrend.Tmax.lab), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))+
  latticeExtra::layer(sp.lines(TCTmax.sig.hatch[[panel.number()]], col = 'black', lwd = 0.01, cex = 1, alpha=1))+
  latticeExtra::layer(panel.text(3, 14.5,index.numbering.Temp[panel.number()],  col = txtColors, fontface= 'bold', cex=1.2))

############################################################################################################
####Analyze Tmax trends per AEZ####
############################################################################################################
#Import AEZ layer##

#aezWArst = raster('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/aezWA6rst.tif')
#aezWArst.tc = projectRaster(aezWArst, TCWaTminmmkh3lagapr.slope)
#aezWArst.fun=function(x) { x[x>7.0001] <- NA; return(x)}
#aezWArst <- calc(aezWArst, aezWArst.fun)
#aezWArst.tc = round(aezWArst.tc)

aezWArst.tc = raster('aezWArstTC.tif')
#aezWArst.tc = ratify(aezWArst.tc)
##Zonal mean of Tmax trends## 

#TCTmaxaez.wa = zonal(TCTmaxmonthlyslopes, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalTmaxWA.csv")
TCTmaxaez.waSig = as.data.frame(zonal(TCTmaxmonthlyslope.sig, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalTmaxWASig.csv"))

####Boxplot for TCTmax trends per AEZ####

par(mfrow = c(3, 4), mar = c(4, 4, 1, 1))

bxtrendTmax.Apr = boxplot(TCTmaxmonthlyslopes[[1]], aezWArst.tc, xlab= '', ylab='Trend (oC yr-1)', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Apr",side=3,line=0,at=3, cex=1)
bxtrendTmax.May = boxplot(TCTmaxmonthlyslopes[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("May",side=3,line=0,at=3, cex=1)
bxtrendTmax.Jun = boxplot(TCTmaxmonthlyslopes[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Jun",side=3,line=0,at=3, cex=1)
bxtrendTmax.Jul = boxplot(TCTmaxmonthlyslopes[[4]], aezWArst.tc,  xlab= '',ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Jul",side=3,line=0,at=3, cex=1)
bxtrendTmax.Aug = boxplot(TCTmaxmonthlyslopes[[5]], aezWArst.tc,   xlab= '',ylab='Trend (oC yr-1)', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Aug",side=3,line=0,at=3, cex=1)
bxtrendTmax.Sept= boxplot(TCTmaxmonthlyslopes[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Sept",side=3,line=0,at=3, cex=1)
bxtrendTmax.Oct = boxplot(TCTmaxmonthlyslopes[[7]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Oct",side=3,line=0,at=3, cex=1)
bxtrendTmax.Nov = boxplot(TCTmaxmonthlyslopes[[8]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Nov",side=3,line=0,at=3, cex=1)
bxtrendTmax.Dec = boxplot(TCTmaxmonthlyslopes[[9]], aezWArst.tc,  xlab= 'Zone',ylab='Trend (oC yr-1)', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Dec",side=3,line=0,at=3, cex=1)
bxtrendTmax.Jan = boxplot(TCTmaxmonthlyslopes[[10]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Jan",side=3,line=0,at=3, cex=1)
bxtrendTmax.Feb = boxplot(TCTmaxmonthlyslopes[[11]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Feb",side=3,line=0,at=3, cex=1)
bxtrendTmax.Mar = boxplot(TCTmaxmonthlyslopes[[12]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.065), cex.lab= 1.3)
mtext("Mar",side=3,line=0,at=3, cex=1)

##
####Boxplot for TCTmax trends per AEZ for only significant trends####


par(mfrow = c(3, 4), mgp=c(2,0.5,0), mar = c(4, 4, 1., 1))

bxtrendTmax.Apr = boxplot(TCTmaxmonthlyslope.sig[[1]], aezWArst.tc, xlab= '', ylab=expression(Tmax~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.05), cex.lab= 1.5) #Make font bold in ylab=list(expression(Tmax~(C^{o}~Month^{-1}~year^{-1})), fontface='bold', rot=90)
mtext("April",side=3,line=0,at=3, cex=1)
mtext("(a)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Apr, pch = 4, cex =2,col = "red")

bxtrendTmax.May = boxplot(TCTmaxmonthlyslope.sig[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("May",side=3,line=0,at=3, cex=1)
mtext("(b)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$May, pch = 4, cex =2,col = "red")

bxtrendTmax.Jun = boxplot(TCTmaxmonthlyslope.sig[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("June",side=3,line=0,at=3, cex=1)
mtext("(c)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Jun, pch = 4, cex =2,col = "red")

bxtrendTmax.Jul = boxplot(TCTmaxmonthlyslope.sig[[4]], aezWArst.tc,  xlab= '',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("July",side=3,line=0,at=3, cex=1)
mtext("(d)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Jul, pch = 4, cex =2,col = "red")

bxtrendTmax.Aug = boxplot(TCTmaxmonthlyslope.sig[[5]], aezWArst.tc,   xlab= '',ylab=expression(Tmax~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("August",side=3,line=0,at=3, cex=1)
mtext("(e)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Aug, pch = 4, cex =2,col = "red")

bxtrendTmax.Sept= boxplot(TCTmaxmonthlyslope.sig[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-0.05,0.05), cex.lab= 1.5)
mtext("September",side=3,line=0,at=3, cex=1)
mtext("(f)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Sept, pch = 4, cex =2,col = "red")

bxtrendTmax.Oct = boxplot(TCTmaxmonthlyslope.sig[[7]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("October",side=3,line=0,at=3, cex=1)
mtext("(g)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Oct, pch = 4, cex =2,col = "red")

bxtrendTmax.Nov = boxplot(TCTmaxmonthlyslope.sig[[8]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("November",side=3,line=0,at=3, cex=1)
mtext("(h)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Nov, pch = 4, cex =2,col = "red")

bxtrendTmax.Dec = boxplot(TCTmaxmonthlyslope.sig[[9]], aezWArst.tc,  xlab= 'Zone',ylab=expression(Tmax~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.06), cex.lab= 1.5)
mtext("December",side=3,line=0,at=3, cex=1)
mtext("(i)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Dec, pch = 4, cex =2,col = "red")

bxtrendTmax.Jan = boxplot(TCTmaxmonthlyslope.sig[[10]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.06), cex.lab= 1.5)
mtext("January",side=3,line=0,at=3, cex=1)
mtext("(j)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Jan, pch = 4, cex =2,col = "red")

bxtrendTmax.Feb = boxplot(TCTmaxmonthlyslope.sig[[11]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.06), cex.lab= 1.5)
mtext("February",side=3,line=0,at=3, cex=1)
mtext("(k)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Feb, pch = 4, cex =2,col = "red")

bxtrendTmax.Mar = boxplot(TCTmaxmonthlyslope.sig[[12]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.07), cex.lab= 1.5)
mtext("March",side=3,line=0,at=3, cex=1)
mtext("(l)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, TCTmaxaez.waSig$Mar, pch = 4, cex =2,col = "red")

####Test for significance differences in trends among zones####

Tmax.wa.signValues = data.frame(cbind(values(TCTmaxmonthlyslope.sig), values(aezWArst.tc)))
TmaxWasigdf = Tmax.wa.signValues %>% drop_na(AEZ) 
write.csv(TmaxWasigdf, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TmaxWaSigdf.csv')
TmaxWasigdf= read.csv('TmaxWaSigdf.csv', header=T, sep=',')
TmaxWasigdf.long =TmaxWasigdf %>% dplyr::filter_at(.vars = vars(Apr, May,Jun, Jul,Aug, Sept,Oct, Nov, Dec,Jan, Feb, Mar), .vars_predicate = any_vars(!is.na(.)))

#Convert wide to Long Df

TmaxWasigdf.long <- TmaxWasigdf %>% gather(Month, Trend, 2:13)
TmaxWasigdf.long$mergeID<-paste0(TmaxWasigdf.long$ID, TmaxWasigdf.long$Month,TmaxWasigdf.long$AEZ)
TmaxWasigdf.long$mergeID2<-paste0(TmaxWasigdf.long$Month, TmaxWasigdf.long$AEZ)
TmaxWasigdf.long$mergeID3<-paste0('Tmax-', TmaxWasigdf.long$mergeID2)
TmaxWasigdf.long <- within(TmaxWasigdf.long, {AEZ <- as.factor(AEZ)})
TmaxWasigdf.long <- within(TmaxWasigdf.long, {Month <- as.factor(Month)})
TmaxWasigdf.long <- within(TmaxWasigdf.long, {mergeID <- as.factor(mergeID)})
TmaxWasigdf.long <- within(TmaxWasigdf.long, {mergeID2 <- as.factor(mergeID2)})
TmaxWasigdf.long <- within(TmaxWasigdf.long, {mergeID3 <- as.factor(mergeID3)})
TmaxWasigdf.long = TmaxWasigdf.long %>% drop_na(Trend)
write.csv(TmaxWasigdf.long, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TmaxWasigdf.long.csv')

##Numerical summaries
Tmax.aez.wa.Sign.sum = numSummary(TmaxWasigdf.long[,"Trend", drop=FALSE], groups=TmaxWasigdf.long$mergeID2, statistics=c("mean", "sd", "median"))
Tmax_sig_AEZ_summary = as.data.frame(read.csv('Tmax_sig_AEZ_summary.csv', header=T, sep=','))

#Sort by MergeID
Tmax_sig_AEZ_summary.sort= Tmax_sig_AEZ_summary %>%
  arrange (match(Month.Zone, c("Apr-02", "Apr-03", "Apr-04", "Apr-05", "Apr-06", "Apr-07", "May-02" ,"May-03", "May-04", "May-05", "May-06", "May-07","Jun-02", "Jun-03" ,"Jun-04", "Jun-05", "Jun-06", "Jun-07",
                               "Jul-02", "Jul-03", "Jul-04", "Jul-05", "Jul-06", "Jul-07","Aug-02", "Aug-03", "Aug-04", "Aug-05", "Aug-06" ,"Aug-07", "Sep-02", "Sep-03", "Sep-04", "Sep-05", "Sep-06", "Sep-07",
                               "Oct-02", "Oct-03", "Oct-04", "Oct-05","Oct-06","Oct-07","Nov-02", "Nov-03", "Nov-04", "Nov-05", "Nov-06", "Nov-07","Dec-02", "Dec-03", "Dec-04", "Dec-05", "Dec-06", "Dec-07" ,"Jan-02" ,"Jan-03", "Jan-04" ,"Jan-05" ,"Jan-06","Jan-07",
                               "Feb-02", "Feb-03", "Feb-04", "Feb-05", "Feb-06", "Feb-07", "Mar-02", "Mar-03", "Mar-04", "Mar-05", "Mar-06", "Mar-07")), desc(mean), desc(sd),desc(n))

####Test significance differences in trends among zones####
##Test for normality based on Anderson-Darling normality test
normalityTest(Trend ~ mergeID2, test = "ad.test", data = TmaxWasigdf.long)

##Test for homogeneity of variances


bartlett.test(Trend ~ mergeID2, data = TmaxWasigdf.long) # var1 != varN

##bcnPower transformation to normality
TmaxWa.powerTransform = powerTransform(Trend ~ mergeID2, data = TmaxWasigdf.long, family = "bcnPower")

Tmax.wa.friedman = friedman.test(Trend~Month |AEZ, data=TmaxWasigdf.long)

####Kruskal.test ####

Tmax.Kruskal = kruskal.test(Trend ~ mergeID2, data = TmaxWasigdf.long)

####Post-hoc testNemenyi.test####

Tmax.nemenyi = posthoc.kruskal.nemenyi.test(Trend ~ mergeID2, data = TmaxWasigdf.long, dist="Chisquare", p.adjust.method="bonferroni")

################