#######################################################################################################################
##Map Tmin monthly trends
#####################################################################################################################
#######################################################################################################################
##Map magnitude & significance of monthly trends of Tmin 
#####################################################################################################################
###APR
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTminwa.apr.maskras <- calc(tcTminwa.apr, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.apr.maskras)=0
tcTminwa.apr = mask(tcTminwa.apr, tcTminwa.apr.maskras)
tcTminwa.apr[tcTminwa.apr==0] = NA

TCWaTminmmkh3lag.apr = eco.theilsen2(tcTminwa.apr,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagapr.slope <- raster("TCWaTminmmkh3lagaprslope.tif")
TCWaTminmmkh3lagapr.pvalue <- raster("TCWaTminmmkh3lagaprpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagapr.p = calc(TCWaTminmmkh3lagapr.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopeapr.sig = mask(TCWaTminmmkh3lagapr.slope, TCWaTminmmkh3lagapr.p)
writeRaster(TCwaTminmmkh3lagslopeapr.sig, 'TCwaTminmmkh3lagslopeaprsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopeapr.sig)
TCwaTminmmkh3lagslopeapr.sig =raster('TCwaTminmmkh3lagslopeaprsig.tif')

#Stack overall trend and significant apr trends for rainfall
TCwaTminmmkh3lagslopeapr.all.sig = stack(TCWaTminmmkh3lagapr.slope,TCwaTminmmkh3lagslopeapr.sig)
names(TCwaTminmmkh3lagslopeapr.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopeapr.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#My at
myat.apr.sigtrend =seq(min(minValue(TCwaTminmmkh3lagslopeapr.all.sig)),max(maxValue(TCwaTminmmkh3lagslopeapr.all.sig)),length.out=10)
myat.apr.sigtrend = round (myat.apr.sigtrend, digits = 1)
myat.apr.sigtrend = seq(-2.5, 0.5,1)
#myat.apr.sigtrend = c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5)


levelplot(TCwaTminmmkh3lagslopeapr.all.sig,at=myat.apr.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.apr.sigtrend,labels=list(cex=1,at=myat.apr.sigtrend), height=0.9), margin = FALSE,
          xlab='', ylab='')+
  layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###May
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTminwa.may.maskras <- calc(tcTminwa.may, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.may.maskras)=0
tcTminwa.may = mask(tcTminwa.may, tcTminwa.may.maskras)
tcTminwa.may[tcTminwa.may==0] = NA

TCWaTminmmkh3lag.may = eco.theilsen2(tcTminwa.may,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagmay.slope <- raster("TCWaTminmmkh3lagmayslope.tif")
TCWaTminmmkh3lagmay.pvalue <- raster("TCWaTminmmkh3lagmaypvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagmay.p = calc(TCWaTminmmkh3lagmay.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopemay.sig = mask(TCWaTminmmkh3lagmay.slope, TCWaTminmmkh3lagmay.p)
writeRaster(TCwaTminmmkh3lagslopemay.sig, 'TCwaTminmmkh3lagslopemaysig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopemay.sig)
TCwaTminmmkh3lagslopemay.sig =raster('TCwaTminmmkh3lagslopemaysig.tif')

#Stack overall trend and significant may trends for rainfall
TCwaTminmmkh3lagslopemay.all.sig = stack(TCWaTminmmkh3lagmay.slope,TCwaTminmmkh3lagslopemay.sig)
names(TCwaTminmmkh3lagslopemay.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopemay.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###June
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcTminwa.jun.maskras <- calc(tcTminwa.jun, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.jun.maskras)=0
tcTminwa.jun = mask(tcTminwa.jun, tcTminwa.jun.maskras)
tcTminwa.jun[tcTminwa.jun==0] = NA

TCWaTminmmkh3lag.jun = eco.theilsen2(tcTminwa.jun,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagjun.slope <- raster("TCWaTminmmkh3lagjunslope.tif")
TCWaTminmmkh3lagjun.pvalue <- raster("TCWaTminmmkh3lagjunpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagjun.p = calc(TCWaTminmmkh3lagjun.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopejun.sig = mask(TCWaTminmmkh3lagjun.slope, TCWaTminmmkh3lagjun.p)
writeRaster(TCwaTminmmkh3lagslopejun.sig, 'TCwaTminmmkh3lagslopejunsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopejun.sig)
TCwaTminmmkh3lagslopejun.sig =raster('TCwaTminmmkh3lagslopejunsig.tif')

#Stack overall trend and significant jun trends for rainfall
TCwaTminmmkh3lagslopejun.all.sig = stack(TCWaTminmmkh3lagjun.slope,TCwaTminmmkh3lagslopejun.sig)
names(TCwaTminmmkh3lagslopejun.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopejun.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###July
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.jul.maskras <- calc(tcTminwa.jul, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.jul.maskras)=0
tcTminwa.jul = mask(tcTminwa.jul, tcTminwa.jul.maskras)
tcTminwa.jul[tcTminwa.jul==0] = NA

TCWaTminmmkh3lag.jul = eco.theilsen2(tcTminwa.jul,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagjul.slope <- raster("TCWaTminmmkh3lagjulslope.tif")
TCWaTminmmkh3lagjul.pvalue <- raster("TCWaTminmmkh3lagjulpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagjul.p = calc(TCWaTminmmkh3lagjul.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopejul.sig = mask(TCWaTminmmkh3lagjul.slope, TCWaTminmmkh3lagjul.p)
writeRaster(TCwaTminmmkh3lagslopejul.sig, 'TCwaTminmmkh3lagslopejulsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopejul.sig)
TCwaTminmmkh3lagslopejul.sig =raster('TCwaTminmmkh3lagslopejulsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwaTminmmkh3lagslopejul.all.sig = stack(TCWaTminmmkh3lagjul.slope,TCwaTminmmkh3lagslopejul.sig)
names(TCwaTminmmkh3lagslopejul.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopejul.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Aug
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.aug.maskras <- calc(tcTminwa.aug, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.aug.maskras)=0
tcTminwa.aug = mask(tcTminwa.aug, tcTminwa.aug.maskras)
tcTminwa.aug[tcTminwa.aug==0] = NA

TCWaTminmmkh3lag.aug = eco.theilsen2(tcTminwa.aug,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagaug.slope <- raster("TCWaTminmmkh3lagaugslope.tif")
TCWaTminmmkh3lagaug.pvalue <- raster("TCWaTminmmkh3lagaugpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagaug.p = calc(TCWaTminmmkh3lagaug.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopeaug.sig = mask(TCWaTminmmkh3lagaug.slope, TCWaTminmmkh3lagaug.p)
writeRaster(TCwaTminmmkh3lagslopeaug.sig, 'TCwaTminmmkh3lagslopeaugsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopeaug.sig)
TCwaTminmmkh3lagslopeaug.sig =raster('TCwaTminmmkh3lagslopeaugsig.tif')

#Stack overall trend and significant aug trends for rainfall
TCwaTminmmkh3lagslopeaug.all.sig = stack(TCWaTminmmkh3lagaug.slope,TCwaTminmmkh3lagslopeaug.sig)
names(TCwaTminmmkh3lagslopeaug.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopeaug.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Sept
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.sep.maskras <- calc(tcTminwa.sep, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.sep.maskras)=0
tcTminwa.sep = mask(tcTminwa.sep, tcTminwa.sep.maskras)
tcTminwa.sep[tcTminwa.sep==0] = NA

TCWaTminmmkh3lag.sep = eco.theilsen2(tcTminwa.sep,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagsep.slope <- raster("TCWaTminmmkh3lagsepslope.tif")
TCWaTminmmkh3lagsep.pvalue <- raster("TCWaTminmmkh3lagseppvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagsep.p = calc(TCWaTminmmkh3lagsep.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopesep.sig = mask(TCWaTminmmkh3lagsep.slope, TCWaTminmmkh3lagsep.p)
writeRaster(TCwaTminmmkh3lagslopesep.sig, 'TCwaTminmmkh3lagslopesepsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopesep.sig)
TCwaTminmmkh3lagslopesep.sig =raster('TCwaTminmmkh3lagslopesepsig.tif')

#Stack overall trend and significant sep trends for rainfall
TCwaTminmmkh3lagslopesep.all.sig = stack(TCWaTminmmkh3lagsep.slope,TCwaTminmmkh3lagslopesep.sig)
names(TCwaTminmmkh3lagslopesep.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopesep.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Oct
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.oct.maskras <- calc(tcTminwa.oct, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.oct.maskras)=0
tcTminwa.oct = mask(tcTminwa.oct, tcTminwa.oct.maskras)
tcTminwa.oct[tcTminwa.oct==0] = NA

TCWaTminmmkh3lag.oct = eco.theilsen2(tcTminwa.oct,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagoct.slope <- raster("TCWaTminmmkh3lagoctslope.tif")
TCWaTminmmkh3lagoct.pvalue <- raster("TCWaTminmmkh3lagoctpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagoct.p = calc(TCWaTminmmkh3lagoct.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopeoct.sig = mask(TCWaTminmmkh3lagoct.slope, TCWaTminmmkh3lagoct.p)
writeRaster(TCwaTminmmkh3lagslopeoct.sig, 'TCwaTminmmkh3lagslopeoctsig.tif',format='GTiff', overwrite=T)
rm(TCwaTminmmkh3lagslopeoct.sig)
TCwaTminmmkh3lagslopeoct.sig =raster('TCwaTminmmkh3lagslopeoctsig.tif')

#Stack overall trend and significant oct trends for rainfall
TCwaTminmmkh3lagslopeoct.all.sig = stack(TCWaTminmmkh3lagoct.slope,TCwaTminmmkh3lagslopeoct.sig)
names(TCwaTminmmkh3lagslopeoct.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopeoct.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Nov
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.nov.maskras <- calc(tcTminwa.nov, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.nov.maskras)=0
tcTminwa.nov = mask(tcTminwa.nov, tcTminwa.nov.maskras)
tcTminwa.nov[tcTminwa.nov==0] = NA

TCWaTminmmkh3lag.nov = eco.theilsen2(tcTminwa.nov,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagnov.slope <- raster("TCWaTminmmkh3lagnovslope.tif")
TCWaTminmmkh3lagnov.pvalue <- raster("TCWaTminmmkh3lagnovpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagnov.p = calc(TCWaTminmmkh3lagnov.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopenov.sig = mask(TCWaTminmmkh3lagnov.slope, TCWaTminmmkh3lagnov.p)
writeRaster(TCwaTminmmkh3lagslopenov.sig, 'TCwaTminmmkh3lagslopenovsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopenov.sig)
TCwaTminmmkh3lagslopenov.sig =raster('TCwaTminmmkh3lagslopenovsig.tif')

#Stack overall trend and significant nov trends for rainfall
TCwaTminmmkh3lagslopenov.all.sig = stack(TCWaTminmmkh3lagnov.slope,TCwaTminmmkh3lagslopenov.sig)
names(TCwaTminmmkh3lagslopenov.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopenov.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###DEC
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.dec.maskras <- calc(tcTminwa.dec, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.dec.maskras)=0
tcTminwa.dec = mask(tcTminwa.dec, tcTminwa.dec.maskras)
tcTminwa.dec[tcTminwa.dec==0] = NA

TCWaTminmmkh3lag.dec = eco.theilsen2(tcTminwa.dec,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagdec.slope <- raster("TCWaTminmmkh3lagdecslope.tif")
TCWaTminmmkh3lagdec.pvalue <- raster("TCWaTminmmkh3lagdecpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagdec.p = calc(TCWaTminmmkh3lagdec.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopedec.sig = mask(TCWaTminmmkh3lagdec.slope, TCWaTminmmkh3lagdec.p)
writeRaster(TCwaTminmmkh3lagslopedec.sig, 'TCwaTminmmkh3lagslopedecsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopedec.sig)
TCwaTminmmkh3lagslopedec.sig =raster('TCwaTminmmkh3lagslopedecsig.tif')

#Stack overall trend and significant dec trends for rainfall
TCwaTminmmkh3lagslopedec.all.sig = stack(TCWaTminmmkh3lagdec.slope,TCwaTminmmkh3lagslopedec.sig)
names(TCwaTminmmkh3lagslopedec.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopedec.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Jan
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.jan.maskras <- calc(tcTminwa.jan, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.jan.maskras)=0
tcTminwa.jan = mask(tcTminwa.jan, tcTminwa.jan.maskras)
tcTminwa.jan[tcTminwa.jan==0] = NA

TCWaTminmmkh3lag.jan = eco.theilsen2(tcTminwa.jan,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagjan.slope <- raster("TCWaTminmmkh3lagjanslope.tif")
TCWaTminmmkh3lagjan.pvalue <- raster("TCWaTminmmkh3lagjanpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagjan.p = calc(TCWaTminmmkh3lagjan.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopejan.sig = mask(TCWaTminmmkh3lagjan.slope, TCWaTminmmkh3lagjan.p)
writeRaster(TCwaTminmmkh3lagslopejan.sig, 'TCwaTminmmkh3lagslopejansig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopejan.sig)
TCwaTminmmkh3lagslopejan.sig =raster('TCwaTminmmkh3lagslopejansig.tif')

#Stack overall trend and significant jan trends for rainfall
TCwaTminmmkh3lagslopejan.all.sig = stack(TCWaTminmmkh3lagjan.slope,TCwaTminmmkh3lagslopejan.sig)
names(TCwaTminmmkh3lagslopejan.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopejan.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Feb
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.feb.maskras <- calc(tcTminwa.feb, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.feb.maskras)=0
tcTminwa.feb = mask(tcTminwa.feb, tcTminwa.feb.maskras)
tcTminwa.feb[tcTminwa.feb==0] = NA

TCWaTminmmkh3lag.feb = eco.theilsen2(tcTminwa.feb,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagfeb.slope <- raster("TCWaTminmmkh3lagfebslope.tif")
TCWaTminmmkh3lagfeb.pvalue <- raster("TCWaTminmmkh3lagfebpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagfeb.p = calc(TCWaTminmmkh3lagfeb.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopefeb.sig = mask(TCWaTminmmkh3lagfeb.slope, TCWaTminmmkh3lagfeb.p)
writeRaster(TCwaTminmmkh3lagslopefeb.sig, 'TCwaTminmmkh3lagslopefebsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopefeb.sig)
TCwaTminmmkh3lagslopefeb.sig =raster('TCwaTminmmkh3lagslopefebsig.tif')

#Stack overall trend and significant feb trends for rainfall
TCwaTminmmkh3lagslopefeb.all.sig = stack(TCWaTminmmkh3lagfeb.slope,TCwaTminmmkh3lagslopefeb.sig)
names(TCwaTminmmkh3lagslopefeb.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopefeb.all.sig)+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###March
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcTminwa.mar.maskras <- calc(tcTminwa.mar, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcTminwa.mar.maskras)=0
tcTminwa.mar = mask(tcTminwa.mar, tcTminwa.mar.maskras)
tcTminwa.mar[tcTminwa.mar==0] = NA

TCWaTminmmkh3lag.mar = eco.theilsen2(tcTminwa.mar,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWaTminmmkh3lagmar.slope <- raster("TCWaTminmmkh3lagmarslope.tif")
TCWaTminmmkh3lagmar.pvalue <- raster("TCWaTminmmkh3lagmarpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWaTminmmkh3lagmar.p = calc(TCWaTminmmkh3lagmar.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwaTminmmkh3lagslopemar.sig = mask(TCWaTminmmkh3lagmar.slope, TCWaTminmmkh3lagmar.p)
writeRaster(TCwaTminmmkh3lagslopemar.sig, 'TCwaTminmmkh3lagslopemarsig.tif',format='GTiff')
rm(TCwaTminmmkh3lagslopemar.sig)
TCwaTminmmkh3lagslopemar.sig =raster('TCwaTminmmkh3lagslopemarsig.tif')

#Stack overall trend and significant mar trends for rainfall
TCwaTminmmkh3lagslopemar.all.sig = stack(TCWaTminmmkh3lagmar.slope,TCwaTminmmkh3lagslopemar.sig)
names(TCwaTminmmkh3lagslopemar.all.sig) = c('a','b')
levelplot(TCwaTminmmkh3lagslopemar.all.sig)+
  layer(sp.polygons(countries.wa.shp, border='black'))

#######################################
####Stack Monthly slopes####
TCTminmonthlyslopes = stack(TCWaTminmmkh3lagapr.slope,TCWaTminmmkh3lagmay.slope,TCWaTminmmkh3lagjun.slope,TCWaTminmmkh3lagjul.slope,TCWaTminmmkh3lagaug.slope,TCWaTminmmkh3lagsep.slope,TCWaTminmmkh3lagoct.slope,TCWaTminmmkh3lagnov.slope,TCWaTminmmkh3lagdec.slope,TCWaTminmmkh3lagjan.slope, TCWaTminmmkh3lagfeb.slope,TCWaTminmmkh3lagmar.slope)
writeRaster(TCTminmonthlyslopes, 'TCTminmonthlyslopes.tif', format='GTiff')
TCTminmonthlyslopes =stack('TCTminmonthlyslopes.tif')
  #names(TCTminmonthlyslopes)= c('Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar')
  names(TCTminmonthlyslopes) = c('April','May','June','July','August','September','October','November','December','January','February','March')
  


#myat.monthly.sigtrend.Tmin =seq(min(minValue(TCTminmonthlyslopes)),max(maxValue(TCTminmonthlyslopes)),length.out=12)
#myat.monthly.sigtrend.Tmin = round (myat.monthly.sigtrend.Tmin, digits = 3)
#myat.monthly.sigtrend.Tmin= c(-0.033,-0.025,-0.018,-0.011,-0.003,0.00,0.012,0.019,0.027,0.034,0.042,0.05)
#myat.monthly.sigtrend.Tmin=seq(-0.033,0.050, 0.01)
myat.monthly.sigtrend.Tmin= c(-0.033, -0.023, -0.013, -0.0015,  0.00,0.001,  0.013,  0.023,  0.033, 0.043, 0.05)
myat.monthly.sigtrend.Tmin.lab= c(-0.033, -0.023, -0.013, 0.00,  0.013,  0.023,  0.033, 0.043, 0.05)
levelplot(TCTminmonthlyslopes,at=myat.monthly.sigtrend.Tmin, par.settings=myTheme.ltmrev , colorkey=list( at=myat.monthly.sigtrend.Tmin,labels=list(cex=1,at=myat.monthly.sigtrend.Tmin), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

#Stack and plot monthly significant slopes
TCTminmonthlyslope.sig = stack(TCwaTminmmkh3lagslopeapr.sig,TCwaTminmmkh3lagslopemay.sig,TCwaTminmmkh3lagslopejun.sig,TCwaTminmmkh3lagslopejul.sig,TCwaTminmmkh3lagslopeaug.sig,TCwaTminmmkh3lagslopesep.sig,TCwaTminmmkh3lagslopeoct.sig,TCwaTminmmkh3lagslopenov.sig,TCwaTminmmkh3lagslopedec.sig,TCwaTminmmkh3lagslopejan.sig, TCwaTminmmkh3lagslopefeb.sig,TCwaTminmmkh3lagslopemar.sig)
writeRaster(TCTminmonthlyslope.sig, 'TCTminmonthlyslopesig.tif', format='GTiff')
TCTminmonthlyslope.sig =stack('TCTminmonthlyslopesig.tif')
names(TCTminmonthlyslope.sig) = c('April','May','June','July','August','September','October','November','December','January','February','March')

levelplot(TCTminmonthlyslope.sig,at=myat.monthly.sigtrend.Tmin, par.settings=myTheme.ltmrev, colorkey=list( at=myat.monthly.sigtrend.Tmin,labels=list(cex=1,at=myat.monthly.sigtrend.Tmin), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

############################################################################################################
####Plot Monthly slopes for Tmin with hatchedLayers####
############################################################################################################
##
####Convert significant slope raster to SPplolygons####
TCTmin.poly.sign = lapply(as.list(TCTminmonthlyslope.sig), rasterToPolygons, dissolve=T)

####Generate hatchedLayer for Theilsen slopes for each month####
#Apr
TCTmin.poly.sign.apr=TCTmin.poly.sign[[1]]
TCTmin.poly.sign.apr =sf::st_as_sf(TCTmin.poly.sign.apr)
TCTmin.poly.sign.apr$area <- (st_area(TCTmin.poly.sign.apr)/1000000)
TCTmin.poly.sign.dsl.apr = TCTmin.poly.sign.apr %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.aprhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.apr, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.aprhl.sp <- as(TCTmin.poly.sign.aprhl, "Spatial")

#May
TCTmin.poly.sign.May=TCTmin.poly.sign[[2]]
TCTmin.poly.sign.May =sf::st_as_sf(TCTmin.poly.sign.May)
TCTmin.poly.sign.May$area <- (st_area(TCTmin.poly.sign.May)/1000000)
TCTmin.poly.sign.dsl.May = TCTmin.poly.sign.May %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Mayhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.May, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Mayhl.sp <- as(TCTmin.poly.sign.Mayhl, "Spatial")

#Jun
TCTmin.poly.sign.Jun=TCTmin.poly.sign[[3]]
TCTmin.poly.sign.Jun =sf::st_as_sf(TCTmin.poly.sign.Jun)
TCTmin.poly.sign.Jun$area <- (st_area(TCTmin.poly.sign.Jun)/1000000)
TCTmin.poly.sign.dsl.Jun = TCTmin.poly.sign.Jun %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Junhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Jun, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Junhl.sp <- as(TCTmin.poly.sign.Junhl, "Spatial")

#Jul
TCTmin.poly.sign.Jul=TCTmin.poly.sign[[4]]
TCTmin.poly.sign.Jul =sf::st_as_sf(TCTmin.poly.sign.Jul)
TCTmin.poly.sign.Jul$area <- (st_area(TCTmin.poly.sign.Jul)/1000000)
TCTmin.poly.sign.dsl.Jul = TCTmin.poly.sign.Jul %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Julhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Jul, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Julhl.sp <- as(TCTmin.poly.sign.Julhl, "Spatial")

#Aug
TCTmin.poly.sign.Aug=TCTmin.poly.sign[[5]]
TCTmin.poly.sign.Aug =sf::st_as_sf(TCTmin.poly.sign.Aug)
TCTmin.poly.sign.Aug$area <- (st_area(TCTmin.poly.sign.Aug)/1000000)
TCTmin.poly.sign.dsl.Aug = TCTmin.poly.sign.Aug %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Aughl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Aug, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Aughl.sp <- as(TCTmin.poly.sign.Aughl, "Spatial")
#Sep
TCTmin.poly.sign.Sep=TCTmin.poly.sign[[6]]
TCTmin.poly.sign.Sep =sf::st_as_sf(TCTmin.poly.sign.Sep)
TCTmin.poly.sign.Sep$area <- (st_area(TCTmin.poly.sign.Sep)/1000000)
TCTmin.poly.sign.dsl.Sep = TCTmin.poly.sign.Sep %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Sephl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Sep, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Sephl.sp <- as(TCTmin.poly.sign.Sephl, "Spatial")
#Oct
TCTmin.poly.sign.Oct=TCTmin.poly.sign[[7]]
TCTmin.poly.sign.Oct =sf::st_as_sf(TCTmin.poly.sign.Oct)
TCTmin.poly.sign.Oct$area <- (st_area(TCTmin.poly.sign.Oct)/1000000)
TCTmin.poly.sign.dsl.Oct = TCTmin.poly.sign.Oct %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Octhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Oct, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Octhl.sp <- as(TCTmin.poly.sign.Octhl, "Spatial")

#Nov
TCTmin.poly.sign.Nov=TCTmin.poly.sign[[8]]
TCTmin.poly.sign.Nov =sf::st_as_sf(TCTmin.poly.sign.Nov)
TCTmin.poly.sign.Nov$area <- (st_area(TCTmin.poly.sign.Nov)/1000000)
TCTmin.poly.sign.dsl.Nov = TCTmin.poly.sign.Nov %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Novhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Nov, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Novhl.sp <- as(TCTmin.poly.sign.Novhl, "Spatial")
#Dec
TCTmin.poly.sign.Dec=TCTmin.poly.sign[[9]]
TCTmin.poly.sign.Dec =sf::st_as_sf(TCTmin.poly.sign.Dec)
TCTmin.poly.sign.Dec$area <- (st_area(TCTmin.poly.sign.Dec)/1000000)
TCTmin.poly.sign.dsl.Dec = TCTmin.poly.sign.Dec %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Dechl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Dec, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Dechl.sp <- as(TCTmin.poly.sign.Dechl, "Spatial")
#Jan
TCTmin.poly.sign.Jan=TCTmin.poly.sign[[10]]
TCTmin.poly.sign.Jan =sf::st_as_sf(TCTmin.poly.sign.Jan)
TCTmin.poly.sign.Jan$area <- (st_area(TCTmin.poly.sign.Jan)/1000000)
TCTmin.poly.sign.dsl.Jan = TCTmin.poly.sign.Jan %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Janhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Jan, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Janhl.sp <- as(TCTmin.poly.sign.Janhl, "Spatial")

#Feb
TCTmin.poly.sign.Feb=TCTmin.poly.sign[[11]]
TCTmin.poly.sign.Feb =sf::st_as_sf(TCTmin.poly.sign.Feb)
TCTmin.poly.sign.Feb$area <- (st_area(TCTmin.poly.sign.Feb)/1000000)
TCTmin.poly.sign.dsl.Feb = TCTmin.poly.sign.Feb %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Febhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Feb, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Febhl.sp <- as(TCTmin.poly.sign.Febhl, "Spatial")

#Mar
TCTmin.poly.sign.Mar=TCTmin.poly.sign[[12]]
TCTmin.poly.sign.Mar =sf::st_as_sf(TCTmin.poly.sign.Mar)
TCTmin.poly.sign.Mar$area <- (st_area(TCTmin.poly.sign.Mar)/1000000)
TCTmin.poly.sign.dsl.Mar = TCTmin.poly.sign.Mar %>% summarise(area = sum(area))
#Plot hatchLayer
TCTmin.poly.sign.Marhl =cartography::hatchedLayer(TCTmin.poly.sign.dsl.Mar, pattern = "left2right", lwd = 0.2, density = 2, mode = 'sfc')
# transform the sf object to an sp oject
TCTmin.poly.sign.Marhl.sp <- as(TCTmin.poly.sign.Marhl, "Spatial")

#List spatial lines hatchLayers for each month
TCTmin.sig.hatch = list(TCTmin.poly.sign.aprhl.sp,TCTmin.poly.sign.Mayhl.sp,TCTmin.poly.sign.Junhl.sp,TCTmin.poly.sign.Julhl.sp,TCTmin.poly.sign.Aughl.sp,TCTmin.poly.sign.Sephl.sp,TCTmin.poly.sign.Octhl.sp,TCTmin.poly.sign.Novhl.sp,TCTmin.poly.sign.Dechl.sp,TCTmin.poly.sign.Janhl.sp, TCTmin.poly.sign.Febhl.sp, TCTmin.poly.sign.Marhl.sp)

####Plot hatchedLayers for statistically sign area over the TheilSens slope for each month####

#levelplot(TCTminmonthlyslopes,at=myat.monthly.sigtrend.Tmin, par.settings=myTheme.ltm.BlRd , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend.Tmin,labels=list(cex=1,at=myat.monthly.sigtrend.Tmin.lab), height=0.9), margin = FALSE,xlab='', ylab='')+
  #latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))+
  #latticeExtra::layer(sp.lines(TCTmin.sig.hatch[[panel.number()]], col = 'black', lwd = 0.01, cex = 1, alpha=1))
index.numbering.Temp = c('(a)','(b)','(c)','(d)', '(e)','(f)','(g)','(h)','(i)','(j)','(k)','(l)')
txtColors <- c("black")


levelplot(TCTminmonthlyslopes,at=myat.monthly.sigtrend.Tmax, par.settings=myTheme.ltm.BlRd , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend.Tmax,labels=list(cex=1,at=myat.monthly.sigtrend.Tmax.lab), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))+
  latticeExtra::layer(sp.lines(TCTmin.sig.hatch[[panel.number()]], col = 'black', lwd = 0.01, cex = 1, alpha=1))+
  latticeExtra::layer(panel.text(3, 14.5,index.numbering.Temp[panel.number()],  col = txtColors, fontface= 'bold', cex=1.2))

############################################################################################################
####Analyze Tmin trends per AEZ####
############################################################################################################
#Import AEZ layer##

#aezWArst = raster('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/aezWA6rst.tif')
aezWArst.tc = projectRaster(aezWArst, TCWaTminmmkh3lagapr.slope, method = 'ngb')
#aezWArst.fun=function(x) { x[x>7.0001] <- NA; return(x)}
#aezWArst <- calc(aezWArst, aezWArst.fun)
#aezWArst.tc = round(aezWArst.tc)
aezWArst.tc = ratify(aezWArst.tc)
writeRaster(aezWArst.tc, format='GTiff', filename='aezWArstTC.tif', overwrite=T)
aezWArst.tc = raster('aezWArstTC.tif')

##Zonal mean of rainfall trends## 

#aez.wa =zonal(TCTminmonthlyslopes, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalTminsWA.csv")
aezTminsWASig =as.data.frame(zonal(TCTminmonthlyslope.sig, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalTminsWASig.csv"))
#aezTminsWASig= aezTminsWASig%>% drop_na(Jun) 
####Boxplot for CHIRPS trends per AEZ for all slopes####

par(mfrow = c(3, 4), mar = c(4, 4, 1, 1))

bxtrendTmin.Apr = boxplot(TCTminmonthlyslopes[[1]], aezWArst.tc, xlab= '', ylab='Trend (oC yr-1)', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Apr",side=3,line=0,at=3, cex=1)
bxtrendTmin.May = boxplot(TCTminmonthlyslopes[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("May",side=3,line=0,at=3, cex=1)
bxtrendTmin.Jun = boxplot(TCTminmonthlyslopes[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Jun",side=3,line=0,at=3, cex=1)
bxtrendTmin.Jul = boxplot(TCTminmonthlyslopes[[4]], aezWArst.tc,  xlab= '',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Jul",side=3,line=0,at=3, cex=1)
bxtrendTmin.Aug = boxplot(TCTminmonthlyslopes[[5]], aezWArst.tc,   xlab= '',ylab='Trend (oC yr-1)', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Aug",side=3,line=0,at=3, cex=1)
bxtrendTmin.Sept= boxplot(TCTminmonthlyslopes[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Sept",side=3,line=0,at=3, cex=1)
bxtrendTmin.Oct = boxplot(TCTminmonthlyslopes[[7]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Oct",side=3,line=0,at=3, cex=1)
bxtrendTmin.Nov = boxplot(TCTminmonthlyslopes[[8]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Nov",side=3,line=0,at=3, cex=1)
bxtrendTmin.Dec = boxplot(TCTminmonthlyslopes[[9]], aezWArst.tc,  xlab= 'Zone',ylab='Trend (oC yr-1)', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Dec",side=3,line=0,at=3, cex=1)
bxtrendTmin.Jan = boxplot(TCTminmonthlyslopes[[10]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Jan",side=3,line=0,at=3, cex=1)
bxtrendTmin.Feb = boxplot(TCTminmonthlyslopes[[11]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Feb",side=3,line=0,at=3, cex=1)
bxtrendTmin.Mar = boxplot(TCTminmonthlyslopes[[12]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.3)
mtext("Mar",side=3,line=0,at=3, cex=1)

##
####Boxplot for CHIRPS trends per AEZ for only significant trends####

par(mfrow = c(3, 4), mgp=c(2,0.5,0), mar = c(4, 4, 1, 1))

bxtrendTmin.Apr = boxplot(TCTminmonthlyslope.sig[[1]], aezWArst.tc, xlab= '', ylab=expression(Tmin~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("April",side=3,line=0,at=3, cex=1)
mtext("(a)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Apr, pch = 4, cex =2,col = "red")

bxtrendTmin.May = boxplot(TCTminmonthlyslope.sig[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("May",side=3,line=0,at=3, cex=1)
mtext("(b)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$May, pch = 4, cex =2,col = "red")

bxtrendTmin.Jun = boxplot(TCTminmonthlyslope.sig[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("June",side=3,line=0,at=3, cex=1)
mtext("(c)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Jun, pch = 4, cex =2,col = "red")

bxtrendTmin.Jul = boxplot(TCTminmonthlyslope.sig[[4]], aezWArst.tc,  xlab= '',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("July",side=3,line=0,at=3, cex=1)
mtext("(d)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Jul, pch = 4, cex =2,col = "red")

bxtrendTmin.Aug = boxplot(TCTminmonthlyslope.sig[[5]], aezWArst.tc,   xlab= '',ylab=expression(Tmin~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("August",side=3,line=0,at=3, cex=1)
mtext("(e)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Aug, pch = 4, cex =2,col = "red")

bxtrendTmin.Sept= boxplot(TCTminmonthlyslope.sig[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("September",side=3,line=0,at=3, cex=1)
mtext("(f)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Sept, pch = 4, cex =2,col = "red")

bxtrendTmin.Oct = boxplot(TCTminmonthlyslope.sig[[7]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("October",side=3,line=0,at=3, cex=1)
mtext("(g)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Oct, pch = 4, cex =2,col = "red")

bxtrendTmin.Nov = boxplot(TCTminmonthlyslope.sig[[8]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("November",side=3,line=0,at=3, cex=1)
mtext("(h)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Nov, pch = 4, cex =2,col = "red")

bxtrendTmin.Dec = boxplot(TCTminmonthlyslope.sig[[9]], aezWArst.tc,  xlab= 'Zone',ylab=expression(Tmin~(C^{o}~Month^{-1}~year^{-1})), ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("December",side=3,line=0,at=3, cex=1)
mtext("(i)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Dec, pch = 4, cex =2,col = "red")

bxtrendTmin.Jan = boxplot(TCTminmonthlyslope.sig[[10]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("January",side=3,line=0,at=3, cex=1)
mtext("(j)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Jan, pch = 4, cex =2,col = "red")

bxtrendTmin.Feb = boxplot(TCTminmonthlyslope.sig[[11]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("February",side=3,line=0,at=3, cex=1)
mtext("(k)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Feb, pch = 4, cex =2,col = "red")

bxtrendTmin.Mar = boxplot(TCTminmonthlyslope.sig[[12]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-0.04,0.05), cex.lab= 1.5)
mtext("March",side=3,line=0,at=3, cex=1)
mtext("(l)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
points(1:6, aezTminsWASig$Mar, pch = 4, cex =2,col = "red")

#####################################################################################################
###Identify priority sites for scaling climate smart technologies based on climatic trends####
####################################################################################################
##Reclass increasing or decreasing

TCpmmkh3lag.Tmin.rc <- function(x) { x[x<0] <- 1|x[x==0] <- 0|x[x>0] <- 2}
TCpmmkh3lag.Tmin.rc.rst = calc(TCpptmonthlyslope.sig, TCpmmkh3lag.Tmin.rc)

##
TCpmmkh3lag.Tmin.rc <- function(x) {
  ifelse( x < 0.00, 1,  ifelse( x > 0.00, 3, return(x)))
}

TCpmmkh3lag.Tmin.rc.rst = calc(TCpptmonthlyslope.sig, TCpmmkh3lag.Tmin.rc)

#Apply function to raster stack

r.class <- overlay(r, fun=rc)

TCpptmonthlyslope.sig.rc<-reclassify(TCpptmonthlyslope.sig, c(0,1,1, 1,2,2, 2,3,3, 3,4,4, 4,5,5, 5,6,6, 6,7,7, 7,8,8, 8,9,9, 9,10,10, 10,11,11, 11,12,12, 12,13,13, 13,14,14, 14,15,15,
                                                                            15,16,16, 16,17,17, 17,18,18, 18,19,19, 19,20,20), filename="FtF.FRDs.UTM.20.reclass", overwrite=T)

####Test for significance differences in trends among zones####

Tmin.wa.signValues = data.frame(cbind(values(TCTminmonthlyslope.sig), values(aezWArst.tc)))
write.csv(Tmin.wa.signValues, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TminWaSigdf.csv')
TminWasigdf= read.csv('TminWaSigdf.csv', header=T, sep=',')
#TminWasigdf= TminWasigdf %>% drop_na(AEZ)
TminWasigdf =TminWasigdf %>% dplyr::filter_at(.vars = vars(Apr, May,Jun, Jul,Aug, Sept,Oct, Nov, Dec,Jan, Feb, Mar), .vars_predicate = any_vars(!is.na(.)))
TminWasigdf = TminWasigdf %>% drop_na(AEZ)

#Convert wide to Long Df
TminWasigdf.long <- TminWasigdf %>% gather(Month, Trend, 2:13)
TminWasigdf.long$mergeID<-paste0(TminWasigdf.long$ID, TminWasigdf.long$Month,TminWasigdf.long$AEZ)
TminWasigdf.long$mergeID2<-paste0(TminWasigdf.long$Month, TminWasigdf.long$AEZ)
TminWasigdf.long$mergeID3<-paste0('Tmin-', TminWasigdf.long$mergeID2)
TminWasigdf.long <- within(TminWasigdf.long, {AEZ <- as.factor(AEZ)})
TminWasigdf.long <- within(TminWasigdf.long, {Month <- as.factor(Month)})
TminWasigdf.long <- within(TminWasigdf.long, {mergeID <- as.factor(mergeID)})
TminWasigdf.long <- within(TminWasigdf.long, {mergeID2 <- as.factor(mergeID2)})
TminWasigdf.long <- within(TminWasigdf.long, {mergeID3 <- as.factor(mergeID3)})
TminWasigdf.long = TminWasigdf.long %>% drop_na(Trend)
write.csv(TminWasigdf.long, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TminWasigdflong.csv')


##Numerical summaries
Tmin.aez.wa.Sign.sum = numSummary(TminWasigdf.long[,"Trend", drop=FALSE], groups=TminWasigdf.long$mergeID2, statistics=c("mean", "sd", "median"))
Tmin_sig_AEZ_summary = as.data.frame(read.csv('Tmin_sig_AEZ_summary.csv', header=T, sep=','))

#Sort by MergeID
Tmin_sig_AEZ_summary.sort= Tmin_sig_AEZ_summary %>%
  arrange (match(Month.Zone, c("Apr-02", "Apr-03", "Apr-04", "Apr-05", "Apr-06", "Apr-07", "May-02" ,"May-03", "May-04", "May-05", "May-06", "May-07","Jun-02", "Jun-03" ,"Jun-04", "Jun-05", "Jun-06", "Jun-07",
                               "Jul-02", "Jul-03", "Jul-04", "Jul-05", "Jul-06", "Jul-07","Aug-02", "Aug-03", "Aug-04", "Aug-05", "Aug-06" ,"Aug-07", "Sep-02", "Sep-03", "Sep-04", "Sep-05", "Sep-06", "Sep-07",
                               "Oct-02", "Oct-03", "Oct-04", "Oct-05","Oct-06","Oct-07","Nov-02", "Nov-03", "Nov-04", "Nov-05", "Nov-06", "Nov-07","Dec-02", "Dec-03", "Dec-04", "Dec-05", "Dec-06", "Dec-07" ,"Jan-02" ,"Jan-03", "Jan-04" ,"Jan-05" ,"Jan-06","Jan-07",
                               "Feb-02", "Feb-03", "Feb-04", "Feb-05", "Feb-06", "Feb-07", "Mar-02", "Mar-03", "Mar-04", "Mar-05", "Mar-06", "Mar-07")), desc(mean), desc(sd),desc(n))


#Merge Tmin+Tmin longDF

TminTminWasigdf.list=list(TminWasigdf, TminWasigdf.long)
TminWasigdf.merge <- merge_all(TminTminWasigdf.list)
write.table(rain.gauges.monthly.ALL.longDF, "E:/Francis_IITA/GIS_RS/GIS/Climatic/Rainfall/Gauge/rain.gauges.monthly.ALL.longDF.csv", sep=",", col.names=TRUE, row.names=TRUE, quote=TRUE, na="")



