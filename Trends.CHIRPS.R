##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## #####Determine Magnitude & significance trends for CHIRPS PPT
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Annual trends for 37 years time series
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
chirpswa.chirpsannual.maskras <- calc(chirps.wa.brick19812016.annual.37, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpswa.chirpsannual.maskras)=0
chirps.wa.brick19812016.annual.37 = mask(chirps.wa.brick19812016.annual.37, chirpswa.chirpsannual.maskras)
chirps.wa.brick19812016.annual.37[chirps.wa.brick19812016.annual.37==0] = NA

TerraclimWa.pptannual.mmkh3lag = eco.theilsen2(chirps.wa.brick19812016.annual.37,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpswachirpsannualmmkh3lagslope <- raster("chirpswaAnnual.mmkh3lag.slope.tif")
chirpswachirpsAnnualmmkh3lagpvalue <- raster("chirpswaAnnual.mmkh3lag.pvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpswachirpsAnnualmmkh3lagpvalue.p = calc(chirpswachirpsAnnualmmkh3lagpvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswachirpsannualmmkh3lagslope.sig = mask(chirpswachirpsannualmmkh3lagslope, chirpswachirpsAnnualmmkh3lagpvalue.p)
writeRaster(chirpswachirpsannualmmkh3lagslope.sig, 'chirpswachirpsannualmmkh3lagslopesig.tif',format='GTiff', overwrite=T)
rm(chirpswachirpsannualmmkh3lagslope.sig)
chirpswachirpsannualmmkh3lagslope.sig =raster('chirpswachirpsannualmmkh3lagslopesig.tif')

#Stack overall trend and significant annual trends for rainfall
chirpswachirpsannualmmkh3lagslope.all.sig = stack(chirpswachirpsannualmmkh3lagslope,chirpswachirpsannualmmkh3lagslope.sig)
names(chirpswachirpsannualmmkh3lagslope.all.sig) = c('a','b')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#My at
#myat.chirpsannual.sigtrend =seq(min(minValue(chirpswachirpsannualmmkh3lagslope.all.sig)),max(maxValue(chirpswachirpsannualmmkh3lagslope.all.sig)),length.out=10)
#myat.chirpsannual.sigtrend = round (myat.chirpsannual.sigtrend, digits = 0)
myat.chirpsannual.sigtrend= c(-7, -6,-5,-4, -3,-2, -1, 0,  1, 2, 3, 4, 5, 6, 7,  11)

levelplot(chirpswachirpsannualmmkh3lagslope.all.sig,at=myat.chirpsannual.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.chirpsannual.sigtrend,labels=list(cex=1,at=myat.chirpsannual.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

#######################################################################################################################
##Map magnitude & significance of monthly trends of CHIRPS PPT 
#####################################################################################################################
###Apr
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
chirpspptwa.apr.maskras <- calc(chirps.wa.apr, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.apr.maskras)=0
chirpspptwa.apr = mask(chirps.wa.apr, chirpspptwa.apr.maskras)
chirpspptwa.apr[chirpspptwa.apr==0] = NA

#chirpspptwa.apr=brick(chirpspptwa.apr)

chirpsWapptmmkh3lag.Apr = eco.theilsen2(chirpspptwa.apr,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagApr.slope <- raster("chirpsWapptmmkh3lagAprslope.tif")
chirpsWapptmmkh3lagApr.pvalue <- raster("chirpsWapptmmkh3lagAprpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagApr.p = calc(chirpsWapptmmkh3lagApr.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopeApr.sig = mask(chirpsWapptmmkh3lagApr.slope, chirpsWapptmmkh3lagApr.p)
writeRaster(chirpswammkh3lagslopeApr.sig, 'chirpswammkh3lagslopeAprsig.tif',format='GTiff')
rm(chirpswammkh3lagslopeApr.sig)
chirpswammkh3lagslopeApr.sig =raster('chirpswammkh3lagslopeAprsig.tif')

#Stack overall trend and significant Apr trends for rainfall
chirpswammkh3lagslopeApr.all.sig = stack(chirpsWapptmmkh3lagApr.slope,chirpswammkh3lagslopeApr.sig)
names(chirpswammkh3lagslopeApr.all.sig) = c('a','b')
#My at
#myat.Apr.sigtrend =seq(min(minValue(chirpswammkh3lagslopeApr.all.sig)),max(maxValue(chirpswammkh3lagslopeApr.all.sig)),length.out=10)
#myat.Apr.sigtrend = round (myat.Apr.sigtrend, digits = 1)
myat.Apr.sigtrend = seq(-2.5, 0.5,1)
#myat.Apr.sigtrend = c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5)


levelplot(chirpswammkh3lagslopeApr.all.sig,at=myat.Apr.sigtrend, par.settings=myTheme.ltm.zero , scales=list(alternating=3), colorkey=list( at=myat.Apr.sigtrend,labels=list(cex=1,at=myat.Apr.sigtrend), height=0.9), margin = FALSE,
          xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###May
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
chirpspptwa.may.maskras <- calc(chirps.wa.may, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.may.maskras)=0
chirpspptwa.may = mask(chirps.wa.may, chirpspptwa.may.maskras)
chirpspptwa.may[chirpspptwa.may==0] = NA

#chirpspptwa.may=brick(chirpspptwa.may)

chirpsWapptmmkh3lag.may = eco.theilsen2(chirpspptwa.may,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagmay.slope <- raster("chirpsWapptmmkh3lagmayslope.tif")
chirpsWapptmmkh3lagmay.pvalue <- raster("chirpsWapptmmkh3lagmaypvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagmay.p = calc(chirpsWapptmmkh3lagmay.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopemay.sig = mask(chirpsWapptmmkh3lagmay.slope, chirpsWapptmmkh3lagmay.p)
writeRaster(chirpswammkh3lagslopemay.sig, 'chirpswammkh3lagslopemaysig.tif',format='GTiff')
rm(chirpswammkh3lagslopemay.sig)
chirpswammkh3lagslopemay.sig =raster('chirpswammkh3lagslopemaysig.tif')

#Stack overall trend and significant may trends for rainfall
chirpswammkh3lagslopemay.all.sig = stack(chirpsWapptmmkh3lagmay.slope,chirpswammkh3lagslopemay.sig)
names(chirpswammkh3lagslopemay.all.sig) = c('a','b')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###June
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
chirpspptwa.jun.maskras <- calc(chirps.wa.jun, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.jun.maskras)=0
chirpspptwa.jun = mask(chirps.wa.jun, chirpspptwa.jun.maskras)
chirpspptwa.jun[chirpspptwa.jun==0] = NA

#chirpspptwa.jun=brick(chirpspptwa.jun)

chirpsWapptmmkh3lag.jun = eco.theilsen2(chirpspptwa.jun,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagjun.slope <- raster("chirpsWapptmmkh3lagjunslope.tif")
chirpsWapptmmkh3lagjun.pvalue <- raster("chirpsWapptmmkh3lagjunpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagjun.p = calc(chirpsWapptmmkh3lagjun.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopejun.sig = mask(chirpsWapptmmkh3lagjun.slope, chirpsWapptmmkh3lagjun.p)
writeRaster(chirpswammkh3lagslopejun.sig, 'chirpswammkh3lagslopejunsig.tif',format='GTiff')
rm(chirpswammkh3lagslopejun.sig)
chirpswammkh3lagslopejun.sig =raster('chirpswammkh3lagslopejunsig.tif')

#Stack overall trend and significant jun trends for rainfall
chirpswammkh3lagslopejun.all.sig = stack(chirpsWapptmmkh3lagjun.slope,chirpswammkh3lagslopejun.sig)
names(chirpswammkh3lagslopejun.all.sig) = c('a','b')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###July
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.jul.maskras <- calc(chirps.wa.jul, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.jul.maskras)=0
chirpspptwa.jul = mask(chirps.wa.jul, chirpspptwa.jul.maskras)
chirpspptwa.jul[chirpspptwa.jul==0] = NA

#chirpspptwa.jul=brick(chirpspptwa.jul)

chirpsWapptmmkh3lag.jul = eco.theilsen2(chirpspptwa.jul,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagjul.slope <- raster("chirpsWapptmmkh3lagjulslope.tif")
chirpsWapptmmkh3lagjul.pvalue <- raster("chirpsWapptmmkh3lagjulpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagjul.p = calc(chirpsWapptmmkh3lagjul.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopejul.sig = mask(chirpsWapptmmkh3lagjul.slope, chirpsWapptmmkh3lagjul.p)
writeRaster(chirpswammkh3lagslopejul.sig, 'chirpswammkh3lagslopejulsig.tif',format='GTiff')
rm(chirpswammkh3lagslopejul.sig)
chirpswammkh3lagslopejul.sig =raster('chirpswammkh3lagslopejulsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopejul.all.sig = stack(chirpsWapptmmkh3lagjul.slope,chirpswammkh3lagslopejul.sig)
names(chirpswammkh3lagslopejul.all.sig) = c('a','b')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Aug
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.aug.maskras <- calc(chirps.wa.aug, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.aug.maskras)=0
chirpspptwa.aug = mask(chirps.wa.aug, chirpspptwa.aug.maskras)
chirpspptwa.aug[chirpspptwa.aug==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.aug = eco.theilsen2(chirpspptwa.aug,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagaug.slope <- raster("chirpsWapptmmkh3lagaugslope.tif")
chirpsWapptmmkh3lagaug.pvalue <- raster("chirpsWapptmmkh3lagaugpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagaug.p = calc(chirpsWapptmmkh3lagaug.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopeaug.sig = mask(chirpsWapptmmkh3lagaug.slope, chirpsWapptmmkh3lagaug.p)
writeRaster(chirpswammkh3lagslopeaug.sig, 'chirpswammkh3lagslopeaugsig.tif',format='GTiff')
rm(chirpswammkh3lagslopeaug.sig)
chirpswammkh3lagslopeaug.sig =raster('chirpswammkh3lagslopeaugsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopeaug.all.sig = stack(chirpsWapptmmkh3lagaug.slope,chirpswammkh3lagslopeaug.sig)
names(chirpswammkh3lagslopeaug.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopeaug.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Sept
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.sep.maskras <- calc(chirps.wa.sep, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.sep.maskras)=0
chirpspptwa.sep = mask(chirps.wa.sep, chirpspptwa.sep.maskras)
chirpspptwa.sep[chirpspptwa.sep==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.sep = eco.theilsen2(chirpspptwa.sep,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagsep.slope <- raster("chirpsWapptmmkh3lagsepslope.tif")
chirpsWapptmmkh3lagsep.pvalue <- raster("chirpsWapptmmkh3lagseppvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagsep.p = calc(chirpsWapptmmkh3lagsep.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopesep.sig = mask(chirpsWapptmmkh3lagsep.slope, chirpsWapptmmkh3lagsep.p)
writeRaster(chirpswammkh3lagslopesep.sig, 'chirpswammkh3lagslopesepsig.tif',format='GTiff')
rm(chirpswammkh3lagslopesep.sig)
chirpswammkh3lagslopesep.sig =raster('chirpswammkh3lagslopesepsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopesep.all.sig = stack(chirpsWapptmmkh3lagsep.slope,chirpswammkh3lagslopesep.sig)
names(chirpswammkh3lagslopesep.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopesep.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Oct
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.oct.maskras <- calc(chirps.wa.oct, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.oct.maskras)=0
chirpspptwa.oct = mask(chirps.wa.oct, chirpspptwa.oct.maskras)
chirpspptwa.oct[chirpspptwa.oct==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.oct = eco.theilsen2(chirpspptwa.oct,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagoct.slope <- raster("chirpsWapptmmkh3lagoctslope.tif")
chirpsWapptmmkh3lagoct.pvalue <- raster("chirpsWapptmmkh3lagoctpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagoct.p = calc(chirpsWapptmmkh3lagoct.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopeoct.sig = mask(chirpsWapptmmkh3lagoct.slope, chirpsWapptmmkh3lagoct.p)
writeRaster(chirpswammkh3lagslopeoct.sig, 'chirpswammkh3lagslopeoctsig.tif',format='GTiff')
rm(chirpswammkh3lagslopeoct.sig)
chirpswammkh3lagslopeoct.sig =raster('chirpswammkh3lagslopeoctsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopeoct.all.sig = stack(chirpsWapptmmkh3lagoct.slope,chirpswammkh3lagslopeoct.sig)
names(chirpswammkh3lagslopeoct.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopeoct.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Nov
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.nov.maskras <- calc(chirps.wa.nov, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.nov.maskras)=0
chirpspptwa.nov = mask(chirps.wa.nov, chirpspptwa.nov.maskras)
chirpspptwa.nov[chirpspptwa.nov==0] = NA

#Sum of pixels not NA values
chirpspptwa.nov.noNA = sum(!is.na(chirpspptwa.nov))
chirpspptwa.nov.noNA4.fun= function(x) { x[x<4] <- NA; return(x)}

chirpspptwa.nov.noNA4 = calc(chirpspptwa.nov.noNA,chirpspptwa.nov.noNA4.fun )

#Mask pixel with < 4 layers !is.na
chirpspptwa.nov= mask(chirpspptwa.nov,chirpspptwa.nov.noNA4)
#Run modified MMK test
chirpsWapptmmkh3lag.nov = eco.theilsen2(chirpspptwa.nov,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagnov.slope <- raster("chirpsWapptmmkh3lagnovslope.tif")
chirpsWapptmmkh3lagnov.pvalue <- raster("chirpsWapptmmkh3lagnovpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagnov.p = calc(chirpsWapptmmkh3lagnov.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopenov.sig = mask(chirpsWapptmmkh3lagnov.slope, chirpsWapptmmkh3lagnov.p)
writeRaster(chirpswammkh3lagslopenov.sig, 'chirpswammkh3lagslopenovsig.tif',format='GTiff')
rm(chirpswammkh3lagslopenov.sig)
chirpswammkh3lagslopenov.sig =raster('chirpswammkh3lagslopenovsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopenov.all.sig = stack(chirpsWapptmmkh3lagnov.slope,chirpswammkh3lagslopenov.sig)
names(chirpswammkh3lagslopenov.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopenov.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###DEC
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.dec.maskras <- calc(chirps.wa.dec, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.dec.maskras)=0
chirpspptwa.dec = mask(chirps.wa.dec, chirpspptwa.dec.maskras)
chirpspptwa.dec[chirpspptwa.dec==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.dec = eco.theilsen2(chirpspptwa.dec,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagdec.slope <- raster("chirpsWapptmmkh3lagdecslope.tif")
chirpsWapptmmkh3lagdec.pvalue <- raster("chirpsWapptmmkh3lagdecpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagdec.p = calc(chirpsWapptmmkh3lagdec.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopedec.sig = mask(chirpsWapptmmkh3lagdec.slope, chirpsWapptmmkh3lagdec.p)
writeRaster(chirpswammkh3lagslopedec.sig, 'chirpswammkh3lagslopedecsig.tif',format='GTiff')
rm(chirpswammkh3lagslopedec.sig)
chirpswammkh3lagslopedec.sig =raster('chirpswammkh3lagslopedecsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopedec.all.sig = stack(chirpsWapptmmkh3lagdec.slope,chirpswammkh3lagslopedec.sig)
names(chirpswammkh3lagslopedec.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopedec.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Jan
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.jan.maskras <- calc(chirps.wa.jan, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.jan.maskras)=0
chirpspptwa.jan = mask(chirps.wa.jan, chirpspptwa.jan.maskras)
chirpspptwa.jan[chirpspptwa.jan==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.jan = eco.theilsen2(chirpspptwa.jan,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagjan.slope <- raster("chirpsWapptmmkh3lagjanslope.tif")
chirpsWapptmmkh3lagjan.pvalue <- raster("chirpsWapptmmkh3lagjanpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagjan.p = calc(chirpsWapptmmkh3lagjan.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopejan.sig = mask(chirpsWapptmmkh3lagjan.slope, chirpsWapptmmkh3lagjan.p)
writeRaster(chirpswammkh3lagslopejan.sig, 'chirpswammkh3lagslopejansig.tif',format='GTiff',overwrite=TRUE)
rm(chirpswammkh3lagslopejan.sig)
chirpswammkh3lagslopejan.sig =raster('chirpswammkh3lagslopejansig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopejan.all.sig = stack(chirpsWapptmmkh3lagjan.slope,chirpswammkh3lagslopejan.sig)
names(chirpswammkh3lagslopejan.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopejan.all.sig)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Feb
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.feb.maskras <- calc(chirps.wa.feb, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.feb.maskras)=0
chirpspptwa.feb = mask(chirps.wa.feb, chirpspptwa.feb.maskras)
chirpspptwa.feb[chirpspptwa.feb==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.feb = eco.theilsen2(chirpspptwa.feb,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagfeb.slope <- raster("chirpsWapptmmkh3lagfebslope.tif")
chirpsWapptmmkh3lagfeb.pvalue <- raster("chirpsWapptmmkh3lagfebpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagfeb.p = calc(chirpsWapptmmkh3lagfeb.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopefeb.sig = mask(chirpsWapptmmkh3lagfeb.slope, chirpsWapptmmkh3lagfeb.p)
writeRaster(chirpswammkh3lagslopefeb.sig, 'chirpswammkh3lagslopefebsig.tif',format='GTiff')
rm(chirpswammkh3lagslopefeb.sig)
chirpswammkh3lagslopefeb.sig =raster('chirpswammkh3lagslopefebsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopefeb.all.sig = stack(chirpsWapptmmkh3lagfeb.slope,chirpswammkh3lagslopefeb.sig)
names(chirpswammkh3lagslopefeb.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopefeb.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###March
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
chirpspptwa.mar.maskras <- calc(chirps.wa.mar, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(chirpspptwa.mar.maskras)=0
chirpspptwa.mar = mask(chirps.wa.mar, chirpspptwa.mar.maskras)
chirpspptwa.mar[chirpspptwa.mar==0] = NA

#Run modified MMK test
chirpsWapptmmkh3lag.mar = eco.theilsen2(chirpspptwa.mar,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

chirpsWapptmmkh3lagmar.slope <- raster("chirpsWapptmmkh3lagmarslope.tif")
chirpsWapptmmkh3lagmar.pvalue <- raster("chirpsWapptmmkh3lagmarpvalue.tif")

#Crop only pvalue below 0.1
chirpspmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
chirpsWapptmmkh3lagmar.p = calc(chirpsWapptmmkh3lagmar.pvalue, chirpspmmkh3lag.p.fun)

#Clip only significant slope
chirpswammkh3lagslopemar.sig = mask(chirpsWapptmmkh3lagmar.slope, chirpsWapptmmkh3lagmar.p)
writeRaster(chirpswammkh3lagslopemar.sig, 'chirpswammkh3lagslopemarsig.tif',format='GTiff')
rm(chirpswammkh3lagslopemar.sig)
chirpswammkh3lagslopemar.sig =raster('chirpswammkh3lagslopemarsig.tif')

#Stack overall trend and significant jul trends for rainfall
chirpswammkh3lagslopemar.all.sig = stack(chirpsWapptmmkh3lagmar.slope,chirpswammkh3lagslopemar.sig)
names(chirpswammkh3lagslopemar.all.sig) = c('a','b')
levelplot(chirpswammkh3lagslopemar.all.sig)


#######################################
#Stack Monthly slopes
chirpspptmonthlyslopes = stack(chirpsWapptmmkh3lagApr.slope,chirpsWapptmmkh3lagmay.slope,chirpsWapptmmkh3lagjun.slope,chirpsWapptmmkh3lagjul.slope,chirpsWapptmmkh3lagaug.slope,chirpsWapptmmkh3lagsep.slope,chirpsWapptmmkh3lagoct.slope,chirpsWapptmmkh3lagdec.slope,chirpsWapptmmkh3lagjan.slope)
chirpspptmonthlyslopes = projectRaster(chirpspptmonthlyslopes, TCTminmonthlyslopes, method = 'ngb')
writeRaster(chirpspptmonthlyslopes, 'chirpspptmonthlyslopes.tif', format='GTiff')
chirpspptmonthlyslopes = stack('chirpspptmonthlyslopes.tif')
#names(chirpspptmonthlyslopes)= c('Apr','May','Jun','Jul','Aug','Sept','Oct','Dec','Jan')
names(chirpspptmonthlyslopes)=c('April','May','June','July','August','September','October','December','January')

levelplot(chirpspptmonthlyslopes)

#myat.monthly.sigtrend =seq(min(minValue(chirpspptmonthlyslopes)),max(maxValue(chirpspptmonthlyslopes)),length.out=10)
#myat.monthly.sigtrend = round (myat.annual.sigtrend, digits = 0)
myat.monthly.sigtrend= c(-5,-4,-3, -2, -1, -0.5,-0.25,0,0.25, 0.5, 1, 2,3,4,5)
myat.monthly.sigtrend.lab= c(-5,-4,-3, -2, -1, -0.5,0, 0.5, 1, 2,3,4,5)
levelplot(chirpspptmonthlyslopes,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))


#Stack and plot monthly significant slopes
chirpspptmonthlyslope.sig = stack(chirpswammkh3lagslopeApr.sig,chirpswammkh3lagslopemay.sig,chirpswammkh3lagslopejun.sig,chirpswammkh3lagslopejul.sig,chirpswammkh3lagslopeaug.sig,chirpswammkh3lagslopesep.sig,chirpswammkh3lagslopeoct.sig,chirpswammkh3lagslopedec.sig,chirpswammkh3lagslopejan.sig)
chirpspptmonthlyslope.sig = projectRaster(chirpspptmonthlyslope.sig, TCTminmonthlyslopes, method = 'ngb')
writeRaster(chirpspptmonthlyslope.sig, 'chirpspptmonthlyslopesig.tif', format='GTiff')
chirpspptmonthlyslope.sig = stack('chirpspptmonthlyslopesig.tif')
#names(chirpspptmonthlyslope.sig) = c('Apr','May','Jun','Jul','Aug','Sept','Oct','Dec','Jan')
names(chirpspptmonthlyslope.sig)=c('April','May','June','July','August','September','October','December','January')
#project raster to TC 4 Km grids

levelplot(chirpspptmonthlyslope.sig,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero, scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))


####Convert significant slope raster to SPplolygons####
chirps.poly.sign = lapply(as.list(chirpspptmonthlyslope.sig), rasterToPolygons, dissolve=T)
#chirps.poly.sign.pnt = lapply(as.list(chirpspptmonthlyslope.sig), rasterToPoints, spatial=T)

####Generate hatchedLayer for Theilsen slopes for each month####
#Apr
chirps.poly.sign.apr=chirps.poly.sign[[1]]
chirps.poly.sign.apr =sf::st_as_sf(chirps.poly.sign.apr)
chirps.poly.sign.apr$area <- (st_area(chirps.poly.sign.apr)/1000000)
chirps.poly.sign.dsl.apr = chirps.poly.sign.apr %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.aprhl =cartography::hatchedLayer(chirps.poly.sign.dsl.apr, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.aprhl.sp <- as(chirps.poly.sign.aprhl, "Spatial")

#May
chirps.poly.sign.May=chirps.poly.sign[[2]]
chirps.poly.sign.May =sf::st_as_sf(chirps.poly.sign.May)
chirps.poly.sign.May$area <- (st_area(chirps.poly.sign.May)/1000000)
chirps.poly.sign.dsl.May = chirps.poly.sign.May %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Mayhl =cartography::hatchedLayer(chirps.poly.sign.dsl.May, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Mayhl.sp <- as(chirps.poly.sign.Mayhl, "Spatial")

#Jun
chirps.poly.sign.Jun=chirps.poly.sign[[3]]
chirps.poly.sign.Jun =sf::st_as_sf(chirps.poly.sign.Jun)
chirps.poly.sign.Jun$area <- (st_area(chirps.poly.sign.Jun)/1000000)
chirps.poly.sign.dsl.Jun = chirps.poly.sign.Jun %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Junhl =cartography::hatchedLayer(chirps.poly.sign.dsl.Jun, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Junhl.sp <- as(chirps.poly.sign.Junhl, "Spatial")

#Jul
chirps.poly.sign.Jul=chirps.poly.sign[[4]]
chirps.poly.sign.Jul =sf::st_as_sf(chirps.poly.sign.Jul)
chirps.poly.sign.Jul$area <- (st_area(chirps.poly.sign.Jul)/1000000)
chirps.poly.sign.dsl.Jul = chirps.poly.sign.Jul %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Julhl =cartography::hatchedLayer(chirps.poly.sign.dsl.Jul, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Julhl.sp <- as(chirps.poly.sign.Julhl, "Spatial")

#Aug
chirps.poly.sign.Aug=chirps.poly.sign[[5]]
chirps.poly.sign.Aug =sf::st_as_sf(chirps.poly.sign.Aug)
chirps.poly.sign.Aug$area <- (st_area(chirps.poly.sign.Aug)/1000000)
chirps.poly.sign.dsl.Aug = chirps.poly.sign.Aug %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Aughl =cartography::hatchedLayer(chirps.poly.sign.dsl.Aug, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Aughl.sp <- as(chirps.poly.sign.Aughl, "Spatial")
#Sep
chirps.poly.sign.Sep=chirps.poly.sign[[6]]
chirps.poly.sign.Sep =sf::st_as_sf(chirps.poly.sign.Sep)
chirps.poly.sign.Sep$area <- (st_area(chirps.poly.sign.Sep)/1000000)
chirps.poly.sign.dsl.Sep = chirps.poly.sign.Sep %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Sephl =cartography::hatchedLayer(chirps.poly.sign.dsl.Sep, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Sephl.sp <- as(chirps.poly.sign.Sephl, "Spatial")
#Oct
chirps.poly.sign.Oct=chirps.poly.sign[[7]]
chirps.poly.sign.Oct =sf::st_as_sf(chirps.poly.sign.Oct)
chirps.poly.sign.Oct$area <- (st_area(chirps.poly.sign.Oct)/1000000)
chirps.poly.sign.dsl.Oct = chirps.poly.sign.Oct %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Octhl =cartography::hatchedLayer(chirps.poly.sign.dsl.Oct, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Octhl.sp <- as(chirps.poly.sign.Octhl, "Spatial")
#Dec
chirps.poly.sign.Dec=chirps.poly.sign[[8]]
chirps.poly.sign.Dec =sf::st_as_sf(chirps.poly.sign.Dec)
chirps.poly.sign.Dec$area <- (st_area(chirps.poly.sign.Dec)/1000000)
chirps.poly.sign.dsl.Dec = chirps.poly.sign.Dec %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Dechl =cartography::hatchedLayer(chirps.poly.sign.dsl.Dec, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Dechl.sp <- as(chirps.poly.sign.Dechl, "Spatial")
#Jan
chirps.poly.sign.Jan=chirps.poly.sign[[9]]
chirps.poly.sign.Jan =sf::st_as_sf(chirps.poly.sign.Jan)
chirps.poly.sign.Jan$area <- (st_area(chirps.poly.sign.Jan)/1000000)
chirps.poly.sign.dsl.Jan = chirps.poly.sign.Jan %>% summarise(area = sum(area))
#Plot hatchLayer
chirps.poly.sign.Janhl =cartography::hatchedLayer(chirps.poly.sign.dsl.Jan, pattern = "left2right", lwd = 0.2, density = 3, mode = 'sfc')
# transform the sf object to an sp oject
chirps.poly.sign.Janhl.sp <- as(chirps.poly.sign.Janhl, "Spatial")


#List spatial lines hatchLayers for each month
chirps.sig.hatch = list(chirps.poly.sign.aprhl.sp,chirps.poly.sign.Mayhl.sp,chirps.poly.sign.Junhl.sp,chirps.poly.sign.Julhl.sp,chirps.poly.sign.Aughl.sp,chirps.poly.sign.Sephl.sp,chirps.poly.sign.Octhl.sp,chirps.poly.sign.Dechl.sp,chirps.poly.sign.Janhl.sp)


####Plot hatchedLayers for statistically sign area over the TheilSens slope for each month####
index.numbering = c('(a)','(b)','(c)','(d)', '(e)','(f)','(g)','(h)','(i)')
txtColors <- c("black")

levelplot(chirpspptmonthlyslopes,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend.lab), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))+
  latticeExtra::layer(sp.lines(chirps.sig.hatch[[panel.number()]], col = 'black', lwd = 0.01, cex = 1, alpha=0.8))+
  latticeExtra::layer(panel.text(3, 14.5,index.numbering[panel.number()],  col = txtColors, fontface= 'bold', cex=1.1))

#levelplot(chirpspptmonthlyslopes,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
 #latticeExtra::layer(sp.polygons(countries.wa.shp, border='black'))+
  #latticeExtra::layer(sp.points(chirps.poly.sign.pnt[[panel.number()]], pch = 21,  col= 'red',lwd= 0.5, cex = 0.2, alpha=0.05))
   #latticeExtra::layer(sp.polygons(chirps.poly.sign[[panel.number()]], col = "black", lwd = 0.2, cex = 0.5))


####Analyze Rainfall trends per AEZ####
#Import AEZ layer##
#aez.wa = shapefile('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/Aez_clipWA6.shp')
aezWArst = raster('E:/Francis_IITA/GIS_RS/GIS/Climatic/Agroecological Zones/AEZ_africa/aezWA6rst.tif')
aezWArst = projectRaster(aezWArst, chirpsWapptmmkh3lagApr.slope)
aezWArst.fun=function(x) { x[x>7.0001] <- NA; return(x)}
aezWArst <- calc(aezWArst, aezWArst.fun)
aezWArst = round(aezWArst)
writeRaster(aezWArst, format='GTiff', filename='aezWArst.tif', overwrite=T)
aezWArst = raster('aezWArst.tif')

aezWArst.tc = raster('aezWArstTC.tif')
##Zonal mean of rainfall trends## 

#chirps.aez.wa =zonal(chirpspptmonthlyslopes, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalchirpsWA.csv")
chirps.aez.wa.Sign =as.data.frame(zonal(chirpspptmonthlyslope.sig, aezWArst.tc, fun='mean', digits=0, na.rm=TRUE, filename="/zonalchirpsWAsign.csv"))


##Sort by MergeID
chirps.aez.wa.Sign.sum = as.data.frame(numSummary(chirps.wa.signdf.long[,"Trend", drop=FALSE], groups=chirps.wa.signdf.long$mergeID2, statistics=c("mean", "sd", "median")))
chirp_sig_AEZ_summary = as.data.frame(read.csv('chirp_sig_AEZ_summary.csv', header=T, sep=','))

chirp_sig_AEZ_summary.sort= chirp_sig_AEZ_summary %>%
  arrange (match(Month.Zone, c("Apr-02", "Apr-03", "Apr-04", "Apr-05", "Apr-06", "Apr-07", "May-02" ,"May-03", "May-04", "May-05", "May-06", "May-07","Jun-02", "Jun-03" ,"Jun-04", "Jun-05", "Jun-06", "Jun-07",
                                             "Jul-02", "Jul-03", "Jul-04", "Jul-05", "Jul-06", "Jul-07","Aug-02", "Aug-03", "Aug-04", "Aug-05", "Aug-06" ,"Aug-07", "Sep-02", "Sep-03", "Sep-04", "Sep-05", "Sep-06", "Sep-07",
                                             "Oct-02", "Oct-03", "Oct-04", "Oct-05","Oct-06", "Oct-07","Dec-02", "Dec-03", "Dec-04", "Dec-05", "Dec-06", "Dec-07" ,"Jan-02" ,"Jan-03", "Jan-04" ,"Jan-05" ,"Jan-06","Jan-07")), desc(mean), desc(sd),desc(n))

####Boxplot for CHIRPS trends per AEZ####


par(mfrow = c(3, 3), mar = c(4, 4, 1, 1))

bxtrendchirps.Apr = boxplot(chirpspptmonthlyslopes[[1]], aezWArst.tc, xlab= '', ylab='Trend (mm/year)', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Apr",side=3,line=0,at=3, cex=1)
bxtrendchirps.May = boxplot(chirpspptmonthlyslopes[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("May",side=3,line=0,at=3, cex=1)
bxtrendchirps.Jun = boxplot(chirpspptmonthlyslopes[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Jun",side=3,line=0,at=3, cex=1)
bxtrendchirps.Jul = boxplot(chirpspptmonthlyslopes[[4]], aezWArst.tc,  xlab= '',ylab='Trend (mm/year)', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Jul",side=3,line=0,at=3, cex=1)
bxtrendchirps.Aug = boxplot(chirpspptmonthlyslopes[[5]], aezWArst.tc,   xlab= '',ylab='', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Aug",side=3,line=0,at=3, cex=1)
bxtrendchirps.Sept= boxplot(chirpspptmonthlyslopes[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Sept",side=3,line=0,at=3, cex=1)
bxtrendchirps.Oct = boxplot(chirpspptmonthlyslopes[[7]], aezWArst.tc,  xlab= 'Zone', ylab='Trend (mm/year)', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Oct",side=3,line=0,at=3, cex=1)
bxtrendchirps.Dec = boxplot(chirpspptmonthlyslopes[[8]], aezWArst.tc,  xlab= 'Zone', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.3)
mtext("Dec",side=3,line=0,at=3, cex=1)
bxtrendchirps.Jan = boxplot(chirpspptmonthlyslopes[[9]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-2.5,4.5), cex.lab= 1)
mtext("Jan",side=3,line=0,at=3, cex=1)

####Boxplot for CHIRPS significant trends per AEZ####

par(mfrow = c(3, 3), mgp=c(2,0.5,0), mar = c(4, 4, 1, 1))

bxtrendchirps.Apr = boxplot(chirpspptmonthlyslope.sig[[1]], aezWArst.tc, xlab= '', ylab=expression(Rainfall~(mm~Month^{-1}~year^{-1})), ylim=c(-2.5,4.5), cex.lab= 1.5)
mtext("April",side=3,line=0,at=3, cex=1)
mtext("(a)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.1, pch = 4, cex =2,col = "red")
points(1:6, chirps.aez.wa.Sign$Apr, pch = 4, cex =2,col = "red")

bxtrendchirps.May = boxplot(chirpspptmonthlyslope.sig[[2]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.8)
mtext("May",side=3,line=0,at=3, cex=1)
mtext("(b)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.2, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$May, pch = 4, cex =2,col = "red")

bxtrendchirps.Jun = boxplot(chirpspptmonthlyslope.sig[[3]], aezWArst.tc,  xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.8)
mtext("June",side=3,line=0,at=3, cex=1)
mtext("(c)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.3, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Jun, pch = 4, cex =2,col = "red")

bxtrendchirps.Jul = boxplot(chirpspptmonthlyslope.sig[[4]], aezWArst.tc,  xlab= '',ylab=expression(Rainfall~(mm~Month^{-1}~year^{-1})), ylim=c(-2.5,4.5), cex.lab= 1.5)
mtext("July",side=3,line=0,at=3, cex=1)
mtext("(d)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.4, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Jul, pch = 4, cex =2,col = "red")

bxtrendchirps.Aug = boxplot(chirpspptmonthlyslope.sig[[5]], aezWArst.tc,   xlab= '',ylab='', ylim=c(-2.5,4.5), cex.lab=1.8)
mtext("August",side=3,line=0,at=3, cex=1)
mtext("(e)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.5, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Aug, pch = 4, cex =2,col = "red")

bxtrendchirps.Sept= boxplot(chirpspptmonthlyslope.sig[[6]], aezWArst.tc, xlab= '', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.8)
mtext("September",side=3,line=0,at=3, cex=1)
mtext("(f)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.6, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Sept, pch = 4, cex =2,col = "red")

bxtrendchirps.Oct = boxplot(chirpspptmonthlyslope.sig[[7]], aezWArst.tc,  xlab= 'Zone', ylab=expression(Rainfall~(mm~Month^{-1}~year^{-1})), ylim=c(-2.5,4.5), cex.lab= 1.5)
mtext("October",side=3,line=0,at=3, cex=1)
mtext("(g)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.7, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Oct, pch = 4, cex =2,col = "red")

bxtrendchirps.Dec = boxplot(chirpspptmonthlyslope.sig[[8]], aezWArst.tc,  xlab= 'Zone', ylab='', ylim=c(-2.5,4.5), cex.lab= 1.5)
mtext("December",side=3,line=0,at=3, cex=1)
mtext("(h)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.8, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Dec, pch = 4, cex =2,col = "red")

bxtrendchirps.Jan = boxplot(chirpspptmonthlyslope.sig[[9]], aezWArst.tc,  xlab= 'Zone',ylab='', ylim=c(-2.5,4.5), cex.lab= 1.5)
mtext("January",side=3,line=0,at=3, cex=1)
mtext("(i)",side=3,line=0,at=-0.8, cex=1)
abline(h=0, lty=2, lwd=1)
#points(1:6, chirps.aez.wa.Sign.df$chirpspptmonthlyslopesig.9, pch = 4, cex = 2,col = "red")
points(1:6, chirps.aez.wa.Sign$Jan, pch = 4, cex =2,col = "red")

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''##
#### Mean plots####
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''##
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

ggline(TmaxTminchirpsFulljoin.cool, x = "mergeID2.x", y = "Trend.x", 
       #add = c("mean_se", "jitter"), 
       order = c("Aug2", "Aug3",  "Sept2", "Sept3"),
       ylab = "Trend", xlab = "Month/AEZ")


####Reclass significant slopes into two -increasing(2) and decreasing(1)####
chirpsslopesig.fun=function(x) { x[x<0] <- 100; return(x)}
chirpsslopesig <- calc(chirpspptmonthlyslope.sig, chirpsslopesig.fun)

chirpsslopesig.fun2=function(x) { x[x<99] <- 200; return(x)}
chirpsslopesig2 <- calc(chirpsslopesig, chirpsslopesig.fun2)
chirpsslopesig2=ratify(chirpsslopesig2/100)

####Plot zonal stats for areas with increasing or decreasing rain####

####Convert wide to long df####

chirps.wa.signValues = data.frame(cbind(values(chirpspptmonthlyslope.sig), values(aezWArst.tc)))
chirps.wa.signdf= chirps.wa.signValues %>% drop_na(AEZ) 
write.csv(chirps.wa.signdf, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/chirps.wa.signdf.csv')
chirps.wa.signdf= read.csv('chirps.wa.signdf.csv', header=T, sep=',')
#chirps.wa.signdf.long =chirps.wa.signdf[rowSums(is.na(chirps.wa.signdf[2:10])) != 9, ]
#chirps.wa.signdf.long = chirps.wa.signdf[rowSums(is.na(chirps.wa.signdf[c("Apr", "May","Jun", "Jul","Aug", "Sept","Oct", "Dec","Jan")])) != 9, ]
chirps.wa.signdf.long =chirps.wa.signdf %>% dplyr::filter_at(.vars = vars(Apr, May,Jun, Jul,Aug, Sept,Oct, Dec,Jan), .vars_predicate = any_vars(!is.na(.)))


chirps.wa.signdf.long <- chirps.wa.signdf %>% gather(Month, Trend, 2:10)
chirps.wa.signdf.long$mergeID<-paste0(chirps.wa.signdf.long$ID, chirps.wa.signdf.long$Month,chirps.wa.signdf.long$AEZ)
chirps.wa.signdf.long$mergeID2<-paste0(chirps.wa.signdf.long$Month, chirps.wa.signdf.long$AEZ)
chirps.wa.signdf.long$mergeID3<-paste0('chirps-', chirps.wa.signdf.long$mergeID2)
chirps.wa.signdf.long <- within(chirps.wa.signdf.long, {AEZ <- as.factor(AEZ)})
chirps.wa.signdf.long <- within(chirps.wa.signdf.long, {Month <- as.factor(Month)})
chirps.wa.signdf.long <- within(chirps.wa.signdf.long, {mergeID <- as.factor(mergeID)})
chirps.wa.signdf.long <- within(chirps.wa.signdf.long, {mergeID2 <- as.factor(mergeID2)})
chirps.wa.signdf.long <- within(chirps.wa.signdf.long, {mergeID3 <- as.factor(mergeID3)})
chirps.wa.signdf.long = chirps.wa.signdf.long %>% drop_na(Trend) 
write.csv(chirps.wa.signdf.long, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/chirpswasigndflong.csv')
chirps.wa.signdf.long= read.csv('chirpswasigndflong.csv', header=T, sep=',')
