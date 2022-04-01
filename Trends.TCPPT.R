##################################################################################################################
##Calculate Rainfall trends from TerraClimate data
##################################################################################################################

##Activate parallel processing

no_cores <- detectCores() - 2  
registerDoParallel(cores=no_cores)  
clr <- makeCluster(no_cores, type="PSOCK")

####Make clusters for multi-core processing####
beginCluster(6, type='SOCK')
endCluster()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##Derive annual trends using modified MK statistic 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#####Magnitude & significance of annual trends

TerraclimWa.pptannual.mmkh3lag = eco.theilsen2(terraclimppt.annual.37yrs,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

#Save the Sen's slope and pvalue rasters generated from eco.theilsen2 function
TCwaannualmmkh3lagslope <- raster("Results/Climatic Trends/Annual/TCwaAnnual.mmkh3lag.slope.tif")
TCwaAnnualmmkh3lagpvalue <- raster("Results/Climatic Trends/Annual/TCwaAnnual.mmkh3lag.pvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCwaAnnualmmkh3lagpvalue.p = calc(TCwaAnnualmmkh3lagpvalue, TCpmmkh3lag.p.fun)

#Crop only significant slope
TCwaannualmmkh3lagslope.sig = mask(TCwaannualmmkh3lagslope, TCwaAnnualmmkh3lagpvalue.p)
writeRaster(TCwaannualmmkh3lagslope.sig, 'TCwaannualmmkh3lagslopesig.tif',format='GTiff')
rm(TCwaannualmmkh3lagslope.sig)
TCwaannualmmkh3lagslope.sig =raster('TCwaannualmmkh3lagslopesig.tif')

#Stack overall trend and significant annual rends for rainfall
TCwaannualmmkh3lagslope.all.sig = stack(TCwaannualmmkh3lagslope,TCwaannualmmkh3lagslope.sig)
names(TCwaannualmmkh3lagslope.all.sig) = c('a','b')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Plot overall trends and only significant trend
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########Color pallete

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

#RasterVis Themes

myTheme.ltm.zero <- rasterTheme(region = c(reds.rev, blues))
myTheme.ltm.zero2 <- rasterTheme(region = c(reds.rev,zeroCol,blues))
#myTheme.ltm.zero.beige <- rasterTheme(region = c(reds.rev,beige, zeroCol, blues))
myTheme.ltm<- rasterTheme(region = c(reds.rev, blues))
myTheme.ltmrev<- rasterTheme(region = c(rev(blues),reds))
myTheme.ltm.BlRd<- rasterTheme(region = c(rev(blues), (reds)))
myTheme.ltm.BlRd2<- rasterTheme(region = colorRamps::blue2red(12))
myTheme.ltm.BlRd2rev<- rasterTheme(region = rev(colorRamps::blue2red(12)))
myTheme.ltm.reds <- rasterTheme(region = reds)
myTheme.ltm.blues <- rasterTheme(region = c( blues))
myTheme.ltm.rdylbu <- rasterTheme(region = c(yelowGnBlueYl))
myTheme.ltm.grdvctopo<-rasterTheme(region = grdevices.matlablike)
myTheme.matlablike <- rasterTheme(region = colorRamps::matlab.like(12))
myTheme.matlablikerev <- rasterTheme(region = rev(colorRamps::matlab.like(12)))
myTheme.matlablike2 <- rasterTheme(region = c(matlablike2))
myTheme.matlablike2rev <- rasterTheme(region = c(rev(matlablike2)))
my.theme.gn2rd<-rasterTheme(region =Gn2Rd)
terrain<-rev(terrain.colors(12))

#My at
myat.annual.sigtrend =seq(min(minValue(TCwaannualmmkh3lagslope.all.sig)),max(maxValue(TCwaannualmmkh3lagslope.all.sig)),length.out=10)
myat.annual.sigtrend = round (myat.annual.sigtrend, digits = 0)
myat.annual.sigtrend= c(-8,-7,-6,-5,-4,-3, -2, -1, 0,  1, 2,3, 4, 5, 6, 7, 8)#customized breaks

levelplot(TCwaannualmmkh3lagslope.all.sig,at=myat.annual.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.annual.sigtrend,labels=list(cex=1,at=myat.annual.sigtrend), height=0.9), margin = FALSE,
            xlab='', ylab='')+
    layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

#######################################################################################################################
##Map magnitude & significance of monthly trends of PPT 
#####################################################################################################################
###Apr
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcpptwa.apr.maskras <- calc(tcpptwa.apr, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.apr.maskras)=0
tcpptwa.apr = mask(tcpptwa.apr, tcpptwa.apr.maskras)
tcpptwa.apr[tcpptwa.apr==0] = NA
tcpptwa.apr=brick(tcpptwa.apr)

TCWapptmmkh3lag.Apr = eco.theilsen2(tcpptwa.apr,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagApr.slope <- raster("TCWapptmmkh3lagAprslope.tif")
TCWapptmmkh3lagApr.pvalue <- raster("TCWapptmmkh3lagAprpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagApr.p = calc(TCWapptmmkh3lagApr.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopeApr.sig = mask(TCWapptmmkh3lagApr.slope, TCWapptmmkh3lagApr.p)
writeRaster(TCwammkh3lagslopeApr.sig, 'TCwammkh3lagslopeAprsig.tif',format='GTiff')
rm(TCwammkh3lagslopeApr.sig)
TCwammkh3lagslopeApr.sig =raster('TCwammkh3lagslopeAprsig.tif')

#Stack overall trend and significant Apr trends for rainfall
TCwammkh3lagslopeApr.all.sig = stack(TCWapptmmkh3lagApr.slope,TCwammkh3lagslopeApr.sig)
names(TCwammkh3lagslopeApr.all.sig) = c('a','b')

#My at
#myat.Apr.sigtrend =seq(min(minValue(TCwammkh3lagslopeApr.all.sig)),max(maxValue(TCwammkh3lagslopeApr.all.sig)),length.out=10)
#myat.Apr.sigtrend = round (myat.Apr.sigtrend, digits = 1)
#myat.Apr.sigtrend = c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5)
myat.Apr.sigtrend = seq(-2.5, 0.5,1)


levelplot(TCwammkh3lagslopeApr.all.sig,at=myat.Apr.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.Apr.sigtrend,labels=list(cex=1,at=myat.Apr.sigtrend), height=0.9), margin = FALSE,
          xlab='', ylab='')+
  layer(sp.polygons(countries.wa.shp, border='black'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###May
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcpptwa.may.maskras <- calc(tcpptwa.may, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.may.maskras)=0
tcpptwa.may = mask(tcpptwa.may, tcpptwa.may.maskras)
tcpptwa.may[tcpptwa.may==0] = NA

#tcpptwa.may=brick(tcpptwa.may)

TCWapptmmkh3lag.may = eco.theilsen2(tcpptwa.may,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagmay.slope <- raster("TCWapptmmkh3lagmayslope.tif")
TCWapptmmkh3lagmay.pvalue <- raster("TCWapptmmkh3lagmaypvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagmay.p = calc(TCWapptmmkh3lagmay.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopemay.sig = mask(TCWapptmmkh3lagmay.slope, TCWapptmmkh3lagmay.p)
writeRaster(TCwammkh3lagslopemay.sig, 'TCwammkh3lagslopemaysig.tif',format='GTiff')
rm(TCwammkh3lagslopemay.sig)
TCwammkh3lagslopemay.sig =raster('TCwammkh3lagslopemaysig.tif')

#Stack overall trend and significant may trends for rainfall
TCwammkh3lagslopemay.all.sig = stack(TCWapptmmkh3lagmay.slope,TCwammkh3lagslopemay.sig)
names(TCwammkh3lagslopemay.all.sig) = c('a','b')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###June
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
tcpptwa.jun.maskras <- calc(tcpptwa.jun, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.jun.maskras)=0
tcpptwa.jun = mask(tcpptwa.jun, tcpptwa.jun.maskras)
tcpptwa.jun[tcpptwa.jun==0] = NA

#tcpptwa.jun=brick(tcpptwa.jun)

TCWapptmmkh3lag.jun = eco.theilsen2(tcpptwa.jun,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagjun.slope <- raster("TCWapptmmkh3lagjunslope.tif")
TCWapptmmkh3lagjun.pvalue <- raster("TCWapptmmkh3lagjunpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagjun.p = calc(TCWapptmmkh3lagjun.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopejun.sig = mask(TCWapptmmkh3lagjun.slope, TCWapptmmkh3lagjun.p)
writeRaster(TCwammkh3lagslopejun.sig, 'TCwammkh3lagslopejunsig.tif',format='GTiff')
rm(TCwammkh3lagslopejun.sig)
TCwammkh3lagslopejun.sig =raster('TCwammkh3lagslopejunsig.tif')

#Stack overall trend and significant jun trends for rainfall
TCwammkh3lagslopejun.all.sig = stack(TCWapptmmkh3lagjun.slope,TCwammkh3lagslopejun.sig)
names(TCwammkh3lagslopejun.all.sig) = c('a','b')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###July
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.jul.maskras <- calc(tcpptwa.jul, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.jul.maskras)=0
tcpptwa.jul = mask(tcpptwa.jul, tcpptwa.jul.maskras)
tcpptwa.jul[tcpptwa.jul==0] = NA

#tcpptwa.jul=brick(tcpptwa.jul)

TCWapptmmkh3lag.jul = eco.theilsen2(tcpptwa.jul,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagjul.slope <- raster("TCWapptmmkh3lagjulslope.tif")
TCWapptmmkh3lagjul.pvalue <- raster("TCWapptmmkh3lagjulpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagjul.p = calc(TCWapptmmkh3lagjul.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopejul.sig = mask(TCWapptmmkh3lagjul.slope, TCWapptmmkh3lagjul.p)
writeRaster(TCwammkh3lagslopejul.sig, 'TCwammkh3lagslopejulsig.tif',format='GTiff')
rm(TCwammkh3lagslopejul.sig)
TCwammkh3lagslopejul.sig =raster('TCwammkh3lagslopejulsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopejul.all.sig = stack(TCWapptmmkh3lagjul.slope,TCwammkh3lagslopejul.sig)
names(TCwammkh3lagslopejul.all.sig) = c('a','b')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Aug
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.aug.maskras <- calc(tcpptwa.aug, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.aug.maskras)=0
tcpptwa.aug = mask(tcpptwa.aug, tcpptwa.aug.maskras)
tcpptwa.aug[tcpptwa.aug==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.aug = eco.theilsen2(tcpptwa.aug,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagaug.slope <- raster("TCWapptmmkh3lagaugslope.tif")
TCWapptmmkh3lagaug.pvalue <- raster("TCWapptmmkh3lagaugpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagaug.p = calc(TCWapptmmkh3lagaug.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopeaug.sig = mask(TCWapptmmkh3lagaug.slope, TCWapptmmkh3lagaug.p)
writeRaster(TCwammkh3lagslopeaug.sig, 'TCwammkh3lagslopeaugsig.tif',format='GTiff')
rm(TCwammkh3lagslopeaug.sig)
TCwammkh3lagslopeaug.sig =raster('TCwammkh3lagslopeaugsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopeaug.all.sig = stack(TCWapptmmkh3lagaug.slope,TCwammkh3lagslopeaug.sig)
names(TCwammkh3lagslopeaug.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopeaug.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Sept
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.sep.maskras <- calc(tcpptwa.sep, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.sep.maskras)=0
tcpptwa.sep = mask(tcpptwa.sep, tcpptwa.sep.maskras)
tcpptwa.sep[tcpptwa.sep==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.sep = eco.theilsen2(tcpptwa.sep,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagsep.slope <- raster("TCWapptmmkh3lagsepslope.tif")
TCWapptmmkh3lagsep.pvalue <- raster("TCWapptmmkh3lagseppvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagsep.p = calc(TCWapptmmkh3lagsep.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopesep.sig = mask(TCWapptmmkh3lagsep.slope, TCWapptmmkh3lagsep.p)
writeRaster(TCwammkh3lagslopesep.sig, 'TCwammkh3lagslopesepsig.tif',format='GTiff')
rm(TCwammkh3lagslopesep.sig)
TCwammkh3lagslopesep.sig =raster('TCwammkh3lagslopesepsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopesep.all.sig = stack(TCWapptmmkh3lagsep.slope,TCwammkh3lagslopesep.sig)
names(TCwammkh3lagslopesep.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopesep.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Oct
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.oct.maskras <- calc(tcpptwa.oct, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.oct.maskras)=0
tcpptwa.oct = mask(tcpptwa.oct, tcpptwa.oct.maskras)
tcpptwa.oct[tcpptwa.oct==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.oct = eco.theilsen2(tcpptwa.oct,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagoct.slope <- raster("TCWapptmmkh3lagoctslope.tif")
TCWapptmmkh3lagoct.pvalue <- raster("TCWapptmmkh3lagoctpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagoct.p = calc(TCWapptmmkh3lagoct.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopeoct.sig = mask(TCWapptmmkh3lagoct.slope, TCWapptmmkh3lagoct.p)
writeRaster(TCwammkh3lagslopeoct.sig, 'TCwammkh3lagslopeoctsig.tif',format='GTiff')
rm(TCwammkh3lagslopeoct.sig)
TCwammkh3lagslopeoct.sig =raster('TCwammkh3lagslopeoctsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopeoct.all.sig = stack(TCWapptmmkh3lagoct.slope,TCwammkh3lagslopeoct.sig)
names(TCwammkh3lagslopeoct.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopeoct.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Nov
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.nov.maskras <- calc(tcpptwa.nov, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.nov.maskras)=0
tcpptwa.nov = mask(tcpptwa.nov, tcpptwa.nov.maskras)
tcpptwa.nov[tcpptwa.nov==0] = NA

#Sum of pixels not NA values
tcpptwa.nov.noNA = sum(!is.na(tcpptwa.nov))
tcpptwa.nov.noNA4.fun= function(x) { x[x<4] <- NA; return(x)}

tcpptwa.nov.noNA4 = calc(tcpptwa.nov.noNA,tcpptwa.nov.noNA4.fun )

#Mask pixel with < 4 layers !is.na
tcpptwa.nov= mask(tcpptwa.nov,tcpptwa.nov.noNA4)
#Run modified MMK test
TCWapptmmkh3lag.nov = eco.theilsen2(tcpptwa.nov,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagnov.slope <- raster("TCWapptmmkh3lagnovslope.tif")
TCWapptmmkh3lagnov.pvalue <- raster("TCWapptmmkh3lagnovpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagnov.p = calc(TCWapptmmkh3lagnov.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopenov.sig = mask(TCWapptmmkh3lagnov.slope, TCWapptmmkh3lagnov.p)
writeRaster(TCwammkh3lagslopenov.sig, 'TCwammkh3lagslopenovsig.tif',format='GTiff')
rm(TCwammkh3lagslopenov.sig)
TCwammkh3lagslopenov.sig =raster('TCwammkh3lagslopenovsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopenov.all.sig = stack(TCWapptmmkh3lagnov.slope,TCwammkh3lagslopenov.sig)
names(TCwammkh3lagslopenov.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopeonov.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###DEC
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.nov.maskras <- calc(tcpptwa.dec, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.dec.maskras)=0
tcpptwa.dec = mask(tcpptwa.dec, tcpptwa.dec.maskras)
tcpptwa.dec[tcpptwa.dec==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.dec = eco.theilsen2(tcpptwa.dec,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagdec.slope <- raster("TCWapptmmkh3lagdecslope.tif")
TCWapptmmkh3lagdec.pvalue <- raster("TCWapptmmkh3lagdecpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagdec.p = calc(TCWapptmmkh3lagdec.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopedec.sig = mask(TCWapptmmkh3lagdec.slope, TCWapptmmkh3lagdec.p)
writeRaster(TCwammkh3lagslopedec.sig, 'TCwammkh3lagslopedecsig.tif',format='GTiff')
rm(TCwammkh3lagslopedec.sig)
TCwammkh3lagslopedec.sig =raster('TCwammkh3lagslopedecsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopedec.all.sig = stack(TCWapptmmkh3lagdec.slope,TCwammkh3lagslopedec.sig)
names(TCwammkh3lagslopedec.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopedec.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Jan
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.jan.maskras <- calc(tcpptwa.jan, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.jan.maskras)=0
tcpptwa.jan = mask(tcpptwa.jan, tcpptwa.jan.maskras)
tcpptwa.jan[tcpptwa.jan==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.jan = eco.theilsen2(tcpptwa.jan,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagjan.slope <- raster("TCWapptmmkh3lagjanslope.tif")
TCWapptmmkh3lagjan.pvalue <- raster("TCWapptmmkh3lagjanpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagjan.p = calc(TCWapptmmkh3lagjan.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopejan.sig = mask(TCWapptmmkh3lagjan.slope, TCWapptmmkh3lagjan.p)
writeRaster(TCwammkh3lagslopejan.sig, 'TCwammkh3lagslopejansig.tif',format='GTiff')
rm(TCwammkh3lagslopejan.sig)
TCwammkh3lagslopejan.sig =raster('TCwammkh3lagslopejansig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopejan.all.sig = stack(TCWapptmmkh3lagjan.slope,TCwammkh3lagslopejan.sig)
names(TCwammkh3lagslopejan.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopejan.all.sig)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###Feb
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.feb.maskras <- calc(tcpptwa.feb, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.feb.maskras)=0
tcpptwa.feb = mask(tcpptwa.feb, tcpptwa.feb.maskras)
tcpptwa.feb[tcpptwa.feb==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.feb = eco.theilsen2(tcpptwa.feb,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagfeb.slope <- raster("TCWapptmmkh3lagfebslope.tif")
TCWapptmmkh3lagfeb.pvalue <- raster("TCWapptmmkh3lagfebpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagfeb.p = calc(TCWapptmmkh3lagfeb.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopefeb.sig = mask(TCWapptmmkh3lagfeb.slope, TCWapptmmkh3lagfeb.p)
writeRaster(TCwammkh3lagslopefeb.sig, 'TCwammkh3lagslopefebsig.tif',format='GTiff')
rm(TCwammkh3lagslopefeb.sig)
TCwammkh3lagslopefeb.sig =raster('TCwammkh3lagslopefebsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopefeb.all.sig = stack(TCWapptmmkh3lagfeb.slope,TCwammkh3lagslopefeb.sig)
names(TCwammkh3lagslopefeb.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopefeb.all.sig)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
###March
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Mask pixels with NA values for all rasters in time series
tcpptwa.mar.maskras <- calc(tcpptwa.mar, fun = sum, na.rm = TRUE) # all NA have zero value
NAvalue(tcpptwa.mar.maskras)=0
tcpptwa.mar = mask(tcpptwa.mar, tcpptwa.mar.maskras)
tcpptwa.mar[tcpptwa.mar==0] = NA

#Run modified MMK test
TCWapptmmkh3lag.mar = eco.theilsen2(tcpptwa.mar,  method = "mk_corrected", my_modified = modifiedmk::mmkh3lag)

TCWapptmmkh3lagmar.slope <- raster("TCWapptmmkh3lagmarslope.tif")
TCWapptmmkh3lagmar.pvalue <- raster("TCWapptmmkh3lagmarpvalue.tif")

#Crop only pvalue below 0.1
TCpmmkh3lag.p.fun <- function(x) { x[x>0.1] <- NA; return(x)}
TCWapptmmkh3lagmar.p = calc(TCWapptmmkh3lagmar.pvalue, TCpmmkh3lag.p.fun)

#Clip only significant slope
TCwammkh3lagslopemar.sig = mask(TCWapptmmkh3lagmar.slope, TCWapptmmkh3lagmar.p)
writeRaster(TCwammkh3lagslopemar.sig, 'TCwammkh3lagslopemarsig.tif',format='GTiff')
rm(TCwammkh3lagslopemar.sig)
TCwammkh3lagslopemar.sig =raster('TCwammkh3lagslopemarsig.tif')

#Stack overall trend and significant jul trends for rainfall
TCwammkh3lagslopemar.all.sig = stack(TCWapptmmkh3lagmar.slope,TCwammkh3lagslopemar.sig)
names(TCwammkh3lagslopemar.all.sig) = c('a','b')
levelplot(TCwammkh3lagslopeomar.all.sig)


#######################################
#Stack Monthly slopes
TCpptmonthlyslopes = stack(TCWapptmmkh3lagApr.slope,TCWapptmmkh3lagmay.slope,TCWapptmmkh3lagjun.slope,TCWapptmmkh3lagjul.slope,TCWapptmmkh3lagaug.slope,TCWapptmmkh3lagsep.slope,TCWapptmmkh3lagoct.slope,TCWapptmmkh3lagdec.slope,TCWapptmmkh3lagjan.slope)
names(TCpptmonthlyslopes)= c('Apr','May','Jun','Jul','Aug','Sept','Oct','Dec','Jan')
levelplot(TCpptmonthlyslopes)

#myat.monthly.sigtrend =seq(min(minValue(TCpptmonthlyslopes)),max(maxValue(TCpptmonthlyslopes)),length.out=10)
#myat.monthly.sigtrend = round (myat.annual.sigtrend, digits = 0)
myat.monthly.sigtrend= c(-5,-4,-3, -2, -1, -0.5,-0.25,0, 0.25, 0.5, 1, 2,3)
levelplot(TCpptmonthlyslopes,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
  layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))

#Stack and plot monthly significant slopes
TCpptmonthlyslope.sig = stack(TCwammkh3lagslopeApr.sig,TCwammkh3lagslopemay.sig,TCwammkh3lagslopejun.sig,TCwammkh3lagslopejul.sig,TCwammkh3lagslopeaug.sig,TCwammkh3lagslopesep.sig,TCwammkh3lagslopeoct.sig,TCwammkh3lagslopedec.sig,TCwammkh3lagslopejan.sig)
names(TCpptmonthlyslope.sig) = c('Apr','May','Jun','Jul','Aug','Sept','Oct','Dec','Jan')

levelplot(TCpptmonthlyslope.sig,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend), height=0.9), margin = FALSE,xlab='', ylab='')+
  layer(sp.polygons(countries.wa.shp, border='black'))#+
#layer(sp.points(demo.mali.ghana, col='purple'))


