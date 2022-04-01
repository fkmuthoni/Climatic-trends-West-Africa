##Load rasters
require(raster)
require (shapefiles)

##Import shapefiles
Africagadm0=
GHgadm0=
GH.gadm1=getData('GADM', country='GHA', level=1)
write.shp(GH.gadm1, 'E:/Francis_IITA/GIS_RS/GIS/Admin_political/Ghana/GADM-GH_1.shp')

##Compute LTM rainfall Ghana
  
chirps.ltm.GH=   
  
#List spatial lines hatchLayers for each month
chirps.sig.hatch = list(chirps.poly.sign.aprhl.sp,chirps.poly.sign.Mayhl.sp,chirps.poly.sign.Junhl.sp,chirps.poly.sign.Julhl.sp,chirps.poly.sign.Aughl.sp,chirps.poly.sign.Sephl.sp,chirps.poly.sign.Octhl.sp,chirps.poly.sign.Dechl.sp,chirps.poly.sign.Janhl.sp)

#Crop all layers to Northern Ghana

chirps.sig.hatch.NG=crop(chirps.sig.hatch, ghana.regions.AR)
countries.wa.shp.NG= crop(countries.wa.shp, ghana.regions.AR)
chirpspptmonthlyslopes.NG= crop(chirpspptmonthlyslopes, ghana.regions.AR)
bbox.NG= st_bbox(ghana.regions.AR)

####Plot hatchedLayers for statistically sign area over the TheilSens slope for each month of CHIRPS rainfall####
index.numbering = c('(a)','(b)','(c)','(d)', '(e)','(f)','(g)','(h)','(i)')
txtColors <- c("black")

levelplot(chirpspptmonthlyslopes.NG,at=myat.monthly.sigtrend, par.settings=myTheme.ltm.zero , scales=list(alternating=3), colorkey=list( at=myat.monthly.sigtrend,labels=list(cex=1,at=myat.monthly.sigtrend.lab), height=0.9), margin = FALSE,xlab='', ylab='')+
  latticeExtra::layer(sp.polygons(countries.wa.shp, border='black', lwd=2))+
  latticeExtra::layer(sp.polygons(ghana.regions.AR, border='black', lwd=0.5))+
  latticeExtra::layer(sp.lines(chirps.sig.hatch[[panel.number()]], col = 'black', lwd = 0.01, cex = 1, alpha=0.8))+
  latticeExtra::layer(panel.text(3, 14.5,index.numbering[panel.number()],  col = txtColors, fontface= 'bold', cex=1.1))

##Plot