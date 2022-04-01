####Process CHIRTmax time series for WA####

list.files= list('F:/CHIRTSmax/CHIRTSmax.monthly.nc')
chirtmax.brick = stack(list.files)


library(ncdf4)
chirtmax.nc<-nc_open('F:/CHIRTSmax/CHIRTSmax.monthly.nc')
print(chirtmax.nc)
varname<-names(chirtmax.nc$var)

Use raster to convert your ncdf to raster

library (raster)
chirtmax.global<-brick('F:/CHIRTSmax/CHIRTSmax.monthly.nc',varname='Tmax')

Here the plot

spplot(r[1])
