
gdal_setInstallation('/Library/Frameworks/GDAL.framework/Versions/1.11/Programs')
gdal_translate("../data/MODIS/e4ftl01.cr.usgs.gov/PullDir/030352425471818/MOD13C2.A2013001.005.2013042100040.hdf",
              "Your_Image_Out.tif",
              sd_index=1,
              projwin = c(-75, 48, -70, 40))  
r = raster('Your_Image_Out.tif')
plot(r)


path = '../data/MODIS/e4ftl01.cr.usgs.gov/PullDir/030352425471818/'
files = list.files(path, pattern='.hdf$')
for(i in 1:length(files)){
  file = paste0(path, files[i])
  gdal_translate(file,
                paste0("../data/MODIS/",files[i],"_NE.tif"),
                 sd_index=1,
                 projwin = c(-75, 48, -70, 40))    
}

path = '../data/MODIS/'
files = list.files(path, pattern='.tif$')
for(i in 1:length(files)){
  ifile = paste0(path,files[i])
  ofile = paste0(ifile, '_wgs84.tif')
  gdalwarp(ifile,ofile,s_srs="+proj=longlat +ellps=clrk66 +no_defs",
           t_srs="+proj=longlat +datum=WGS84 +no_defs",
           overwrite = TRUE) 
}


zone = raster('../GIS/CT_River_Watershed_Above_Haddam_WGS84.tif')
plot(zone)
files = list.files(path, pattern='.wgs84.tif$')
out = NULL
for(i in 1:length(files)){
  ifile = paste0(path,files[i])
  ifile.raster = projectRaster(raster(ifile), zone)
  stats = zonal(ifile.raster, zone, 'mean')
  out = rbind(out, c(ifile, stats[2,2]))
}
out[,1] = substr(out[,1], 24,30)
ndvi.values = as.numeric(out[,2])
ts.ndvi = xts(ndvi.values, order.by=strptime(out[,1], '%Y%j'))

# need to create filled ndvi series
ts.ndvi = na.approx(ts.ndvi, xout=index(tscopy))
indexTZ(ts.ndvi) = Sys.getenv("TZ")

save(ts.ndvi, file='ts.ndvi.Rdata')



