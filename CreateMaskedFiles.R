#!/usr/bin/Rscript
require(raster)

path = "../precip_data/precipitation_raster/";
files = list.files(path = path, pattern = '2015.*.shp.tiff$')

# invalid files in 2012
#files[194] = NA;
#files[272] = NA;

# invalid files in 2013
#files[130] = NA

# invalid files in 2014
#files[258] = NA;


file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')


maximumValue = 0;
par(mfrow=c(37,10))
par(mfrow=c(3,10))

par(mar=c(1,1,3,1))


brks <- seq(0, 5, by=0.01) 
nb <- length(brks)-1 
colors <- rev(rainbow(nb))

for(i in 1:length(files)) {
  
  cat(i)
  
  if(is.na(files[i])) {
    next  
  }
  
  file.raster = raster(paste(path, files[i], sep='/'))
  
  cropped = crop(file.raster, file.zone)
  masked = mask(cropped, file.zone, maskvalue=0)
  writeRaster(masked, 
              paste('../precip_data/precip_cropped_ct_river', files[i], sep = '/'),
              overwrite=TRUE)
  
}

