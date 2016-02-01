#!/usr/bin/Rscript
library('raster')
directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'

file.zone = raster('../GIS/HubbardRiverWatershed.tif')  # not weighted, just interiod squares

files = list.files(path = directory, pattern = '.*.shp.tif$')


stats = matrix(, ncol = 2, nrow = length(files))

for(i in 1:length(files)) {
  cat(i)
  if(is.na(files[i])){
    stats[i,1] = files[i];
    stats[i,2] = 0;
    next
  }
  
  file.raster = raster(paste(directory,files[i],sep='/'))
  masked = mask(file.raster, file.zone)
  
  
  result = zonal(masked,file.zone, 'sum', digits=1)
  stats[i, 1] = files[i]
  stats[i, 2] = result[2,2]
  #print(result[2,2])
  
}

write.table(stats, file = "data-precip-hubbard.csv", sep = ",", 
            col.names = NA)
