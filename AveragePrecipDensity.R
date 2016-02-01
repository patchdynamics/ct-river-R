#!/usr/bin/Rscript
library('raster')
directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'

file.zone = raster('../GIS/LowerCTRiverCatch.tif') # mainstem low order

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
  cropped = crop(file.raster, file.zone)
  
  # ignore small precip - disabled
  # threshold = .8
  data = cropped
  # data[cropped < threshold] = 0
  
  
  result = zonal(data,file.zone, 'sum', digits=1)
  stats[i, 1] = files[i]
  stats[i, 2] = result[2,2]
  #print(result[2,2])
  
}

write.table(stats, file = "data-average-active.csv", sep = ",", 
            col.names = NA)
