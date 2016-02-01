#!/usr/bin/Rscript --vanilla
library(raster)
setwd('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R')
path = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'
file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')

files = list.files(path = path, pattern = '.*.shp.tif$')
#files = list.files(path = path, pattern = '2014.*.shp.tif$')

watershed_pixels = length(file.zone[file.zone == 1])

stats = matrix(, ncol = 9, nrow = length(files))

for(i in 1:length(files)) {
  cat(i)
  
  file.raster = raster(paste(path,files[i],sep=''))
  file.raster[file.raster < .01] = 0
  pixels_active = length(file.raster[file.raster >= .01])
  
  tenth_pixels = length(file.raster[file.raster > .1])
  one_pixels = length(file.raster[file.raster > 1])
  two_pixels = length(file.raster[file.raster > 2])
  three_pixels = length(file.raster[file.raster > 3])
  four_pixels = length(file.raster[file.raster > 4])
  five_pixels = length(file.raster[file.raster > 5])
  six_pixels = length(file.raster[file.raster > 6])
  
  
  stats[i, 1] = files[i]
  stats[i, 2] = pixels_active
  stats[i, 3] = tenth_pixels / pixels_active
  stats[i, 4] = one_pixels / pixels_active
  stats[i, 5] = two_pixels / pixels_active
  stats[i, 6] = three_pixels / pixels_active
  stats[i, 7] = four_pixels / pixels_active
  stats[i, 8] = five_pixels / pixels_active
  stats[i, 9] = six_pixels / pixels_active
  
}

write.table(stats, file = "data-precip-percent-threshold_activated.csv", sep = ",", 
            col.names = c('Files', 'PixelsActive', '.1', '1', '2', '3', '4', '5', '6'))
