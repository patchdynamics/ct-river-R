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
  hundreth_pixels = length(file.raster[file.raster > .01])
  tenth_pixels = length(file.raster[file.raster > .1])
  one_pixels = length(file.raster[file.raster > 1])
  two_pixels = length(file.raster[file.raster > 2])
  three_pixels = length(file.raster[file.raster > 3])
  four_pixels = length(file.raster[file.raster > 4])
  five_pixels = length(file.raster[file.raster > 5])
  six_pixels = length(file.raster[file.raster > 6])
  
  
  stats[i, 1] = files[i]
  stats[i, 2] = hundreth_pixels / watershed_pixels
  stats[i, 3] = tenth_pixels / watershed_pixels
  stats[i, 4] = one_pixels / watershed_pixels
  stats[i, 5] = two_pixels / watershed_pixels
  stats[i, 6] = three_pixels / watershed_pixels
  stats[i, 7] = four_pixels / watershed_pixels
  stats[i, 8] = five_pixels / watershed_pixels
  stats[i, 9] = six_pixels / watershed_pixels
  
}

write.table(stats, file = "data-precip-percent-receiving.csv", sep = ",", 
            col.names = c('Files', '.01', '.1', '1', '2', '3', '4', '5', '6'))