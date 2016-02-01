
directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'
file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')

files = list.files(path = directory, pattern = '.*.shp.tif$')
# invalid files in 2014
#files[258] = NA;


stats = matrix(, ncol = 2, nrow = length(files))

for(i in 1:length(files)) {
  cat(i)
  if(is.na(files[i])){
    stats[i,1] = files[i];
    stats[i,2] = 0;
    next
  }
  
  file.raster = raster(paste(directory,files[i],sep='/'))
  masked = mask(file.raster, file.zone, maskvalue = 1)
  
  # ignore small precip
  threshold = 0
  avg = mean(masked[masked > 0])  
  
  stats[i, 1] = files[i]
  stats[i, 2] = avg
  #print(result[2,2])
  
}

write.table(stats, file = "data-average-intensity.csv", sep = ",", 
            col.names = NA)