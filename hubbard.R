directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'

file.zone = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/HubbardWatershedHRAP_0.1_Full.tif') # mainstem low order

files = list.files(path = directory, pattern = '.*2014.*.shp.tif$')
# invalid files in 2014
files[258] = NA;


stats = matrix(, ncol = 2, nrow = length(files))

for(i in 1:length(files)) {
  cat(i)
  if(is.na(files[i])){
    stats[i,1] = files[i];
    stats[i,2] = 0;
    next
  }
  
  file.raster = raster(paste(directory,files[i],sep='/'))
  resampled = resample(file.raster, file.zone)
  masked = mask(resampled, file.zone)
  
  result = zonal(masked,file.zone, 'mean', digits=2)
  stats[i, 1] = files[i]
  stats[i, 2] = result[1,2]
  #print(result[2,2])
  
}

write.table(stats, file = "data-precip-hubbard-average.csv", sep = ",", 
            col.names = NA)