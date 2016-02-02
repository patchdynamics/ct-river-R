directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_raster_rest_of_2015//'
files = list.files(directory)
file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')

#file.zone = raster('../GIS/LowerCTRiverCatch.tiff') # mainstem low order

#directory = '.';
#files = list.files(path = directory, pattern = '.*.shp.tif$')
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
  data = crop(file.raster, file.zone)
    

  result = zonal(data,file.zone, 'sum', digits=1)
  stats[i, 1] = files[i]
  stats[i, 2] = result[2,2]
  #print(result[2,2])
  
}

write.table(stats, file = "data-precip-rest-of-2015.csv", sep = ",", 
            col.names = NA)





tsprecipitation = rbind(tscombined$Precipitation, tsprecipitation2)
