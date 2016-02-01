directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precipitation_raster/'
files = list.files(directory)
file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')

file.zone = raster('../GIS/LowerCTRiverCatch.tiff') # mainstem low order

directory = '.';
files = list.files(path = directory, pattern = '.*.shp.tif$')
# invalid files in 2014
#files[258] = NA;


stats = matrix(, ncol = 2, nrow = length(files))

for(i in 1:count(files)) {
  cat(i)
  if(is.na(files[i])){
    stats[i,1] = files[i];
    stats[i,2] = 0;
    next
  }
  
  file.raster = raster(paste(directory,files[i],sep='/'))
  cropped = crop(file.raster, file.zone)
  
  # ignore small precip
  threshold = .8
  data = cropped
  data[cropped < threshold] = 0
  

  result = zonal(data,file.zone, 'sum', digits=1)
  stats[i, 1] = files[i]
  stats[i, 2] = result[2,2]
  #print(result[2,2])
  
}

write.table(stats, file = "data-precip-floodplain_zone.csv", sep = ",", 
            col.names = NA)