#!/usr/bin/Rscript --vanilla
library(raster);
directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'

file.zone = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_Watershed_Above_Haddam_HRAP_Projected_0.1.tiff')


stats = matrix(, ncol = 2, nrow = 366 * 5)


years = c('2011', '2012', '2013', '2014', '2015')
si = 1;
for(y in 1:length(years)) {
  
  year = years[y];
  cat(year)
  files = list.files(path = directory, pattern = paste('.*',year,'.*.shp.tif$', sep=''))  
  
  if(year == '2012') {
    # invalid files in 2012
    files[194] = NA;
    files[272] = NA;    
  } else if (year == '2014') {
    # invalid files in 2014
    files[258] = NA;    
  }
  
  for(i in 1:length(files)) {
    cat(i)
    if(is.na(files[i])){
      stats[si,1] = files[i];
      stats[si,2] = 0;
      si = si + 1;
      next
    }
    
    file.raster = raster(paste(directory,files[i],sep='/'))
    resampled = resample(file.raster, file.zone)
    masked = mask(resampled, file.zone)
    
    result = zonal(masked,file.zone, 'mean', digits=2)
    stats[si, 1] = files[i]
    stats[si, 2] = result[1,2]
    #print(result[2,2])
    si = si + 1;
    
  }
  
}

write.table(stats, file = "data-precip-ct-river-watershed-average.csv", sep = ",", 
            col.names = NA)
