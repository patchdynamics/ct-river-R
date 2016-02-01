require(raster)

files = list.files()
files = list.files(path = ".", pattern = '2012')
files[194] = NA;
files[272] = NA;

file.zone = raster('../../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')


thresholds = cbind(.2,.5,1)
#thresholds = cbind(.2)

stats = matrix(, ncol = 2+length(thresholds), nrow = length(files))

for(i in 1:length(files)) {
  cat(i)
  
  if(is.na(files[i])) {
    next  
  }
  
  file.raster = raster(files[i])
  
  cropped = crop(file.raster, file.zone)
  stats[i, 1] = files[i]
  result = zonal(cropped, file.zone, 'sum', digits=1) 
  stats[i, 2] = result[2,2]
  
  for(j in 1:length(thresholds)) {
    threshold = thresholds[j]
    data = cropped
    data[cropped < threshold] = 0
    data[cropped >= threshold] = 1
    result = zonal(data, file.zone, 'sum', digits=1)
    stats[i, 2+j] = result[2,2]
  }
}
write.table(stats, file = "storm-areal-data-appended.csv", sep = ",", 
            col.names = NA)

