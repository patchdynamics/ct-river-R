  directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/raster_2014/'
  files = list.files(directory)
  file.zone = raster('../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')
  
  
  b1masked2 = 0
  b1masked3 = 0
  b2masked3 = 0
  
  stats = matrix(, ncol = 2, nrow = length(files))
  
  for(i in 1:365) {
    if(files[i] == 'nws_precip_1day_observed_20140915.shp.tiff') {
      b1masked2 = 0
      b2masked3 = b1masked3
      b1masked3 = 0
      next
    }
    
    file.raster = raster(paste(directory,files[i],sep=''))
    cropped = crop(file.raster, d.zone.1)
    masked1 = mask(cropped, d.zone.1)
    masked1[is.na(masked1)] = 0 
    cropped = crop(file.raster, d.zone.2)
    masked2 = mask(cropped, d.zone.2)
    masked2[is.na(masked2)] = 0 
    cropped = crop(file.raster, d.zone.3)
    masked3 = mask(cropped, d.zone.3)
    masked3[is.na(masked3)] = 0 
    
    
    basefile = paste('precip_adjust/', files[i],sep='')
    save(masked1,file=paste(basefile,'masked1.tiff',sep=''))
    save(masked2,file=paste(basefile,'masked2.tiff',sep=''))
    save(masked3,file=paste(basefile,'masked3.tiff',sep=''))
                   
    composite = masked1 + b1masked2*.85 + b2masked3*.7
    #plot(composite)
    result = zonal(composite,file.zone, 'sum', digits=1)
    stats[i, 1] = files[i]
    stats[i, 2] = result[2,2]
    print(result[2,2])
    #save(composite, file=paste(basefile,'composite.tiff',sep=''))
    
    #result = zonal(file.zone, cropped, 'sum', digits=1)
  
    b1masked2 = masked2
    b2masked3 = b1masked3
    b1masked3 = masked3
  }
  
  write.table(stats, file = "data-appended.csv", sep = ",", 
              col.names = NA)