  require(ncdf4)
  require(raster)
  
  soil_temp_for_zones <- function (zones, zone_rasters, level = 1, years = rbind('2009', '2010', '2011', '2012', '2013', '2014')) {
    
    num_years = length(years)
    data = matrix(ncol = 3 + length(zones), nrow = 366 * num_years)
    
    basefile = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/data/NARR/downloads/tsoil.'
    
    # set up zone and extents
    file.nc <- paste(basefile, years[1] ,'01.nc', sep='')  
    file.nc
    narr.raster <- raster(file.nc, 1, level=1)
    
    zone_masks = c()
    for(z in 1:length(zones)) {
      zone = zone_rasters[[z]]
      zone[zone == 0] = NA
      zone.extent = projectExtent(zone, crs(zone))
      res(zone.extent) = res(narr.raster) / 4
      zone.prepared = projectRaster(zone, zone.extent)    
      zone_masks = cbind(zone_masks, zone.prepared)
    }
    
    row=0
    for(y in 1:num_years){
      year = years[y]  
      
      for(m in 1:12) {
        
        if(year == '2015' &&  m == 8) {
          # skip out of there, all 2015 data isn't available yet
          break
        }
        
        file.nc <- paste(basefile, year ,  sprintf("%.2d",m), '.nc', sep='')
        bands_in_file = nbands(raster(file.nc, level=1))
        
        for(band in 1:bands_in_file) {
          cat(year, ":", m , ':',  band, "of " , bands_in_file, " \r") 
          
          narr.raster <- raster(file.nc, band, level=1)
          
          for(z in 1:length(zones)) {
            paste('zone ', toString(zones[z]), 'of ', toString(length(zones)), "zones  f\r") 
            
            narr.raster.clipped = projectRaster(narr.raster, zone_masks[[z]])     
            result = zonal(narr.raster.clipped, zone_masks[[z]], 'mean', digits=1)
            
            temp.mean = result[1,2]
            data[row,1] = year
            data[row,2] = m
            data[row,3] = band
            data[row,3+z] = temp.mean
            row = row + 1
          }
        }
        
      }
    }
    return(data)
    
  }
  
  
  require('methods')
  setwd('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R/')
  
  # zones for CT River Watershed
  zones = rbind('CT River Watershed')
  zone_rasters = list()
  zone_rasters[1] = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_NARR_Above_Haddam.tif')
  
  data = soil_temp_for_zones(zones, zone_rasters, 1, rbind('2011', '2012', '2013', '2014', '2015'))
  write.csv(data, file = "soil_temp_avg.csv")
  
