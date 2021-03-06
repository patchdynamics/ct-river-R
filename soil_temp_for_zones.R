  require(ncdf4)
  require(raster)
  
  soil_temp_for_zones <- function (zones, zone_rasters, levels = c(1), years = rbind('2009', '2010', '2011', '2012', '2013', '2014')) {
    
    num_years = length(years)
    data = matrix(ncol = 3 + length(zones)* length(levels), nrow = 365 * num_years )
    
    basefile = '~/Documents/Projects/PrecipGeoStats/data/NARR/downloads/tsoil.'
    
    # set up zone and extents
    file.nc <- paste(basefile, years[1] ,'12.nc', sep='')  
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
    
    row=1
    for(y in 1:num_years){
      year = years[y]  
      
      for(m in 1:1) {
        
       
        file.nc <- paste(basefile, year ,  sprintf("%.2d",m), '.nc', sep='')
        bands_in_file = nbands(raster(file.nc, level=1))
        
        for(band in 1:bands_in_file) {
          cat(year, ":", m , ':',  band, "of " , bands_in_file, " \r") 
          
          for(l in 1:length(levels)) {
            
            narr.raster = raster(file.nc, band, level=levels[l])
            
            for(z in 1:length(zones)) {
              
              narr.raster.clipped = projectRaster(narr.raster, zone_masks[[z]])     
              result = zonal(narr.raster.clipped, zone_masks[[z]], 'mean', digits=1)
              
              temp.mean = result[1,2]
              data[row,1] = year
              data[row,2] = m
              data[row,3] = band
              data[row,3+z + (l-1)*length(zones)] = temp.mean
            }
          }
          row = row + 1
          
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
  
  data = soil_temp_for_zones(zones, zone_rasters, c(1,2,3,4,5), rbind('2011'))
  write.csv(data, file = "soil_temp_avg_201101.csv")
  
