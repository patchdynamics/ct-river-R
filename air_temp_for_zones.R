require(ncdf4)
require(raster)

air_temp_for_zones <- function (zones, 
                                zone_rasters, 
                                years = rbind('2009', '2010', '2011', '2012', '2013', '2014'),
                                basefile = '/Users/matthewxi/Documents/Projects/GIS/NARR/air.sfc.') {
  
  num_years = length(years)
  data = matrix(ncol = 2 + length(zones), nrow = 366 * num_years)
  
  
  # set up zone and extents
  file.nc <- paste(basefile, years[1] ,'.nc', sep='')  
  file.nc
  r.temperature <- raster(file.nc, 1)

  zone_masks = c()
  for(z in 1:length(zones)) {
    zone = zone_rasters[[z]]
    zone[zone == 0] = NA
    zone.extent = projectExtent(zone, crs(zone))
    res(zone.extent) = res(r.temperature) / 4
    zone.prepared = projectRaster(zone, zone.extent)    
    zone_masks = cbind(zone_masks, zone.prepared)
  }
  
  flush.console()
  row=1
  for(y in 1:num_years){
    year = years[y]  
    
    file.nc <- paste(basefile, year ,'.nc', sep='')
    days_in_year = nbands(raster(file.nc))
    
    for(day in 1:days_in_year) {
      cat(year, ":", (y-1) * days_in_year + day, "of " , days_in_year * num_years, " ") 
      flush.console()
      
      r.temperature <- raster(file.nc, day)
      
      for(z in 1:length(zones)) {
        paste('zone ', toString(zones[z]), 'of ', toString(length(zones)), "zones  f\r") 
        flush.console()
        
        r.temperature.clipped = projectRaster(r.temperature, zone_masks[[z]])     
        result = zonal(r.temperature.clipped, zone_masks[[z]], 'mean', digits=1)
      
        
        temp.mean = result[1,2]
        data[row,1] = year
        data[row,2] = day
        data[row,2+z] = temp.mean
        row = row + 1
      }
    }
    
  }
  return(data)
  
}


zones = rbind('CT River Watershed')
zone_rasters = list()
zone_rasters[1] = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_NARR_Above_Haddam.tif')

data = air_temp_for_zones(zones, zone_rasters, rbind('2015'), basefile='~/Downloads/air.sfc.')
write.csv(data, file = "temperature_2015_all.csv")

