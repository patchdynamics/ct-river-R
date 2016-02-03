require(ncdf4)
require(raster)

air_temp_for_zones <- function (zones, zone_rasters, years = rbind('2009', '2010', '2011', '2012', '2013', '2014')) {
  
  days_in_year = 365
  num_years = length(years)
  data = matrix(ncol = 2 + length(zones), nrow = days_in_year * num_years)
  
  flush.console()
  for(y in 1:num_years){
    year = years[y]  
    
    file.nc <- paste('/Users/matthewxi/Documents/Projects/GIS/NARR/air.sfc.', year ,'.nc', sep='')
    
    for(day in 1:days_in_year) {
      cat(year, ":", (y-1) * days_in_year + day, "of " , days_in_year * num_years, " \r") 
      flush.console()
      
      r.temperature <- raster(file.nc, day)
      
      for(z in 1:length(zones)) {
        paste('zone ', toString(zones[z]), 'of ', toString(length(zones)), "zones  f\r") 
        flush.console()
        
        zone = zone_rasters[[z]]
        clipped = resample(r.temperature, zone, method='ngb')
        result = zonal(clipped, zone, 'mean', digits=1)
      
        
        temp.mean = result[2,2]
        row = (y-1)*(days_in_year) + day;
        data[row,1] = year
        data[row,2] = day
        data[row,2+z] = temp.mean
      }
    }
    
  }
  return(data)
  
}
