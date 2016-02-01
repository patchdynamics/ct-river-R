require(ncdf4)
require(raster)


zones = rbind('CT_River_Watershed')
zone_rasters = list();
zone_rasters[1] = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_Above_Haddam_WGS84.tif')
years = rbind('2012', '2013', '2014')

  
days_in_year = 365
num_years = length(years)

data = matrix(ncol = 2 + length(zones), nrow = days_in_year * num_years)
  
flush.console()
index = 1
for(y in 1:num_years){
    year = years[y]  
    dates = seq(as.Date(paste(year,"-01-01",sep='')), as.Date(paste(year,"-12-31",sep='')), by="1 day")    
    for(day in 1:length(dates)) {
      
      date_string = format(dates[day], '%Y%m%d')
      file <- paste('/Volumes/substrate/GIS/Snow/zeros_removed/processed_us_ssmv11044bS__T0024TTNATS', date_string,'05DP000.dat.reproj.tif', sep='')
      
      cat(year, ":", (y-1) * days_in_year + day, "of " , days_in_year * num_years, " \r") 
      cat(file)
      flush.console()
      
      if( ! file.exists(file)) {
        next
      }
      r.raster <- raster(file, 1)
      
      for(z in 1:length(zones)) {
        paste('zone ', toString(zones[z]), 'of ', toString(length(zones)), "zones  f\r") 
        flush.console()
        
        zone = zone_rasters[[z]]
        cropped = crop(r.raster, zone)
        clipped = resample(cropped, zone, method='ngb')
        result = zonal(clipped, zone, 'sum', digits=1, na.rm=TRUE)
        
        
        zonal_statistic= result[2,2]
        data[index,1] = year
        data[index,2] = day
        data[index,2+z] = zonal_statistic
        index = index + 1
      }
    }
    
}

#return(data)
  
