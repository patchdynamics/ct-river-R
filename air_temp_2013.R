#!/usr/bin/Rscript --vanilla
require('methods')
source('/Users/matthewxi/Documents/Projects/GIS/NARR/air_temp_for_zones.R')

# zones for CT River Watershed
zones = rbind('CT River Watershed')
zone_rasters = list()
zone_rasters[1] = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_NARR_Above_Haddam.tif')

data = air_temp_for_zones(zones, zone_rasters, rbind('2013'))
write.csv(data, file = "temperature_avg_2013.csv")
