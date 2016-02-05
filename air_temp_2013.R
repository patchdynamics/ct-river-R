#!/usr/bin/Rscript --vanilla
require('methods')
source('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R/air_temp_for_zones.R')

# zones for CT River Watershed
zones = rbind('CT River Watershed')
zone_rasters = list()
zone_rasters[1] = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_NARR_Above_Haddam.tif')

data = air_temp_for_zones(zones, zone_rasters, rbind('2011', '2012', '2013', '2014', '2015'))
write.csv(data, file = "temperature_2011_2015.csv")
