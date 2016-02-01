#!/usr/bin/Rscript --vanilla
require('methods')
source('/Users/matthewxi/Documents/Projects/GIS/NARR/air_temp_for_zones.R')

# zones for CT River Watershed
zones = rbind('b01372058' , 
              'b01463500' ,
              'b01578310' ,
              'b01646580' ,
              'b02226160' ,
              'b02359170' ,
              'b02470500' ,
              'b03303280' ,
              'b03374100' ,
              'b03378500' ,
              'b03609750' ,
              'b03612500' ,
              'b04264331' ,
              'b05420500' ,
              'b05586100' ,
              'b05587455' ,
              'b06338490' ,
              'b06440000' ,
              'b06467500' ,
              'b06610000' ,
              'b06805500' ,
              'b06934500' ,
              'b07022000' ,
              'b07263620' ,
              'b07288955' ,
              'b07373420' ,
              'b07374000' ,
              'b07374525' ,
              'b07381495' ,
              'b07381590' ,
              'b07381600' ,
              'b08012150' ,
              'b08116650' ,
              'b08475000' ,
              'b09522000' ,
              'b11303500' ,
              'b11447650' ,
              'b14246900' ,
              'b15565447' ,
              'b32202309' )   
zone_rasters = list()
for(i in 1:length(zones)){
  zone_rasters[i] = raster(paste('/Users/matthewxi/Documents/Projects/NASQAN/nasqan_raster_narr/', zones[i], '.shp.tiff', sep=''))
}

data = air_temp_for_zones(zones, zone_rasters, rbind('2014'))
write.csv(data, file = "nasqan_temperature_avg_2014.csv")
