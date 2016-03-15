library(raster)
library(rasterVis)
library(ggplot2)


zone.narr = raster('../GIS/CT_River_Watershed_NARR_Above_Haddam.tif')
zone.narr[zone.narr==0] = NA
plot(zone.narr)


precip.o = brick('~/Downloads/acpcp.2014.nc')
evap = brick('~/Downloads/evap.2014.nc')
snow = brick('~/Downloads/weasd.2014.nc')

# data debugging
zone.narr.placement = projectRaster(zone.narr, precip.o)
zone.narr.placement[is.na(zone.narr.placement)] = 0

gplot(precip.o[[10]] + zone.narr.placement*100) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = 'white', high = 'blue') + 
  coord_equal() 


  precip = projectRaster(precip.o, zone.narr)
save(precip, file='precip.narr.2014.Rdata')
load('precip.narr.2014.Rdata')
evap = projectRaster(evap, zone.narr)
save(evap, file='evap.narr.2014.Rdata')
load('evap.narr.2014.Rdata')

balance = precip - evap

ts.precip = cellStats(precip ,'sum')
ts.evap = cellStats(evap ,'sum')
ts.snow = cellStats(snow ,'sum')


balance = mask(balance, zone.narr)

save(balance, file='balance.narr.2014.Rdata')
load('balance.narr.2014.Rdata')

balance.mean = cellStats(balance, 'mean')  # kg/m^2 , same as l/m^2

watershed_area_sq_ft = 303311329435.446 # ft^2
watershed_area_sq_m = watershed_area_sq_ft * 0.092903;

# NO not a cubic meter is a cell
balance.kg = balance.mean * watershed_area_sq_m  
balance.liters = balance.kg # 1 kg water = 1 L
balance.cubic.meters = balance.kg * 0.001

tsd2014 = ts$Discharge['2014-01-01/2014-12-31']
cubic.feet.daily = tsd2014$Discharge * 60 * 60 * 24;
cubic.meters.daily = cubic.feet.daily * 0.0283168
discharge = cubic.meters.daily

discharge$atmospheric = balance.cubic.meters
discharge$precipitation = ts.precip

discharge$Discharge = na.locf(discharge$Discharge)
discharge$dS = discharge$atmospheric - discharge$Discharge

fakeS = cumsum(discharge$dS)

#hmmm

tsp2014 = ts$Precipitation['2014-01-01/2014-12-31']
plot(tsp2014)
plot(tsd2014$Discharge)


# WTF disaggrement
precip2013 = brick('~/Downloads/acpcp.2013.nc')
zone.narr = projectRaster(zone.narr, precip2013)


gplot(zone.narr) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = 'white', high = 'blue') + 
  coord_equal() 

gplot(precip2013[[1]]) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = 'white', high = 'blue') + 
  coord_equal() 



#precip2013 = projectRaster(precip2013, zone.narr)
precip2013.masked = mask(precip2013, zone.narr)


ts.precip2013 = cellStats(precip2013.masked ,'sum')
plot(ts.precip2013)

tsp2013 = ts$Precipitation['2013-01-01/2013-12-31']
plot(tsp2013)

tsp2013$narr = ts.precip2013

par(mfrow=c(2,1))
plot(tsp2013$Precipitation)
plot(tsp2013$narr)


# ???
precip2014 = brick('~/Downloads/acpcp.2014.nc')
zone.narr = projectRaster(zone.narr, precip2014)


gplot(zone.narr) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = 'white', high = 'blue') + 
  coord_equal() 

gplot(precip2014[[1]]) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = 'white', high = 'blue') + 
  coord_equal() 



#precip2014 = projectRaster(precip2014, zone.narr)
precip2014.masked = mask(precip2014, zone.narr)


ts.precip2014 = cellStats(precip2014.masked ,'sum')
plot(ts.precip2014, typ='l')


snow2014 = brick('~/Downloads/snom.2014.nc')
snow.masked = mask(snow2014, zone.narr)
ts.snow2014 = cellStats(snow.masked, 'sum')
plot(ts.snow2014, typ='l')


tsp2014 = ts$Precipitation['2014-01-01/2014-12-31']
plot(tsp2014)


tsp2014$narrp = ts.precip2014
tsp2014$narr2 = ts.snow2014

par(mfrow=c(4,1))
plot(tsp2014$Precipitation)
plot(tsp2014$narrp)
plot(tsp2014$narr2)
plot(discharge)
