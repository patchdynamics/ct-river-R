library(gstat)

directory = '/Users/matthewxi/Documents/Projects/PrecipGeoStats/precip_data/precip_cropped_ct_river/'

i = 10
file.raster = raster(paste(directory,files[i],sep=''))
r_spdf = as(file.raster, 'SpatialPointsDataFrame')
names(r_spdf) <- c("P")
par(mfrow=c(2,1))
plot(file.raster)
vario = variogram(P ~ 1, r_spdf, cutoff=150)
plot(vario$dist, vario$gamma, ylim=c(0,.1))
