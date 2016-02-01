require(raster)

path = "../precip_data/precip_cropped_ct_river/";
files = list.files(path = path, pattern = '2012.*.shp.tif$')

# invalid files in 2012
files[194] = NA;
files[272] = NA;

# invalid files in 2014
#files[258] = NA;



maximumValue = 0;
par(mfrow=c(37,10))
par(mfrow=c(3,10))

par(mar=c(1,1,3,1))


brks <- seq(0, 5, by=0.01) 
nb <- length(brks)-1 
colors <- rev(rainbow(nb))

for(i in 1:length(files)) {
  
  cat(i)
  
  if(is.na(files[i])) {
    next  
  }
  
  file.raster = raster(paste(path, files[i], sep='/'))
  
  maxval = maxValue(file.raster )
  if(maxval > maximumValue){
    maximumValue = maxval;
  }
  
  date = gsub('nws_precip_1day_observed_', '', files[i])
  date = gsub('.shp.tiff', '', date)
  
  plot(file.raster, breaks=brks, lab.breaks=brks, zlim=c(0,5),  
       legend=FALSE, axes=FALSE, box=FALSE, main=date) 
  
}

