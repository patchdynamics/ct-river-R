require(raster)

files = list.files()

file.zone = raster('../../GIS/CT_Watershed_Above_Haddam_HRAP_Projected.tiff')

for(i in 1:length(files)) {

	file.raster = raster(files[i])
	cropped = crop(file.raster, file.zone)
	result = zonal(cropped, file.zone, 'sum', digits=1)  # backwards ??!>?!
	stats = matrix(, ncol = 2, nrow = 1)
	stats[i, 0] = files[i]
	stats[i, 1] = result[1]
	write.table(resultMatrix, file = "data-appended.csv", sep = ",", 
			col.names = NA)
}


