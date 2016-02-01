soilt1_monthly = brick('~/Downloads/tsoil.mon.mean.nc', level=1)
soilt2_monthly = brick('~/Downloads/tsoil.mon.mean.nc', level=2)
soilt3_monthly = brick('~/Downloads/tsoil.mon.mean.nc', level=3)
soilt4_monthly = brick('~/Downloads/tsoil.mon.mean.nc', level=4)


zone = raster('/Users/matthewxi/Documents/Projects/PrecipGeoStats/GIS/CT_River_Watershed_NARR_Above_Haddam.tif')
zone_mask = projectRaster(zone, soilt1_monthly)
zone_mask[zone_mask==0] = NA

soilt = soilt4_monthly
soilt_clipped = stack(mask(soilt$X2014.01.31.23.56.02, zone_mask),
        mask(soilt$X2014.02.28.23.56.02, zone_mask),
        mask(soilt$X2014.04.01.00.56.02, zone_mask),
        mask(soilt$X2014.05.01.00.56.02, zone_mask),
        mask(soilt$X2014.06.01.00.56.02, zone_mask),
        mask(soilt$X2014.07.01.00.56.02, zone_mask),
        mask(soilt$X2014.08.01.00.56.02, zone_mask),
        mask(soilt$X2014.09.01.00.56.02, zone_mask),
        mask(soilt$X2014.10.01.00.56.02, zone_mask),
        mask(soilt$X2014.11.01.00.56.02, zone_mask),
        mask(soilt$X2014.12.31.23.56.02, zone_mask)
)
soilt_clipped <- crop(soilt_clipped, zone)
plot(soilt_clipped, zlim=c(250,300))

freezing = soilt_clipped
freezing[freezing < 273] = NA
plot(freezing, zlim=c(250, 300))




zlimits = c(273, 300)

# 0 , 10 , 40 , 100, and 800cm for soil temperature.
par(mfrow=c(1,3))
plot(soilt1_clipped$X2014.02.28.23.56.02, zlim=zlimits)
plot(soilt2_clipped$X2014.02.28.23.56.02, zlim=zlimits)
plot(soilt3_clipped$X2014.02.28.23.56.02, zlim=zlimits)

par(mfrow=c(1,3))
plot(soilt1_clipped$X2014.04.01.00.56.02, zlim=zlimits)
plot(soilt2_clipped$X2014.04.01.00.56.02, zlim=zlimits)
plot(soilt3_clipped$X2014.04.01.00.56.02, zlim=zlimits)

par(mfrow=c(1,3))
plot(soilt1_clipped$X2014.05.01.00.56.02, zlim=zlimits)
plot(soilt2_clipped$X2014.05.01.00.56.02, zlim=zlimits)
plot(soilt3_clipped$X2014.05.01.00.56.02, zlim=zlimits)

par(mfrow=c(1,3),oma = c(0, 0, 2, 0))
plot(soilt1_clipped$X2014.06.01.00.56.02, zlim=zlimits)
plot(soilt2_clipped$X2014.06.01.00.56.02, zlim=zlimits)
plot(soilt3_clipped$X2014.06.01.00.56.02, zlim=zlimits)
#mtext("Title for Two Plots", outer = TRUE, cex = 1.5, line=-21)




par(mfrow=c(3,4))
plot( mask(soilt_monthly$X2014.01.31.23.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.02.28.23.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.04.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.05.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.06.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.07.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.08.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.09.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.10.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.11.01.00.56.02, zone, maskvalue=0), zlim=c(260,295) )
plot( mask(soilt_monthly$X2014.12.31.23.56.02, zone, maskvalue=0), zlim=c(260,295) )


