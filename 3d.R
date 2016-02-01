library(scatterplot3d)
library(rgl)

par(mfrow=c(1,1))
scatterplot3d(tscombined$HPOA.mgl, tscombined$Discharge, tscombined$Precipitation)


par(mfrow=c(1,1))
plot3d(tscombined$HPOA.mgl, tscombined$Discharge, tscombined$Precipitation, col='red')


percent.activated = tscombined$X2
percent.activated[percent.activated == 0] = NA
percent.activated = na.locf(percent.activated)
# shift timeseries
index(percent.activated) = index(percent.activated)+2*60*60*24

above = 5
below = 9
summer = tscombined[.indexmon(tscombined) > above & .indexmon(tscombined) < below]
percent.activated = percent.activated[.indexmon(percent.activated) > above & .indexmon(percent.activated) < below]
names(percent.activated) = c('percent.activated')
summer = merge(summer, percent.activated)
par(mfrow=c(1,1))
scatterplot3d(summer$HPOA.mgl, summer$Discharge, summer$percent.activated,
              xlab='[HPOA]', ylab='Discharge', zlab='Percentage Activated')

par3d("windowRect"= c(0,0,1200,1200))
plot3d(summer$Discharge,summer$percent.activated,  summer$HPOA.mgl, 
              zlab='[HPOA]', xlab='Discharge', ylab='Percentage Activated')

library(plotly)
library(rglwidget)
names(percent.activated) = c('pa')
summer = merge(summer, percent.activated)

plot_ly(as.data.frame(summer), y = HPOA.mgl, x = Discharge, z = pa,  type = "scatter3d")
plot_ly(as.data.frame(summer), y = HPOA.mgl, x = pa,  type = "scatter")


