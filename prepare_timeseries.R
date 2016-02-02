library(xts)
setwd('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R')

fdom = read.csv('../matlab/fdom_corrected_daily.csv', 'header' = FALSE)
discharge = read.csv('../matlab/discharges_full.csv', 'header' = FALSE)
percent.receiving = read.csv('data-precip-percent-receiving.csv')
percent.receiving$Files = substr(percent.receiving$Files,26,33)
rm(tscombined)

fdom[,3] = 0.0482 * fdom[,2] + .2184
fdom_dates = strptime(fdom[,1], '%Y%m%d')
tsfdom = xts(x=fdom[,2:3], order.by=fdom_dates)
head(tsfdom)
tsfdom = rbind(tsfdom, tsfdom2)

discharge_dates = strptime(discharge[,1], '%Y%m%d')
tsdischarge = xts(x=discharge[,2], order.by=discharge_dates)
names(tsdischarge) = 'Discharge'
names(discharge2) = 'Discharge'
tsdischarge = rbind(tsdischarge, discharge2)
head(tsdischarge)

tscombined = merge(tsfdom, tsdischarge)
names(tscombined) = c('FDOM.QSE', 'HPOA.mgl', 'Discharge')

precip_dates = strptime(percent.receiving[,1], '%Y%m%d')
tsprecip = xts(x=percent.receiving[,2:9], order.by=precip_dates)

tscombined = merge(tscombined, tsprecip)

precipitation = read.csv('../matlab/precip_all.csv', 'header' = FALSE)
precipitation_dates = strptime(precipitation[,1], '%Y%m%d')
tsprecipitation = xts(x=precipitation[,2], order.by=precipitation_dates)
tsprecipitation[is.na(tsprecipitation)] = 0

names(tsprecipitation2) = c('Precipitation')
tsprecipitation = rbind(tsprecipitation, tsprecipitation2)

tscombined = merge(tscombined, tsprecipitation)
tscombined[is.na(tscombined$Precipitation)]$Precipitation = 0

names = names(tscombined)
names[12] = 'Precipitation'
names(tscombined) = names

head(tscombined)
save(tscombined, file='ts_fdom_discharge.Rdata')

ts = tscombined["2012-06-01/2012-09-01"]
head(ts)

par(mfrow=c(3,1))
plot(ts$HPOA)
plot(ts$X1, ylim =c(0,1) )
plot(ts$Precipitation)


ts = tscombined["2015-01-01/2016-01-01"]
head(ts)

par(mfrow=c(3,1))
plot(ts$HPOA)
plot(ts$Discharge )
plot(ts$Precipitation)