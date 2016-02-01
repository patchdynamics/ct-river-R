library(timeSeries)
discharge = read.csv('../Haddam/discharge/full_record_discharge_date_formatted.csv')
tsdx = timeSeries(discharge[,2], strptime(discharge[,1], '%Y%m%d'))

library(zoo)
dates = strptime(discharge[,1], '%Y%m%d')
tsd = zoo(x=discharge[,2], order.by=dates)
str(tsd)
dates
collection.dates = strptime(as.character(fractionation$Collection.Date), '%m/%d/%y')
as.character(collection.dates)
tsd[as.Date('2010-09-27')]

# xts has the best subsetting
# can create with any kind of ts object
library(xts)
tsd = xts(x=discharge[,2], order.by=dates)
#tsd = as.xts(tsd)

collection.dates = strptime(as.character(fractionation$Collection.Date), '%m/%d/%y')
discharges_for_samples = tsd[collection.dates]
head(discharges_for_samples)
str(discharges_for_samples)

fractionation_series = xts(x=fractionation, order.by=collection.dates)
fractionation_series = merge(discharges_for_samples, fractionation_series[!is.na(index(fractionation_series))])

fdf = as.data.frame(fractionation_series)
names(fdf)
plot(fdf$discharges_for_samples, fdf$DOC..ppm.C.)

plot(fdf$discharges_for_samples, fdf$HPOA_ppm)

plot(fdf$discharges_for_samples, fdf$HPOA_Percentage)
discharges_for_samples = fdf$discharges_for_samples
lm.discharge.hpoa = lm(fdf$HPOA_Percentage ~ poly(discharges_for_samples,2))
summary(lm.discharge.hpoa)


plot(fdf$discharges_for_samples, fdf$HPI_ppm)

# fitting HPI % vs sample discharges
fit_discharges = fdf$discharges_for_samples
lm.discharge.hpi = lm(fdf$HPI_Percentage ~ poly(fit_discharges,2))
summary(lm.discharge.hpi)

plot(fdf$discharges_for_samples, fdf$HPI_Percentage)
fitvals= data.frame( fit_discharges = 
                       seq(min(fdf$discharges_for_samples),max(fdf$discharges_for_samples),100) )
lines(fitvals$discharges_for_samples, predict(lm.discharge.hpi, fitvals))

plot(fdf$discharges_for_samples, fitted(lm.discharge.hpi))

# there is one HPI point that is elevation during spring high flow
# othe than this there is a descending polynomial trend





