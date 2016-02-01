library('waterData')

sdate = "2015-09-22"
edate = "2016-01-01"


haddam = importDVs("01193050",code="32295", sdate=sdate, edate=edate)
plotParam(haddam)


library('dataRetrieval')
url = constructNWISURL("01193050",parameterCd=c("32295","00010","63680"), 
                 startDate=sdate, endDate=edate,
                 "uv")
site_chan_data <- importWaterML1(url)

fdom = site_chan_data$X_32295_00011
temp = site_chan_data$X_00010_00011
turb = site_chan_data$X_63680_00011
fdom_temp_corr = fdom + fdom * .01 * (temp - 25)
fdom_temp_turb_corr = fdom_temp_corr / (exp(-.003*turb))
fdom_temp_turb_corr_value = fdom_temp_turb_corr[! is.na(fdom_temp_turb_corr)]

dates = as.Date(site_chan_data[! is.na(fdom_temp_turb_corr),3])
mean_cdom = aggregate(fdom_temp_turb_corr_value, by=list(dates), FUN=mean)
head(mean_cdom)
mean_cdom[,3] = 0.0482 * mean_cdom[,2] + 0.2184
tsfdom2 = xts(mean_cdom[,2:3], order.by=strptime(mean_cdom[,1], '%Y-%m-%d'))

names(tsfdom) = c('FDOM', 'HPOA')
names(tsfdom2) = c('FDOM', 'HPOA')

head(tsfdom)
head(tsfdom2)
fdom_complete = rbind(tsfdom, tsfdom2)

plot(fdom_complete$HPOA)


# and get the rest of the data - precip and discharge

url = constructNWISURL("01193050",parameterCd=c("00060"), 
                       startDate=sdate, endDate=edate,
                       "dv")
site_chan_data <- importWaterML1(url)
discharge2 = xts(site_chan_data[,5], order.by=strptime(site_chan_data[,3], '%Y-%m-%d'))

