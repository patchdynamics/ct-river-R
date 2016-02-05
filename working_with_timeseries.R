library(xts)
load('ts_fdom_discharge.Rdata')
ts = tscombined['2011-01-01/2015-12-31']

temp = read.csv("temperature_2011_2015.csv")
temp = data[!is.na(temp[,2]),]

times = strptime(paste0(temp[,2],temp[,3]), '%Y%j')
tstemp = xts(temp[,4], order.by=times)
names(tstemp) = c('Temperature')