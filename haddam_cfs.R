library('waterData')


precip = read.csv("data-precip-ct-river-watershed-average.csv")
precip = precip[1:1725,]
plot(precip[,3], type='l')

precip = precip[762:nrow(precip),] # to match what's available at haddam


watershed_area_sq_ft = 303311329435.446; 
precip_cubic_feet_per_day = (precip[,3] / 12) * watershed_area_sq_ft;



sdate = "2011-01-01"
edate = "2015-09-18"

discharge = importDVs("01193050",code="00060", sdate=sdate, edate=edate)
cfs = discharge[,2] * 60 * 60 * 24;
cfs[is.na(cfs)] = 0;

plot(precip_cubic_feet_per_day[100:300], type='l',  col='red')
lines(cfs[100:300],  col='blue')


plot(cumsum(precip_cubic_feet_per_day[100:300]), type='l',  col='red')
lines(cumsum(cfs[100:300]),  col='blue')




plot(precip_cubic_feet_per_day, type='l',  col='red')
lines(cfs,  col='blue')

plot(cumsum(precip_cubic_feet_per_day), type='l',  col='red')
lines(cumsum(cfs),  col='blue')