library('waterData')


hubbard_p = read.csv("data-precip-hubbard-average.csv")
plot(hubbard_p[,3], type='l')


watershed_area_sq_ft = 577746992.4819;
precip_cubic_feet_per_day = (hubbard_p[,3] / 12) * watershed_area_sq_ft;



sdate = "2014-01-01"
edate = "2014-12-31"

hubbard = importDVs("01187300",code="00060", sdate=sdate, edate=edate)
cfs = hubbard[,2] * 60 * 60 * 24;

plot(precip_cubic_feet_per_day[100:300], type='l',  col='red')
lines(cfs[100:300],  col='blue')


plot(precip_cubic_feet_per_day[100:300], type='l',  col='red')
lines(cfs[100:300],  col='blue')