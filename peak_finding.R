library(pracma)
p = findpeaks(x)
plot(ts$HPOA.mgl)
points(ts$HPOA.mgl[p[,2]])


discharges = as.data.frame(ts$Discharge[p[,2]])
head(discharges)
plot(discharges$Discharge, p[,1])