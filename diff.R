ts$HPOA.diff =  diff(ts$HPOA.mgl)
ts$Discharge.diff =  diff(ts$Discharge)

tsdiff = ts
tsdiff = tsdiff[!is.nan(tsdiff$Discharge.diff)]
tsdiff = tsdiff[!is.na(tsdiff$Discharge.diff)]
tsdiff = tsdiff[!is.nan(tsdiff$HPOA.diff)]
tsdiff = tsdiff[!is.na(tsdiff$HPOA.diff)]

ts3 = tsdiff[.indexmon(tsdiff) == 6]
ts3 = tsdiff
plot(as.vector(ts3$Discharge.diff), as.vector(ts3$HPOA.diff))
plot(as.vector(ts$Discharge[.indexmon(ts) == 6]), as.vector(ts$HPOA.mgl[.indexmon(ts) == 6]), typ='l')


formula = y ~x
frame = data.frame(x = as.vector(ts3$Discharge.diff), y = as.vector(ts3$HPOA.diff))
lm.diff = lm(formula, frame)
summary(lm.diff)

# first differences are completely uncorrelated because of mixing