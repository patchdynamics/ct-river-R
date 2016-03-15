times2 = index(tshval[tshval$Temperature >= 290.15 & tshval$Temperature < 295.15])
ts2 = ts[times2]

hpoa.vector = as.vector(ts2$HPOA.mgl)
# test for autocorrellation
plot(hpoa.vector[1:length(hpoa.vector)-1], hpoa.vector[2:length(hpoa.vector)])
# very very correlated, which is not a surprise


# so we will try to extract random subsets of the data, still in order
df = as.data.frame(ts2)
df = df[!is.na(df$HPOA.mgl) & !is.na(df$Discharge),]
training = df[sort(sample(nrow(df), 20)),]

# test for autocorrellation
hpoa.vector = training$HPOA.mgl
plot(hpoa.vector[1:length(hpoa.vector)-1], hpoa.vector[2:length(hpoa.vector)])

mod2 = lm(hpoa.vector[-length(hpoa.vector)] ~ hpoa.vector[-1]) 
#summary(mod2)

lm.hpoa = lm(HPOA.mgl ~ Discharge, data = training)
#summary(lm.hpoa)

mod3 = lm(lm.hpoa$residuals[-1] ~  lm.hpoa$residuals[-length(lm.hpoa$residuals)]) 
out = summary(mod3)
plot(lm.hpoa$residuals[-1], lm.hpoa$residuals[-length(lm.hpoa$residuals)], main=out$r.squared)
lines(lm.hpoa$residuals[-length(lm.hpoa$residuals)], predict(mod3))



plot(as.vector(ts2$Discharge), as.vector(ts2$HPOA.mgl))
lines(as.vector(training$Discharge), predict(lm.hpoa))
points(as.vector(training$Discharge), as.vector(training$HPOA.mgl), col='red', pch=2)

hpoa.diff = diff(df$HPOA.mgl)
discharge.diff = diff(df$Discharge)
plot(discharge.diff, hpoa.diff)  # looking pretty gaussian, no information here..

plot(df$Discharge[-nrow(df)], hpoa.diff)  # no response

