tsfreezing.mean = mean(tsfreezing$HPOA.mgl[!is.na(tsfreezing$HPOA.mgl)])
tsfreezing.residuals = tsfreezing$HPOA.mgl[!is.na(tsfreezing$HPOA.mgl)] - tsfreezing.mean

plot(tsfreezing.residuals)
plot(as.vector(tsfreezing.residuals))

mse = mean(tsfreezing.residuals^2)

histogram(as.vector(tsfreezing$HPOA.mgl[!is.na(tsfreezing$HPOA.mgl)]))


hpoa.winter = as.vector( tsfreezing$HPOA.mgl[!is.na(tsfreezing$HPOA.mgl)] )
training = hpoa.winter[sort(sample(length(hpoa.winter), 20))]
training.mean = mean(training)
plot(training)
resid = training - training.mean
plot(resid)
plot(resid[1:length(resid)-1], resid[2:length(resid)], xlim=c(-.3,.3), ylim=c(-.3,.3))

mse = mean((hpoa.winter - mean(training))^2)
mse  # around .017
     # mean around 1.4, both population and training set

# no rsquared here, because all we are estimating by is a mean