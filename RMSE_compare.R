S1 = c(491182.1,477730.8,478103.6,455138.7)
S2 = c(296483,273825.3,245745.3,233565)
S3 = c(257128.1,226785.7,208258.1,390247.5)
S4 = c(241036.7,227682.5,216020.2,330900.6)
TA = c(554008,575942.4,578096,516009.9)

par(mfrow=c(1,1))
plot(TA, typ='l', ylim=c(0, max(TA)), ylab='RMSE')
points(TA, pch=18)
lines(S1, col='coral')
points(S1, col='coral', pch=18)
lines(S2, col='blue')
points(S2, col='blue', pch=18)
lines(S3, col='aquamarine')
points(S3, col='aquamarine', pch=18)
lines(S4, col='cyan')
points(S4, col='cyan', pch=18)


rmse.difference = 207892.1 - 168551.2
rmse.difference / 168551.2