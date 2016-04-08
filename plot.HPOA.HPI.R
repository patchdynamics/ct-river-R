#fractionation.df
names(fractionation.df)

par(mfrow=c(1,2))
attach(fractionation.df)
lm.hpoa = lm(HPOA.ppm ~ DOC.ppm)
lm.hpi = lm(HPI.ppm ~ DOC.ppm)

plot(DOC.ppm, HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='DOC (mg/l)')
points(DOC.ppm, HPI.ppm, col='brown1')
legend('topleft', legend=c('[HPOA] r^2=0.87, p << .001', 
                           '[HPI] r^2=.18, p=.02'), 
       col=c('chartreuse3', 'brown1'), pch=1, cex=.8)

lm.hpoa = lm(HPOA.ppm ~ fractionation.df$Discharge)
lm.hpi = lm(HPI.ppm ~ fractionation.df$Discharge)
plot(fractionation.df$Discharge * 0.0283168, HPOA.ppm, ylim=c(0,3), col='chartreuse3',
     ylab='mg/l', xlab='Discharge (m^3/s)')
points(fractionation.df$Discharge * 0.0283168, HPI.ppm, col='brown1')

detach(fractionation.df)

