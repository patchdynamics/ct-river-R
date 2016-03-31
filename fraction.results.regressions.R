fractionation.results = read.csv('../data/HPOA_regression.csv')
fractionation.results = fractionation.results[!is.na(fractionation.results$DOC.ppm),]
head(fractionation.results)
plot(fractionation.results$DOC.ppm,)
times = strptime(as.character(fractionation.results$Date), '%m/%d/%y')

ts.fractionation.results = xts(fractionation.results, order.by=times)
ts.fractionation.results = merge(ts.fractionation.results, tscombined)

fractionation.df = as.data.frame(ts.fractionation.results[!is.na(ts.fractionation.results$DOC.ppm)])
fractionation.df = fractionation.df[!is.na(fractionation.df$FDOM.QSE),]
fractionation.df = fractionation.df[order(fractionation.df$FDOM.QSE),]

DOC.lm = lm(DOC.ppm ~ FDOM.QSE, fractionation.df)
summary(DOC.lm)

HPOA.lm = lm(HPOA.ppm ~ FDOM.QSE, fractionation.df)
summary(HPOA.lm)

HPI.lm = lm(HPI.ppm ~ FDOM.QSE, fractionation.df)
summary(HPI.lm)

TPIA.lm = lm(TPIA.ppm ~ FDOM.QSE, fractionation.df)
summary(TPIA.lm)

OA.lm = lm(OA.ppm ~ FDOM.QSE, fractionation.df)
summary(OA.lm)

require(RColorBrewer)
colors = brewer.pal(9, "Set1")

attach(fractionation.df)
plot(FDOM.QSE,DOC.ppm, pch=2, col=rainbow(5)[1],
     ylim=c(0,7),
     xlab='FDOM (QSE) ',
     ylab='ppm'
     )
lines(DOC.lm$model$FDOM.QSE, predict(DOC.lm), col=colors[1], lwd=2)

points(FDOM.QSE, HPOA.ppm, pch=3, col=colors[2])
lines(HPOA.lm$model$FDOM.QSE, predict(HPOA.lm), col=colors[2], lwd=2)

points(FDOM.QSE, HPI.ppm, pch=4, col=colors[3])
lines(HPI.lm$model$FDOM.QSE, predict(HPI.lm), col=colors[3], lwd=2)

points(FDOM.QSE, TPIA.ppm, pch=5, col=colors[4])
lines(TPIA.lm$model$FDOM.QSE, predict(TPIA.lm), col=colors[4], lwd=2)

points(FDOM.QSE, OA.ppm, pch=1, col=colors[5])
lines(OA.lm$model$FDOM.QSE, predict(OA.lm), col=colors[5], lwd=2)

legend('topleft', 
       legend=c('[DOC]  r^2=.78, p << .001',
                '[OA]    r^2=.83, p << .001', 
                '[HPOA] r^2=.92, p << .001', 
                '[TPIA]  r^2=.68, p << .001', 
                '[HPI]    r^2=.10, p = .08'),
       col=colors[c(1,5,2,4,3)],
       lwd=2,
       cex=.8
       )

#text(48, 6.1, labels=c('DOC r^2=.78 p<<.001'), cex=.6)
#text(48, 4.2, labels=c('OA r^2=.83 p<<.001'), cex=.6)
detach(fractionation.df)
