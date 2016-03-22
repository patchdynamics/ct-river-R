library(car)
dwt(lm(flux.mgs ~ Discharge, df))

df = as.data.frame(ts.window)
df$t = .indexday(ts.window)
df$group = .indexyear(ts.window)
df = df[!is.na(df$Discharge) & !is.na(df$flux.mgs),]

lm.model = lm(flux.mgs ~ Discharge, df)
gls.model = gls( flux.mgs ~ Discharge, df)
gls.model.ar1 = update(gls.model, correlation=corARMA(p=1, form= ~t | group))
gls.model.ar1.ma1 = update(gls.model, correlation=corARMA(p=1, q=1, form= ~t | group))
gls.model.ar2 = update(gls.model, correlation=corARMA(p=2, form= ~t | group))
gls.model.ma1 = update(gls.model, correlation=corARMA(q=1, form= ~t | group))

summary(gls.model)$AIC
summary(gls.model.ar1)$AIC

p = predict(gls.model, df)
p.ar1 = predict(gls.model.ar1, df)
p.ar2= predict(gls.model.ar2, df)
p.ar1.ma1= predict(gls.model.ar1.ma1, df)
p.ma1= predict(gls.model.ma1, df)
p.lm = predict(lm.model)


plot(df$flux.mgs - p, col='blue', typ='l')
lines(df$flux.mgs - p.ar1, col='red')
lines(df$flux.mgs - p.ar2, col='green')
lines(df$flux.mgs - p.ar1.ma1, col='orange')
lines(df$fluxd.mgs - p.ma1, col='azure1', lwd='2')

plot(df$Discharge, df$flux.mgs/10000)
lines(df$Discharge, p.ar1/10000, col='red')
lines(df$Discharge, p.ma1/10000, col='azure4')
lines(df$Discharge, p.ar2/10000, col='green')
lines(df$Discharge, p.lm/10000, col='pink', lwd='4')

plot(1:nrow(df), df$flux.mgs, type='l')
lines(1:nrow(df), p.lm , col='pink')
lines(1:nrow(df), p.ar1 , col='red')
lines(1:nrow(df), p.ar1.ma1, col='orange', lwd='2')

rmse(df$flux.mgs, p.lm)/10000
rmse(df$flux.mgs, p.ar1)/10000


#gls.model.1 = gls( flux.mgs ~ Discharge, df, correlation=corARMA(p=1, form= ~t | group))
#gls.model.2 = gls( flux.mgs ~ Discharge, df, correlation=corARMA(p=2, form= ~t | group))
