
tscombined$HPI.Percent.Predicted = predict(lm.hpi.discharge, tscombined)
tscombined$HPOA.Percent.Predicted = predict(lm.hpoa.discharge, tscombined)
tscombined$DOM = tscombined$HPOA.mgl / tscombined$HPOA.Percent.Predicted
tscombined$HPI.ppm = tscombined$DOM * tscombined$HPI.Percent.Predicted


library(ggplot2)
ts = tscombined['2011-01-01/2015-12-31']
ts.df = as.data.frame(ts)
ts.df = ts.df[!is.na(ts.df$HPOA.mgl) & !is.na(ts.df$HPI.ppm) & !is.na(ts.df$TPIA.mgl),]
ts.df$Time = index(ts)
ts.df.long = reshape(ts.df, 
                     varying=c( 'HPI.ppm', 'TPIA.mgl', 'HPOA.mgl'), 
                     v.names='Concentration', 
                     timevar='DOM', 
                     times=c('HPI', 'TPIA', 'HPOA' ), 
                     direction='long')

t = ggplot(ts.df.long, aes(
  x = id,
  y = ts.df.long$Concentration,
  fill = DOM
))   + geom_area(position = 'stack')
t + xlab('Time') + ylab('Concentration ppm')
