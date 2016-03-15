
ts.massflow = ts$Discharge * ts$HPOA.mgl

names(ts.massflow) = "MassFlow"
ts.massflow = merge(ts, ts.massflow)

tswinter = ts.massflow[.indexmon(ts.massflow) < 5]
tssummer = ts.massflow[.indexmon(ts.massflow) > 5 & .indexmon(ts.massflow) < 9]

winter = as.data.frame(tswinter)
summer = as.data.frame(tssummer)

par(mfrow=c(2,1))
maxY = max( rbind(max(na.locf(winter$MassFlow)), max(na.locf(summer$MassFlow))))
maxX = max( rbind(max(na.locf(winter$Discharge)), max(na.locf(summer$Discharge))))

plot(winter$Discharge, winter$MassFlow, xlim=c(0,maxX), ylim=c(0,maxY), col='purple') 
plot(summer$Discharge, summer$MassFlow, xlim=c(0,maxX), ylim=c(0,maxY), col='orange') 

winter$season = 'winter'
summer$season = 'summer'
massflow = rbind(winter, summer)
massflow = massflow[!is.na(massflow$Discharge) & 
                      !is.na(massflow$MassFlow) &
                      massflow$Discharge > 0,]
ggplot(massflow, aes(x=Discharge, y=MassFlow, group=season, color=season)) + 
  geom_point() +
  geom_smooth(method=lm, formula=y ~ exp(x / 100000)) 

winter.df = as.data.frame(winter)
#winter.df = winter.df[winter.df$Limb != 0]
winter.df.sample = winter.df[sample(nrow(winter.df), .25 * nrow(winter.df)),]
lm.summer = lm(MassFlow ~ exp(Discharge / 100000), winter.df.sample,na.action=na.omit  )
summary(lm.summer)$adj.r.squared

lm.summer = lm(MassFlow ~ Discharge / 100000, winter.df.sample,na.action=na.omit  )
summary(lm.summer)