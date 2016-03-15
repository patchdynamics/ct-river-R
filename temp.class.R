
l = tslval
l[tslval$Temperature >= tshval$Temperature] = NA
plot(l)
ts.temperature.class = tshval
ts.temperature.class[!is.na(l)] = l[!is.na(l)]
plot(ts.temperature.class)

par(mfrow=c(2,1))
plot(ts.temperature.class)
plot(tscopy$flux.mgs)