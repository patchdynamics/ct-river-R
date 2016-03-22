
ts.analyze = merge(tscopy, ts.stemp)
ts.analyze = ts.analyze[.indexmon(ts.analyze) < 6]

nls.model = nls(flux.mgs ~ y.intercept + constant * SoilTemperature1 * Discharge,
                data = as.data.frame(ts.analyze),
                start = list(y.intercept = 1.5, constant = 1/290 )
                )
summary(nls.model)

new.data = data.frame(Discharge = (0:50) * 2000, SoilTemperature1 = 270 + (0:50)*25/50)

z = outer(new.data$Discharge , new.data$SoilTemperature1 ,
           function(a, b) predict(nls.model , newdata = data.frame(Discharge = a, SoilTemperature1 = b)))


persp(new.data$Discharge, new.data$SoilTemperature1, z,
      , phi = 20, theta = 0,
      col= rainbow(6)[cut(z,6)],
      xlab='Discharge',
      ylab='SoilTemperature',
      zlab='HPOA.mgl',
      zlim=c(1,3),
      ylim=c(270,295)
)

library(lattice)
wireframe(Height ~ x*y, data = elevation.fit,
          

#persp(seq(10, 300, 5), seq(10, 300, 5), seq(10, 300, 5), phi = 45, theta = 45,
#      xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#      main = "Surface elevation data"
#)
