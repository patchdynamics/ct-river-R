#library(Evapotranspiration)

tau.series = 1:12
#Ttau = cbind() # vector of average monthly temperatures, len = 12

# First calculate the fourier coefficients
Ta = sum(Ttau) / 12
c1 = sum( (Ttau - Ta) * cos(pi * tau.series / 6)  ) / 6
d1 = sum( (Ttau - Ta) * sin(pi * tau.series / 6)  ) / 6
c2 = sum( (Ttau - Ta) * cos(pi * tau.series / 3)  ) / 6
d2 = sum( (Ttau - Ta) * sin(pi * tau.series / 3)  ) / 6

ETa = 1.25 + 0.118 * Ta
a1 = 1.62 + -0.00843 * Long + -0.0860 * Ta + 0.191 * c1 + -0.564 * c2
b1 = 2.59 + -0.0137  * Long + -0.0318 * Ta + 0.269 * d1 + -0.190 * d2
a2 = -0.637 + 0.00761 * Long + -0.000095 * Elev + -0.0573 * c1 + 0.0374 * d1 + 0.404 * c2
b2 = -0.708 + 0.00409 * Long + -0.000131 * Elev + -0.0403 * c1 + -0.039 * d1 + 0.271 * c2 + 0.205 * d2

ET = ETa + a1 * cos( pi * tau / 6 ) + b1 * sin ( pi * tau / 6 ) +
  a2 * cos( pi * tau / 3 ) + b2 * sin ( pi * tau / 3)

