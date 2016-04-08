
a = 1.5
b = .5
x = 1:50
y_det = a + b*x
points = rnorm(50, mean = y_det, sd=2)

plot(x, points)

df = data.frame(x = x, y=points)
lm.model = lm(y ~ x, df)
lm.model

lm.poly.model = lm(y ~ poly(x,1), df)
lm.poly.model

lm.raw.model = lm(y ~ poly(x,1, raw=TRUE), df)
lm.raw.model



lm.avgs = lm(var.mean ~ poly(discharge.mean, 1), data=avgs)

lm.avgs.raw = lm(var.mean ~ poly(discharge.mean, 1, raw=TRUE), data=avgs)
