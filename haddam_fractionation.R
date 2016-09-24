setwd('/Users/matthewxi/Documents/Projects/PrecipGeoStats/R')

fractionation = read.csv('../Haddam/HaddamFractionation.csv')
fractionation$HPOA_Percentage = as.numeric(substring(as.character(fractionation$HPOA..),1,2))/100
fractionation$HPOA_ppm = fractionation$DOC..ppm.C. * fractionation$HPOA_Percentage
fractionation$HPI_Percentage = as.numeric(substring(as.character(fractionation$HPI..),1,2))/100
fractionation$HPI_ppm = fractionation$DOC..ppm.C. * fractionation$HPI_Percentage
fractionation$TPIA_Percentage = as.numeric(substring(as.character(fractionation$TPIA..),1,2))/100
fractionation$TPIA_ppm = fractionation$DOC..ppm.C. * fractionation$TPIA_Percentage

plot(fractionation$DOC..ppm.C., fractionation$HPOA_Percentage)
plot(fractionation$DOC..ppm.C., fractionation$HPOA_Percentage, ylim=c(0,1))


# check for rise in HPOA % with DOC
par(mfrow=c(1,1))
lm = lm(HPOA_Percentage~DOC..ppm.C., fractionation)
plot(fractionation$DOC..ppm.C., fractionation$HPOA_Percentage, 
     ylim=c(0,1), xlab='[DOC]',  ylab='HPOA Percentage', col='blue')
abline(lm, col='red')
legend("topright", bty="n", legend=paste("R2 =", 
    format(summary(lm)$adj.r.squared, digits=4)))

plot(fractionation$DOC..ppm.C., fractionation$HPOA_ppm)

# HPI vs HPOA
plot(fractionation$DOC..ppm.C., fractionation$HPOA_ppm, col='blue', 
     ylim=c(0,3), xlab='[DOC] ppm', ylab='ppm')
points(fractionation$DOC..ppm.C., fractionation$HPI_ppm, col='orange')
legend("topleft", c("HPOA", "HPI"), pch=1, col=c('blue', 'orange'))


##  Adding in the Remainder calculation
fractionation$Remainder.ppm = fractionation$DOC..ppm.C. - 
  (fractionation$HPOA_ppm + fractionation$HPI_ppm + fractionation$TPIA_ppm)
points(fractionation$DOC..ppm.C., fractionation$Remainder.ppm, col='red')

points(fractionation$DOC..ppm.C., fractionation$TPIA_ppm, col='green')


#HPI
plot(fractionation$DOC..ppm.C., fractionation$HPI_Percentage, 
     ylim=c(0,1), xlab='[DOC] ppm', ylab='HPI Percentage', col='blue')
lm.HPI = lm(fractionation$HPI_Percentage~fractionation$DOC..ppm.C.)
abline(lm.HPI, col='red')
legend("topright", bty="n", legend=paste("R2 =", 
    format(summary(lm.HPI)$adj.r.squared, digits=4)))

# HPI no trend in ppm
lm.HPI_ppm = lm(fractionation$HPI_ppm~fractionation$DOC..ppm.C.)
plot(fractionation$DOC..ppm.C., fractionation$HPI_ppm, col='blue')
abline(lm.HPI_ppm, col='red');
legend("topright", bty="n", 
       legend=paste("R2 =", format(summary(lm.HPI_ppm)$adj.r.squared, digits=4)))


# TPIA
plot(fractionation$DOC..ppm.C., fractionation$TPIA_Percentage)

plot(fractionation$DOC..ppm.C., fractionation$TPIA_Percentage, ylim=c(0,1))
lm.TPIA = lm(fractionation$TPIA_Percentage~fractionation$DOC..ppm.C.)
abline(lm.TPIA)
legend("topright", bty="n", legend=paste("R2 =", 
                                         format(summary(lm.TPIA)$adj.r.squared, digits=4)))


