#Calculando el parámetro Hurts de la siguiente traza:

library(fArma)
library(longmemo)

data(ethernetTraffic)
x<-ethernetTraffic

#Ethernet traffic data from a LAN at Bellcore, Morristown (Leland et al. 1993, 
#Leland and Wilson 1991). The data are listed in chronological sequence by row.

par(mfrow = c(2, 2))
#Aggregated variance method
aggvarFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
          doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
#Differenced aggregated variance method
diffvarFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
           doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
#aggregated absolute value (moment) method
absvalFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), moment = 1,
          doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
#Higuchi’s or fractal dimension method
higuchiFit(x, levels = 50, minnpts = 2, cut.off = 10^c(0.7, 2.5),
           doplot = TRUE, trace = FALSE, title = NULL, description = NULL)

par(mfrow = c(2, 2))
#Peng’s or variance of residuals method
pengFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
        method = c("mean", "median"),
        doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
#R/S Rescaled Range Statistic method
rsFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
      doplot = TRUE, trace = FALSE, title = NULL, description = NULL)
#periodogram method
perFit(x, cut.off = 0.1, method = c("per", "cumper"),
       doplot = TRUE, title = NULL, description = NULL)
#boxed (modified) periodogram method
boxperFit(x, nbox = 100, cut.off = 0.10,
          doplot = TRUE, trace = FALSE, title = NULL, description = NULL)


#Interactive Display of Hurst Estimates, de los primeros 7 modelos
hurstSlider(x = fgnSim())

#waveletFit wavelet estimator.
waveletFit(x, length = NULL, order = 2, octave = c(2, 8),
           doplot = TRUE, title = NULL, description = NULL)
