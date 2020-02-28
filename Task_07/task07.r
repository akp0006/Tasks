source("http://jonsmitchell.com/code/reformatData07.R")
head(alleleFreqs)
alleleFreqs[1:20,]
alleleFreqs[30:50,]
plot(1, 1, type = "n", xlim = c(1998, 2013), ylim = c(0, 1))
overallFreq[,1:5]
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col = rgb(0,0,0,0.01)))
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x) x - x[1])
plot(1, 1, type = "n", xlim = c(1998, 2013), ylim = c(-0.25, 0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col = rgb(0,0,0,0.01)))
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100)
source("http://jonsmitchell.com/code/simFxn.R")
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 1, s = 0.20, startT = 1997, simCol = "gray40", rescale = TRUE)
#verrrrry pos. skewed
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.50, s = 0.05, startT = 1997, simCol = "gray40", rescale = TRUE)
#too much on pos. side
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.10, s = 0.05, startT = 1997, simCol = "gray40", rescale = TRUE)
#still too much on pos. side
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 50, ngens = 18, h = 1, s = 0, startT = 1997, simCol = "gray40", rescale = TRUE)
#most all over the place (due to lower pop. size and thus higher genetic drift)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 1, s = -0.04, startT = 1997, simCol = "gray40", rescale = TRUE)
#too much on neg. side still
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.07, s = -0.04, startT = 1997, simCol = "gray40", rescale = TRUE)
#still too neg.
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 90, ngens = 18, h = 0.05, s = -0.02, startT = 1997, simCol = "gray40", rescale = TRUE)
#too wild, even with only 10 difference in pop. size and less h and s.
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 95, ngens = 18, h = 0.02, s = -0.02, startT = 1997, simCol = "gray40", rescale = TRUE)
#still too wild, even with only 5 difference in pop. size and even less h. 
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.02, s = -0.02, startT = 1997, simCol = "gray40", rescale = TRUE)
#best one yet
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.02, s = -0.01, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.025, s = -0.02, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.04, s = -0.01, startT = 1997, simCol = "gray40", rescale = TRUE
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.05, s = -0.01, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.05, s = -0.015, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.06, s = -0.015, startT = 1997, simCol = "gray40", rescale = TRUE)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit(nruns = 50, n = 100, ngens = 18, h = 0.02, s = -0.01, startT = 1997, simCol = "gray40", rescale = TRUE)
# The combinations of n, h and s that produces changes most comparable to what is seen in the Scrub-Jay population are ones where no selection takes place (n=100, h=1, s=0) and seemingly ones where a very very small amount of negative selection occurs(n=100, h=0.02, s=-0.01/0.02). This suggests that the Scrub-Jay population experiences little to no selection, which would then suggest that most changes in allele frequency are due to nonadaptive evolutionary processes such as genetic drift.
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim = c(-0.15, 0.15), xlab = "overall freq. change", ylab = "freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col = 'blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col = 'red')

