source("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(0,1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0,0,0,0.01)))
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x) x-x[1])

#This graph shows the frequencies over time. The dark gray shows less changes as they grow further out into light gray there are changing.
plot(1,1, type="n",xlim=c(1998, 2013), ylim=c(-0.25, 0.25) )
s <- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col= rgb(0,0,0,0.01)))
dYear <-c()
dAlleles <-c()
for (i in 3:ncol(overallFreq)) {
  dYear <- c(dYear, overallFreq[,1])
  Vec <- overallFreq[,i]
  Init <- overallFreq[1,i]
  dAlleles <- c(dAlleles, Vec - Init)
}

#This graph shows the frequncies over time but in color so it's easier to tell them apart. As time goes on the frequecnes change colors from red to indicate they're changing.
smoothScatter(dYear, dAlleles, colramp=Pal, nbin=100)
smoothScatter(dYear, dAlleles, colramp = Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns = 80, n = 125, ngens = 18, startT = 1997, simCol = "gray40", rescale = TRUE)
#This now shows the frequencies but with the gray lines accurately representing the different frequencies. 

#This plot is showing the changes in frequencies within the scrub jays population but the blue is showing death rate, the red is showing birth rate, and the black is showing migration.
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')
#Migration is the strongest evolutionary force in this data shown in this graph, then death rate and then birth rate.
