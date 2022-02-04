source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop (Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot (1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), xlab="generation", ylab="allele freq.",lwd=2)
lines(1: nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd=2, bty="n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p= 0.5, h=1, s=0)       
Expectation <- c(10,10,10,10)       
Observed <- c(15,15,5,5)
Chisq <- sum(((Expectation - Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
Observed <- c(10,10,10,10)
Chisq <- sum(((Expectation - Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~. (Chisq)), legend.text=c("expected", "observed"))
#When you set the observed to 10 while the expectation is also at ten them the x^2 is 0. When you said the observed at 40 the x^2 is 360. Therefore if expectations are the same number as observed them there isn't a difference in x^2.
results <-read.csv("http://jonsmitchell.com/data/biol112labresults.csv",stringsAsFactors=F)
length(data)
nrow (data)
ncol(data)
colnames(data)
head(data)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#They aren't even when Chisqs high but more even when Chisqs is low. It shows that the lower the Chisqs the more similiarity between samples.
Avg <- mean(Chisqs)
#The chisqs is higher than the critical value. The chisqs also differs by background.
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70) / length(Chisqs))
#Yes
percSig <- round(100 * propSig)
#No there may also be error in the selection.
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist (Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar = c(4,4,1,1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis =1)
plot(1,1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at= 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for( i in backgrounds) {
  Data <- Chisqs[which(results [,3] == i)]
  addHist (Y=counter, Dat=Data, Color= backgroundCol[counter])
  counter <- counter + 1
}
abline( v= 11.70, lty=2, lwd = 2, col = 'black')
#The backgrounds don't have much difference between them. 
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line = 0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
#60%
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w= Fit)
addHist (Y=8, Dat=Simulation3, Color=rgb (0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
Simulation4 <- simDraws(1e4, w= Fit)
addHist (Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist (Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#The mixture has similar values to each point, where as the students values seem more all over the place.
#Most students show strong selection when compared to the simulation.
#The students show more genetic drift over the years in the evolutionary process.
#The evolutionary process done by lab students over the years is genetic drift.
#The evolutionary process done by the simulation is natural selection.
#The relative strength isn't strongly related to the simulation because the data on the graph from the lab students is scattered.
#Comparing the students to simulated numbers tells us more about the process being done than the critical value.
#The x^2 would increase because the expected number and the observed number would be highly different with mutations.