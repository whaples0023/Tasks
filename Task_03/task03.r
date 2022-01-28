setwd('C:\\Users\\Mallori\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_03')
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
#Yes the populations are different population one has a higher average than population two
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder ('grandpa_mom')
PatGrandma <- makeFounder ("grandma_da")
PatGrandpa <- makeFounder ("grandpa_da")
head(MatGrandma)
nrow(MatGrandma)
head(MatGrandpa)
nrow(MatGrandpa)
head(PatGrandma)
nrow(PatGrandma)
head(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
#The number should be 50% because that is how much you inherit from each parent.
ToMom <- length (grep("mom", Focus))/ length(Focus)
#These numbers should be 25%
ToMomMom <- length (grep("grandma_mom", Focus))/ length (Focus)
ToMomDad <- length (grep("grandpa_mom", Focus))/ length (Focus)
ToDadDad <- length (grep("grandma_da", Focus))/ length (Focus)
ToDadMom <- length (grep("grandma_mom", Focus))/ length (Focus)
#Focus is not equally related to any grandparent. I was not expecting this, I was expecting a 25% relation.
Sibling_01 <- makeBaby (Brenda, Alan)
#I expect the amount to be 50%
ToSib <- length (intersect(Focus, Sibling_01))/length(Focus)
ManySiblings <- replicate (1e3, length(intersect (Focus, makeBaby(Brenda, Alan)))/length (Focus))
quantile (ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab=" proportion shared genes")
#This difference could be due to the possibility of them getting 50% from the parents but its not exactly the same genes they all inherit, so the percents of shared genes are different between them.
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1,1, type="n", xlim=c (0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p,HWE))
lines (p, GenoFreq[,"aa"], lwd=2, col="red")
#This graph shows that as the frequency of allele a increases then the genotype increases as well. When allele a frequency is low then so is the genotype frequnecy. Time and geogaphic space is not shown in the plot.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red", "purple", "blue"), lty = 1, lwd = 2, bty= "n")
Pop <- simPop(500)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#No it doesn't match instead the ones from Hardy-Weinberg are cluster together towards the center of the predicted line.
Pop <- simPop(50)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/50, pch= 22, bg="red")
#The points are now more spread out across the aa line. I think this because in a smaller population the frequency is more relevent because less individuals have the aa allele.
install.packages("learnPopGen")
library(learnPopGen)
x<- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples + 0)
abline(Line2)
#Both of these lines are almost equal to eachother.The better line to use would be the second line because it is not offer more samples to enter the population. The points that are close to the line will more likely to become extinct due to the grap showing as samples grow extinction occurs.