library(phytools)
trees<- list()
births <- vector()
Fractions <- vector()
netdiversification <- vector()
speciationrate <- vector()
Avgbranchlength <- vector()
for (i in 1:100) {
  births[i] <- runif(1,0,1)
  Fractions[i] <- runif(1,0,1)
  trees[[i]] <- pbtree(n=100, b=births[i], d=Fractions[i]* births[i])
  netdiversification[i] <- (births[i] - Fractions[i]* births[i])
speciationrate[i] <- births[i]
Avgbranchlength[[i]] <- mean(trees[[i]] $edge.length)
}
netdiversification <- (births - Fractions*births)
totaltreetips <- log(sapply(trees, Ntip))
plot(netdiversification, totaltreetips)
#Question4: As net diversification rate grows the total tree tips begin to decrease and are more consistant.

plot(speciationrate, Avgbranchlength)
#Question5: Speciation rate increases as the average branch lengths go down.

#Question6:
cor(speciationrate, Avgbranchlength)

#Question7:
trees
tree[73]
Tree <- trees[[73]]
rates <- vector()
traits <- list()
plot(Tree)
for (i in 1:100){
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(Tree, sig2=rates[i])
}
#Question8: There is no correlation between the means of traits and rates. Why I am correct is because as rate decreases it has no effect on traits, and traits shows no effect on rate.
head(traits)
length(traits)
mtraits <- sapply(traits, mean)
mtraitsratecor <- cor(mtraits, rates)
plot(mtraits, rates)
dev.off()

#Question9: 
element1 <- sapply(traits, "[[", 1)
element2 <- sapply(traits, "[[", 2)
traitMat <- cbind(element1, element2)
element1and2cor <- cor(element1, element2)
plot(element1, element2)
dev.off()
#Question10P1: There is a positive correlation between element 1 and element 2. The correlation is due to the relatedness in the phylogeny tree.
#Question10P2: As the elements of the traits are randomly generated, there is always a positive correlation, so the correlation is not significant. As a result, all traits are positively correlated in some way. 

#ExtraCredit:
TraitTree <- pbtree(n=100)
X <- fastBM(TraitTree, nsim=2)
phylomorphospace(TraitTree,X, xlab= "element1", ylab= "element2")
