setwd('C:\\Users\\Mallori\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_09')
library(phytools)
AnolisTree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names (Lengths) <- AnolisTree$tip.label
names (Lengths) [which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
plot(AnolisTree,type="fan")
print(AnolisTree)
#Question 1: There are 82 tips with no branch lengths present.

data<-read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors =F, row.names=1)
head(data)
length(data)
row(data)
names(data)
max(data)
plot(data)

#Question 2: The "data" is a linear object with some data points relating to different species.
svl<-setNames (data$svl, rownames(data))
Ancestors<-fastAnc(AnolisTree, svl, vars=TRUE, CI=TRUE)
fit.BM<-fastAnc(AnolisTree, svl, CI=TRUE)
print(fit.BM,printlen=10)
#Question 3: The estimated values are stored in the lower and upper 95% CI. The CI95 decides whether to compute 95% confidence intervals on state estimates. 
#Question 4: 83 has a lower of 3.85 and an upper of 4.252, while 84 has a lower of 3.86 and a higher of 4.209.
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree,type="fan",lwd=2,show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj<-contMap(AnolisTree, svl, plot=F)

plot(obj,type="fan", legend=0.7*max(nodeHeights(AnolisTree)), sig=2, fsize=c(0.7, 0.9))
fossilData<- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger","Anolis_occultus","Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilNodes<-c()
nodeN<-c()
#Question 5: code below
for(i in 1:dim(fossilData)[1]) {
  Node<-fastMRCA(AnolisTree,fossilData[i, "tip1"], fossilData[i,"tip2"])
  fossilNodes[i]<-fossilData[i, "svl"]
  nodeN[i]<-Node
  }
names(fossilNodes)<-nodeN
Ancestors_withFossils <-fastAnc(AnolisTree,svl,anc.states=fossilNodes, CI=TRUE, var=TRUE)

plot(fossilData, nodeN)
#Question 7: Fossils change the ancestral sizes by decreasing size over time.

install.packages("geiger")
library(geiger)
#Question 8:
AncestorTreeFit<- fitContinuous(AnolisTree,svl)
svlFit <- fitContinuous(svl, data)
FossilNodesFit<- fitContinuous(fossilNodes, data)
#Question 9: The best fit model is AncestorTreeFit
print(fit.BM)
#Question 10: The model isn't different from what fastAnc shows. 
