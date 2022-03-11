library(phytools)
library(ape)
text.string<-
  "(((((((cow,pig),whale),bat(,(lemur,human))),(robin,iguana)),coelacanth),(gold_fish,trout)),shark);"
vert.tree<-read.tree (text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#Question1: human because they both come from node 14?
#Note to self - A good place to practice commands for phylogeny tree

vert.tree
#Question2: There are no branch lengths.
str(vert.tree)
tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge

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

?plot.phylo
#Question3: plot(Some.Data, show.tip.label=FALSE)
#Question4: plot(Some.Data, type= "fan")
#Question5: plot(Some.Data, tip.color=c("red",some number)
AnolisTree$edge
AnolisTree$tip.label
ShortestEdge <- which.min(Lengths)
which.min(Lengths)
#Question6:Anolis_occultus has the shortest edge length
#Question7: The code below this line
AnolisTree2 <-drop.tip(AnolisTree, tip = ShortestEdge)
#Question8: The code below this line
plot(AnolisTree2, cex = 0.25)

ltt(AnolisTree)
abline (0, 1, lwd=2, col='red', lty=2)
#Question9: This line never goes down because the number of lineages are increasing. The slope is always the same, and these lizards are reproducing at a constant rate.

?fit.bd()
data<-AnolisTree
lizards <- (data)
sampling.f<-Ntip.phylo(lizards)/0.2
bd.fit<-fit.bd(lizards, rho =sampling.f)
print(bd.fit)

#Question10: ML(b/lambda) = 0.001187 ML(d/mu) = 0 log(L) = 101.5265 

#Extra Credit
library(treebase)
data(treebase)
head(treebase)
cache_treebase(file=paste("treebase",Sys.Date(), "rda",sep = ""),pause1 = 3, pause2 = 3, attempts = 10, max_trees = Inf, only_metadata = FALSE, save =TRUE)
search_treebase( by year)
