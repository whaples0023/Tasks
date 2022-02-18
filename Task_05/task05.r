library(learnPopGen)
#install.packages(learnPopGen)
coalescent.plot()
coalescent.plot(n=30,ngen=10,col.order="alternating")
object<-coalescent.plot()
print(object)
plot(object)
#The simulation begins with eight alleles. You can modify that by changing the n to a lower number because that will lower the number of gene copies.
#It takes on average about the fourth or fifth generation.
#
#Fitness plays an important role because the more offspring an organism is having them the more likely ther alleles are being passed on.
#Yes, they are alive in generation 0.
#install.packages(coala)
#install.packages(phytools)
library("coala")

