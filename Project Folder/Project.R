setwd('C:\\Users\\Mallori\\OneDrive\\Desktop\\Evolution\\Tasks\\Project Folder')
Data2<-read.csv('BlackRObinData2.csv')
head(Data2)
plot(Data2$ll, Data2$Fa, xlab="Recessive Allele Frequency in Offspring", ylab="Recessive Allele Frequency in Ancestors", pch=16)
abline(lm(Data2$Fa~Data2$ll)) 
cor.test(Data2$ll,Data2$Fa)
         