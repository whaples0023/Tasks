install.packages("paleabioDB", dep=T)
"install.packages.compile.from.source"

#Future note you have to use this command to install packages
install.packages(type="paleabioDB")
library(paleobioDB)
setwd('C:\\Users\\Mallori\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_07')
canidae<-  pbdb_occurrences (limit="all", base_name="canidae",interval="Quaternary", show=c("coords", "phylo", "ident"))
Data <- read.csv('pbdb_data.csv')
head(Data)
nrow(Data)
length(Data)
ncol(Data)
Data[1,]
Data[2,]
