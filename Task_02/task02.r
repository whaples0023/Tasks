setwd('C:\\Users\\Mallori\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02')
# Read this file off the internet
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3,4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] =='bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
#There are six rows now and each row represents the duration of drinking milk.
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
#These are all the same ways to finding the bottle events because the commands are the same. The first one takes you step by step in accessing the bottle events, while the second one chooses to access it throught the event, and the third uses the dollar sign which is just another command to access.
dayID <- apply(Data, 1, function (X) paste (x [1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin =" 2019-04-18")
Data$age <- dateID - dateID [which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2 [order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
