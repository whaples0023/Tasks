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
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin ="2019-04-18")
Data$age <- dateID - dateID [which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2 [order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
#The units are in ounces.The value tells us what was measured and we put Feeds into brackets so the computer knows to pull it from that section of the data.
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')    
pdf("r02b-totalMilkByDay.pdf",height=4, width=4)
par(las=1,mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)),totalFeed, type="b", pch=16, xlab="abline (h=mean(totalFeed), lty=2, col='red'")
#Question2: The graph is hard to interpret because the points are too close together. 