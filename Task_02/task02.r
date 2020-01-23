setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
write.csv(Data, "rawdata.csv", quote=F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Data[257, 1:3]
Feeds <- which(Data[,9] == "bottle")
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,"event"] == "bottle")
Feeds <- which(Data$event == "bottle")
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == "birth")]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(beren2)
head(beren3)
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_02")
beren3 <- read.csv("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_02\\beren_new.csv", stringsAsFactors=F)
beren3
#Question1: The first hypothesis is inappropriate for the given data for many reasons. First of all, Beren's weight is not regularly recorded, especially after August 19th. There is also the fact that there is no recroded quantitative data for how much he eats (only how many oz he drinks from the bottle). Therefore, a comparison of how much Beren eats each day and how much he weighs is not testable given the data. The second hypothesis is not testable because it is too vague--the presence of a "relationship" is impossible to test.
Feeds <- which(beren3[,9] == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
length(avgFeed)
class(avgFeed)
dim(avgFeed)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
totalFeed
numFeeds
varFeed
?cor
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
options("device")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question2: Because the milk data was recorded at various times of day, multiple times per day while he was in daycare. The graph only shows the total milk per day and it doesn't account for the time of day, how much he had at each feeding, or how long he was actually in daycare to be fed.
source("http://jonsmitchell.com/code/plotFxn02b.R")
Naps <- which(beren3[,9] == "nap")
beren4 <- beren3[Naps,]
beren4
class(beren4)
napstart <- beren4[,5]
napstartmin <- beren4[,6]
NStimestamp <- paste(napstart, ":", napstartmin, sep="")
NStimestamp
sapply(strsplit(NStimestamp,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
napend <- beren4[,7]
napendmin <- beren4[,8]
NEtimestamp <- paste(napend,":", napendmin, sep="")
sapply(strsplit(NEtimestamp,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
#I had to re-do this part because I didn't remove the rows with NAs (in the relevant columns) first.
beren5 <- beren4
beren4[complete.cases(beren4[ , 7:8]),]
napstart2 <- beren4[,5]
napstartmin2 <- beren4[,6]
NStimestamp2 <- paste(napstart2, ":", napstartmin2, sep="")
NStimestamp2
sapply(strsplit(NStimestamp2,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
napend2 <- beren4[,7]
napendmin2 <- beren4[,8]
NEtimestamp2 <- paste(napend2,":", napendmin2, sep="")
sapply(strsplit(NEtimestamp2,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
#and again because I forgot to add the beren4 changes to an object...
beren6 <-  beren4[complete.cases(beren4[ , 7:8]),]
napstart3 <- beren6[,5]
napstartmin3 <- beren6[,6]
NStimestamp3 <- paste(napstart3, ":", napstartmin3, sep="")
NStimestamp3
NS_dec <- sapply(strsplit(NStimestamp3,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
napend3 <- beren6[,7]
napendmin3 <- beren6[,8]
NEtimestamp3 <- paste(napend3,":", napendmin3, sep="")
NE_dec <- sapply(strsplit(NEtimestamp3,":"),
	function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
	}
)
NE_dec - NS_dec
timeeachnap <- NE_dec - NS_dec
beren7 <- cbind(beren6, timeeachnap) 
totalNap <- tapply(beren7$timeeachnap, beren7$age, sum)
plot(as.numeric(names(totalNap)), totalNap, type="b", pch=16, xlab="age in days", ylab="total time slept (hours)")
beren8 <- cbind(beren7, NS_dec) 
berenCorr <- cor.test(beren8$timeeachnap, beren8$NS_dec)
#Step8: Given the p-value of 0.002, the test suggests that the time the nap starts and the nap's duration are significantly correlated. The correlation coefficient of -0.28 suggests that the correlation isn't particularly strong and that these two variables are negatively correlated--as one variable increases, the other decreases. 