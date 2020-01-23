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