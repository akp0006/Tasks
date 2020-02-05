trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
#The samples are different and the populations were also different. The first population was a random generation for the normal distribution with mean 5 while the second population was the same but with mean 4.
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandma
nrow(MatGrandma)
ncol(MatGrandma)
colnames(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus <- makeBaby (Brenda, Alan)
head(Focus)
Focus
ToMom <- length(grep("mom", Focus))/ length(Focus)
ToMomMom <- length(grep("grandma_mom", Focus))/ length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/ length(Focus)
ToDadMom <- length(grep("grandma_da", Focus))/ length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/ length(Focus)
#No, Focus is not equally related to each maternal grandparent nor to each paternal grandparent, which isn't what I expected. However, the average relatedness of Focus to all four grandparents is 25%.
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
MatGrandma2 <- makeFounder("grandma_mom2", len = 1e5)
MatGrandpa2 <- makeFounder("grandpa_mom2", len = 1e5)
PatGrandma2 <- makeFounder("grandma_da2", len = 1e5)
PatGrandpa2 <- makeFounder("grandpa_da2", len = 1e5)
Alan2 <- makeBaby(PatGrandma2, PatGrandpa2)
Brenda2 <- makeBaby(MatGrandma2, MatGrandpa2)
Focus2 <- makeBaby (Brenda2, Alan2)
ToMom2 <- length(grep("mom", Focus2))/ length(Focus2)
ToMomMom2 <- length(grep("grandma_mom2", Focus2))/ length(Focus2)
ToMomDad2 <- length(grep("grandpa_mom2", Focus2))/ length(Focus2)
ToDadMom2 <- length(grep("grandma_da2", Focus2))/ length(Focus2)
ToDadDad2 <- length(grep("grandpa_da2", Focus2))/ length(Focus2)
Sibling_02 <- makeBaby(Brenda2, Alan2)
ToSib2 <- length(intersect(Focus2, Sibling_02))/length(Focus2)
ManySiblings2 <- replicate(1e3, length(intersect(Focus2, makeBaby(Brenda2, Alan2)))/ length(Focus2))
quantile(ManySiblings2)
mean(ManySiblings2)
plot(density(ManySiblings2), main="", xlab="proportion shared genes")
#We see a range of values for the sibling and grandparent analyses because, although a child always receives 50% of his/her parent's genes, there's a differential inheritance of the grandparents' genes which results in the range of shared genes seen in a child's grandparents and siblings. This is because for each of the chromosomes a child inherits from a given parent, they have a 50% chance of getting the copy from their grandfather and a 50% chance of gaining the copy from their grandmother. And there is also the occurrence of recombination, which means that some chromosomes aren't purely from one grandparent or another but a mix of the two.
HWE <- function(p) {
	aa <- p^2
	ab <- 2 * p * (1 - p)
	bb <- (1 - p)^2
	return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#As the freq. of the a allele increases in the pop., the freq. of aa individuals increases. As the freq. of the a allele decreases in the pop., the freq. of aa individuals decreases. This plot does not show time nor geographic space.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Yes, the frequency of the aa genotype in my simulated population matches the expectation from Hardy-Weinberg.
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#In this plot, there are more outliers, and the data is less uniform to the line than before with the bigger population. This is due to the smaller population size, which pronounces the effects of genetic drift causing random changes in allele frequencies due to sampling error from one generation to the next.
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
y <- genetic.drift(Ne=20, nrep=5, pause=0.01)
z <- genetic.drift(Ne=2000, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples + 0)
plot(Samples, tExt)
abline(Line2)
#I notice that as the population size increases, the distance between the points and the line also generally increases. This violates the assumption of the linear model, which means that its results may not be accurate.

#Extra Credit
install.packages("robustbase")
library(robustbase)
Line3 <- lmrob(tExt ~ Samples)
plot(Samples, tExt)
abline(Line3)
#The estimate of the slope for Line (the 1st linear model), at 2.8443, is quite a bit higher than the estimate of the slope for Line3 (the robust linear model), at 1.707. I believe this is because the large amount of variability as the population size increased, particularly toward the top end of the graph, which pulled the 1st non-robust linear model upward, while the robust linear model was able to recognize and adjust for the variability.
