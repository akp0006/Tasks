source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim = c(0, 1), type = "l", xlab = "generation", ylab = "allele freq.", lwd = 2)
lines(1:nrow(Pop1), Pop1[,2], lwd = 2, col = 'red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty = "n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed)^2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text = c("expected", "observed"))
Observed <- c(5, 0, 0, 35)
Chisq <- sum(((Expectation - Observed)^2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text = c("expected", "observed"))
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_05")
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
results[1:20,]
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
counts[1:20,]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
Avg <- mean(Chisqs)
#If this were a straightforward statistical test, given our calculated chi-squared value of 60 and the critical value of 11.70, this would be deemed a significant result.
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
#The chi-squared differs somewhat by background color, with red and white backgrounds having the highest (74.5) and lowest (49.3) chi-squared values, respectively.
propSig <- length(which(Chisqs > 11.70)) / length(Chisqs)
percSig <- round(100 * propSig)
#The high percentage of trials that had a "signigcant" p-value (92%) was surprising. And no, I don't think that natural selection is the only thing driving that high percentage.
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
hist(Chisqs, main = "", xlab = "chi-squared values", ylab = "frequency")
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
plot(1, 1, xlim = c(0, 400), ylim = c(1, 8.5), xlab = "", ylab = "", type = "n", yaxt = "n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side = 1, expression(chi^2), cex = 1.75, line = 2.5)
counter <- 1
for (i in backgrounds) {
	Data <- Chisqs[which(results[,3] == i)]
	addHist(Y = counter, Dat = Data, Color = backgroundCol[counter])
	counter <- counter + 1
}
abline(v = 11.70, lty = 2, lwd = 2, col = 'black')
#I couldn't see as much meaningful difference between backgrounds than I thought I would. The only somewhat apparent thing was that the white background seemed to have the least number of significant trials due to having less data right of the line than the others. Also, I noticed that the red background had the highest frequency of extreme (~300) chi-squared values.
Simulation <- simDraws(10000)
addHist(Y = 7, Dat = Simulation, Color = "lightgray")
mtext(side = 2, at = 7, line = 0, "simulated")
length(which(Simulation > 11.70)) / length(Simulation)
#Now simulating actual selection
#1st scenario (no fitness differences between the 6 toothpick types)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation2, Color = rgb(0, 0, 0, 0.25))
#2nd scenario (1 toothpick type selected against)
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation3, Color = rgb(0, 0, 0, 0.25))
#3rd scenario (3 toothpick types selected against)
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation4, Color = rgb(0, 0, 0, 0.25))
#4th scenario (5 selected against)
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation5, Color = rgb(0, 0, 0, 0.25))
#5th scenario (insane selection)
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation6, Color = rgb(0, 0, 0, 0.25))
mtext(side = 2, at = 8, line = 0, "sel. sim.")
#
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y = 8, Dat = Simulation7, Color = rgb(0, 0, 1, 0.25))
#The histogram of the mixture/combination of selection simulations looks quite similar to that of the student-generated data. Given how the mixture was generated, I'd say that most student groups do not actually show evidence of strong selection (most distributions would largely fit into the sel. simulated distribution of 3 toothpick types selected against).
#Inference Questions
# Natural selection (kind of)
# Natural selection
# The graphs tell us that the relative srength of selection varied greatly.
# Comparing the student numbers to the simulated numbers tells us more about the processes occurring here.
# Adding the possibility for a toothpick to mutate into a different type would increase the chi-squared values.
