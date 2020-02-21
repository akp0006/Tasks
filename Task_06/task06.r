library(learnPopGen)
?coalescent.plot
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_06")
Cplot_default <- coalescent.plot()
plot(Cplot_default)
coalescent.plot(n=8, ngen=25)
coalescent.plot(n=10, ngen=25) || * 15 [264/15 = 18 gens average till fixation]
coalescent.plot(n=5, ngen=25) || * 15 [136/15 = 9 gens average till fixation]
# Q1: By default, each simulation begins with 10 alleles. You modify that by using the 'n' argument in the coalescent.plot function. For example, if I wanted to start with 15 alleles, I'd enter "coalescent.plot(n = 15)" into R.
# Q2: The average # of generations for an allele to go to fixation depends on the # of alleles that were present at the beginning, at Generation 0. Based on 15 simulations each for 10 alleles at Generation 0 (avg. 18 gens till fixation) and 5 alleles at Generation 0 (avg. 9 gens till fixation), a very loose generalization for this data could be that it takes around 2 generations per # of beginning alleles for one of those alleles to go to fixation,  but there is no reliable all-encompassing average # of generations that applies to every situation.
# Q3: Average number of offspring per individual is about 1, though there are individuals who have no offspring, those with 2-3, and I believe one with 4 offspring.
# Q4: Fitness doesn't play any role as the simulation only models genetic drift.
# Q5: No.
install.packages("coala")
install.packages("phytools")
library("coala")
library("phytools")
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
	feat_mutation(10) +
	feat_recombination(10) +
	sumstat_trees() +
	sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
Diversity
# Looking at the Diversity object, all the values are not the same. The differences are caused by differences in genetic diversity between individuals, specifically the average number of differences per locus between any two individuals.
Nloci <- length(stats$trees)
t1 <- read.tree(text = stats$trees[[1]][1])
plot(t1)
axisPhylo()
# Q6: Because each individual is diploid, so the 5 individuals simulated have 2 copies of each locus (ex. so for Locus 1, there are 10 copies present today because each of the 5 individuals has 2 copies of Locus 1).
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text = stats$trees[[2]][1])
plot(t2)
axisPhylo()
Age2 <- max(nodeHeights(t2))
# The MRCA for the first SNP of the second locus is age 0.67, which is not the same age as the first SNP of the first locus (age = 0.25).
par(mfrow = c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
#Q7: No, they do not match.
compare.chronograms(t1, t2)
t1_1 <- read.tree(text = stats$trees[[1]][1])
t1_2 <- read.tree(text = stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text = stats$trees[[locus]][n])
		}
		else {
			outPhy <- ape:::c.phylo(outPhy, read.tree(text = stats$trees[[locus]][n]))
		}
	}
}
par(mfrow = c(1,1))
densityTree(outPhy)

model2 <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
	feat_mutation(10) +
	feat_recombination(1.5) +
	sumstat_trees() +
	sumstat_nucleotide_div()
stats2 <- simulate(model2, nsim = 1)
Nloci2 <- length(stats2$trees)
for (locus in 1:Nloci2) {
	ntrees2 <- length(stats2$trees[[locus]])
	for (n in 1:ntrees2) {
		if (locus == 1 && n == 1) {
			outPhy2 <- read.tree(text = stats2$trees[[locus]][n])
		}
		else {
			outPhy2 <- ape:::c.phylo(outPhy2, read.tree(text = stats2$trees[[locus]][n]))
		}
	}
}
par(mfrow = c(1,1))
densityTree(outPhy2)

model3 <- coal_model(10, 50) +
	feat_mutation(par_prior("theta", sample.int(100,1))) +
	sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
Line <- lm(mean_pi ~ theta)
plot(theta, mean_pi)
abline(Line)

#Extra Credit
activate_msms(download = TRUE)
activate_msms(jar = "C:\\Users\\Abbey\\AppData\\Local\\Temp\\Rtmp0SI8ul\\msms.jar", java = "C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath\\java.exe")
ECmodel <- coal_model(sample_size = c(10,40), loci_number = 50) +
	feat_mutation(10) +
	feat_migration(rate = 0.5, pop_from = 1, pop_to = 2) +
	feat_migration(rate = 1.0, pop_from = 2, pop_to = 1) +
	feat_selection(strength_A = 1000, population = 1, time = 0) +
	feat_selection(strength_A = 500, population = 2, time = 0) +
	feat_size_change(0.7, population = 1, time = 0) +
	feat_size_change(1.0, population = 1, time = 0.6) +
	feat_size_change(0.4, population = 2, time = 0) +
	feat_size_change(1.0, population = 2, time = 0.9) +
	sumstat_nucleotide_div("pi_1", population = 1) +
	sumstat_nucleotide_div("pi_2", population = 2)
ECmodel
ECstats <- simulate(ECmodel, nsim = 40)
mean_pi1 <- sapply(ECstats, function(x) mean(x$pi_1))
mean_pi2 <- sapply(ECstats, function(x) mean(x$pi_2))
mean_pi1
mean_pi2
