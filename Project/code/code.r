#Source: https://datadryad.org/stash/dataset/doi:10.5061/dryad.k7r52
Data <- read.csv("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Project\\data\\Myanmar_elephant_data.csv", stringsAsFactors=F)
# AFR = Age of first reproduction, LBS = lifetime breeding success
# Hypothesis: Age of first reproduction and lifetime breeding success are negatively correlated in female (semi-)captive-born Asian elephants.
# Useful info: A t-test is a hypothesis test for the difference in means of a single variable. A correlation test is a hypothesis test for a relationship between two variables.
# Analysis Plan: Given the hypothesis, it is pertinent that I test for correlation using the cor.test function.
DataCor <- cor.test(Data$AFR, Data$LBS, method = "pearson")
	# returns test statistic (t) and the p-value for the test that the correlation equals 0, as well as a 95% confidence interval for the correlation.
#With the plot, I also want to fit a linear model to the plotted data (i.e. perform a linear regression analysis) so that a visual representation of correlation is available.
plot(Data$LBS, Data$AFR, xlab = "Lifetime breeding success", ylab = "Age of first reproduction", las = 1, pch = 1)
library(robustbase)
Line_robust <- lmrob(Data$AFR ~ Data$LBS)
abline(Line_robust)
