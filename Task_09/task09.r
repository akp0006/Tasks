setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_09")
library(phytools)
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
plot(AnolisTree, type = "fan")
AnolisTree
#Question 1: There are 82 tips in the tree, and yes branch lengths are included in the tree.
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)
#Question 2: The "data" object is a data.frame; as far as its dimensions, it has 100 rows (or observations) of a single column (or variable, specifically svl or snout-vent length).
svl <- setNames(data$svl, rownames(data))
## From Sherwin's Reddit post
tree <- read.tree("http://www.phytools.org//Cordoba2017//data//Anolis.tre")
tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
head(tree)
edgelabels(tree$edge.length, bg="black", col="white", font=2)
##
Ancestors <- fastAnc(tree, svl, vars = TRUE, CI = TRUE)
#Question 3: The estimated values are stored in a list of 3 (along with the variances on the estimates and the 95% confidence intervals) under the name "ace". The CI95 element is the 95% confidence intervals on the generated estimates of how large the ancestors were.
#Question 4: 
par(mar = c(0.1,0.1,0.1,0.1))
plot(tree, type = "fan", lwd =2, show.tip.label = F)
tiplabels(pch = 16, cex = 0.25*svl[tree$tip.label])
nodelabels(pch = 16, cex = 0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type = "fan", legend = 0.7*max(nodeHeights(tree)), sig = 2, fsize = c(0.7, 0.9))
fossilData <- data.frame(svl = log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1 = c("aliniger", "aliniger", "occultus", "christophei", "cristatellus", "occultus"), tip2 = c("chlorocyanus", "coelestinus", "monticola", "cybotes", "angusticeps", "angusticeps"))



#??
for (i in fossilData$svl) {
	fossilNodes <- c()
	nodeN <- c()
	
	Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
	fossilNodes[i] <- fossilData[i, "svl"]
	nodeN <- c()
	
	names(fossilNodes) <- nodeN