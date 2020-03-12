library(ape)
library(phytools)
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_08")
text.string <-
	"(((((((cow, pig), whale),(bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
nchar(text.string)
vert.tree <- read.tree(text = text.string)
plot(vert.tree, edge.width = 2)
nodelabels(frame = "circle", bg = 'white', cex = 1)
#Q1: Humans and gold fish share a more recent common ancestor (node 14) than sharks and gold fish (node 13). Therefore, humans are more closely related to gold fish than sharks are. 
vert.tree
#Q2: No, there are no branch lengths in this tree.
str(vert.tree)
tree <- read.tree(text = "(((A, B), (C, D)), E);")
plotTree(tree, offset = 1)
tiplabels(frame = "circle", bg = 'lightblue', cex = 1)
nodelabels(frame = "circle", bg = 'white', cex = 1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las = 1)
hist(AnolisTree$edge.length, col = 'black', border = 'white', main = "", xlab = "edge lengths for the Anolis tree", ylim = c(0, 50), xlim = c(0, 6))
tipEdges <- which(AnolisTree$edge[, 2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex = 0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits = 2)
edgelabels(text = Labs, cex = 0.25)
?plot.phylo
#Q3: Create a tree with no tip labels
plot(AnolisTree, show.tip.label = FALSE)
#Q4: Create a tree that is plotted as a circle instead of facing right or left
plot(AnolisTree, "f")
#Q5: Create a tree with the tips colored red instead of black
plot(AnolisTree, tip.color = "red")
#Q6: Find which living, named species has the shortest edge length (not necessarily the shortest overall length)
	# Using the code from before:
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
	# "Anolis_occultus" has the shortest edge length.
#Q7: Then drop that tip from the tree
no_occultTree <- drop.tip(AnolisTree, "Anolis_occultus")
#Q8: Then plot the resulting tree
plot(no_occultTree, cex = 0.25)
Labs_no <- sapply(no_occultTree$edge.length, round, digits = 2)
edgelabels(text = Labs_no, cex = 0.25)
ltt(AnolisTree)
abline(0, 1, lwd = 2, col = 'red', lty = 2)
#Q9: No, the line on the lineage-through-time plot never goes down (shows the # of observed lineages alive at any given time). It never goes down because the # of observed lineages alive at any given time has only increased as Anolis lizards continue to speciate. No, the slope is not always the same. The slope of this curve tells us that speciation occurs quite quickly in Anolis lizards.
#Q10: Use the fxn fit.bd() to calculate the rate that new species form (b) and disappear (d) in Anolis lizards. Set rho = 0.2
bd <- fit.bd(AnolisTree, rho = 0.2)
	# Results:
		Fitted birth-death model:

			ML(b/lambda) = 0.8031 
			ML(d/mu) = 0 
			log(L) = 132.9163 

			Assumed sampling fraction (rho) = 0.2 

			R thinks it has converged.
# Extra Credit
install.packages("treebase")
library(treebase)
dolphins <- search_treebase("Delphinus", by="taxon", max_trees=5)
plot(dolphins[[4]], cex = 0.25)
ltt(dolphins[[4]])
dolphin_bd <- fit.bd(dolphins[[4]])
abline(0, dolphin_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, dolphin_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
n_card <- search_treebase("Cardinalis", by="taxon", max_trees=10)
plot(n_card[[2]], cex = 0.25)
ltt(n_card[[2]])
ncard_bd <- fit.bd(n_card[[2]])
abline(0, ncard_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, ncard_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
thrushes <- search_treebase("Turdus", by="taxon", max_trees=13, branch_lengths = TRUE, attempts = 1)
plot(thrushes[[3]], cex = 0.25)
ltt(thrushes[[2]])
thrush_bd <- fit.bd(thrushes[[3]])
abline(0, ncard_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, ncard_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
pigeons <- search_treebase("Columba", by="taxon", max_trees=10, branch_lengths = TRUE, attempts = 2)
ltt(pigeons[[2]])
pigeon_bd <- fit.bd(pigeons[[2]])
abline(0, pigeon_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, pigeon_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
sparrows <- search_treebase("Passer", by="taxon", max_trees=10, branch_lengths = TRUE, attempts = 2)
ltt(sparrows[[3]])
sparrow_bd <- fit.bd(sparrows[[3]])
abline(0, sparrow_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, sparrow_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
mockingbirds <- search_treebase("Mimus", by="taxon", max_trees=10, branch_lengths = TRUE, attempts = 2)
ltt(mockingbirds[[4]])
mbird_bd <- fit.bd(mockingbirds[[4]])
abline(0, mbird_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, mbird_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
wrens <- search_treebase("Troglodytes", by="taxon", max_trees=10, branch_lengths = TRUE, attempts = 2)
ltt(wrens[[4]])
wren_bd <- fit.bd(wrens[[4]])
abline(0, wren_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, wren_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)
#
chipmunks <- search_treebase("Tamias", by="taxon", max_trees=10, branch_lengths = TRUE, attempts = 2)
ltt(chipmunks[[5]])
cmunk_bd <- fit.bd(chipmunks[[5]])
abline(0, cmunk_bd$b, lwd = 2, col = 'red', lty = 2)
abline(0, cmunk_bd$d, lwd = 2, col = 'blue', lty = 2)
legend("topleft", legend = c("speciation rate", "extinction rate"), col = c("red", "blue"), lty = 2, lwd = 2)