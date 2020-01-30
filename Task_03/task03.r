install.packages("paleobioDB", dep = T)
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_03")
library(paleobioDB)
Taxon <- "Dinosauria"
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species", temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
MinMA <- 201
MaxMA <- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <- 66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Triassic (252 - 201Ma)", cex=3, line=-2)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Jurassic (201 - 145Ma)", cex=3, line=-2)
dev.new(height = 7.8, width = 13)
pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
mtext(side = 3, "Cretaceous (145 - 66Ma)", cex=3, line=-2)
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMA <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col=Col_dino, lwd= LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col= Col_mammal, lwd= LineWidth)
legend("topleft", legend=c(Taxon, Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)
Taxon3 <- "Echinodermata"
MinMA <- 2
MaxMA <- 66
fossils3 <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 2
nspeciesOverTime3 <- pbdb_richness(fossils3, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
newspeciesOverTime3 <- pbdb_orig_ext(fossils3, res=2, rank = "species", temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime3)), newspeciesOverTime3[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon3)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime3)), newspeciesOverTime3[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
pbdb_map_richness(fossils3, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
Taxon4 <- "Gastropoda"
fossils4 <- pbdb_occurrences(base_name = Taxon4, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime4 <- pbdb_richness(fossils4, rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)
newspeciesOverTime4 <- pbdb_orig_ext(fossils4, res=2, rank = "species", temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime4)), newspeciesOverTime4[,1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main = Taxon4)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime4)), newspeciesOverTime4[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
pbdb_map_richness(fossils4, col.ocean=OceanCol, col.int=LandCol, col.rich=Cols)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_echinoderm <- Col_dino
Col_gastropod <- Col_mammal
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime3)), nspeciesOverTime3[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col=Col_echinoderm, lwd= LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime4)), nspeciesOverTime4[,2], col= Col_gastropod, lwd= LineWidth)
legend("topleft", legend=c(Taxon3, Taxon4), col=c(Col_echinoderm, Col_gastropod), bty="n", lwd=LineWidth)
#Hypothesis: An event approximately 30 million years ago led to the sharp decline of echinoderm and gastropod species, whose numbers have not recovered to this day.

#Extra Credit
install.packages("rebird")
setwd("C:\\Users\\Abbey\\Desktop\\Evolution\\Tasks\\Task_03")
library(rebird)
birds <- ebirdfreq("states", "US-WV", 2019, 2019, 1, 12)
a.goldfinch <- which(birds[,1] == "American Goldfinch")
a.gold <- birds[a.goldfinch,]
firstWeek <- a.gold[a.gold$monthQt %in% c("January-1", "February-1", "March-1", "April-1", "May-1", "June-1", "July-1", "August-1", "September-1", "October-1", "November-1", "December-1"),]
crows <- which(birds[,1] == "American Crow")
a.crow <- birds[crows,]
firstWeekCrow <- a.crow[a.crow$monthQt %in% c("January-1", "February-1", "March-1", "April-1", "May-1", "June-1", "July-1", "August-1", "September-1", "October-1", "November-1", "December-1"),]
AcceptDataFinch <- read.table(text="
 monthQt     frequency
 1 January-1       0.244
 2 February-1      0.242
 3 March-1         0.263
 4 April-1         0.321
 5 May-1           0.346
 6 June-1          0.376
 7 July-1          0.457
 8 August-1        0.596
 9 September-1     0.548
10 October-1       0.295
11 November-1      0.336
12 December-1      0.370", stringsAsFactors=FALSE)
plot(frequency~factor(monthQt, levels=c("January-1", "February-1", "March-1", "April-1", "May-1", "June-1", "July-1", "August-1", "September-1", "October-1", "November-1", "December-1")), AcceptDataFinch, type="p", pch=15, las=2, 
     xlab="", main="Frequencies in WV during the first week of each month in 2019")
AcceptDataCrow <- read.table(text="
 monthQt     frequency
 1 January-1       0.389
 2 February-1      0.437
 3 March-1         0.426
 4 April-1         0.410
 5 May-1           0.360
 6 June-1          0.382
 7 July-1          0.394
 8 August-1        0.438
 9 September-1     0.455
10 October-1       0.451
11 November-1      0.498
12 December-1      0.387", stringsAsFactors=FALSE)
points(frequency~factor(monthQt, levels=c("January-1", "February-1", "March-1", "April-1", "May-1", "June-1", "July-1", "August-1", "September-1", "October-1", "November-1", "December-1")), AcceptDataCrow, type = "p", pch=0, col = "red")
legend("topleft", legend=c("Am. Goldfinch", "Am. Crow"), fill=c("black", "red"), cex=0.5, bty="n")
