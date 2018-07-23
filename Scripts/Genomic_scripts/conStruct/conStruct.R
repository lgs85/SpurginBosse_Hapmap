
library(data.table)
library(fields)
library(conStruct)
library(plotrix)
library(rworldmap)

# Make an input dataset ---------------------------------------------------

#Filter on relatedness


#Allele freqs
system("plink --bfile ~/Documents/Research/Great_tit_hapmap/PlinkFiles/HapMapMajorPruned --rel-cutoff 0.05 --family --freq --out HapMapMajorPruned --autosome-num 36")
system("rm *.log")
system("rm *.nosex")

dd <- fread("HapMapMajorPruned.frq.strat",header = T)

m <- matrix(dd$MAF,nrow = length(unique(dd$CLST)), ncol = length(unique(dd$SNP)),byrow = F)
rownames(m) <- unique(dd$CLST)
colnames(m) <- unique(dd$SNP)


ll <- read.table("~/Documents/Research/SpurginBosse_Hapmap/Data/LatLongAllPops.txt")
llx <- as.matrix(ll[,c("V3","V2")])

d1 <- rdist.earth(llx)

Hapmap.data <- list(allele.frequencies = m,coords = llx,geoDist = d1)


#Look at vignettes
vignette(topic="format-data",package="conStruct")
vignette(topic="run-conStruct",package="conStruct")


out <- conStruct(spatial = TRUE, 
          K = 5, 
          freqs = Hapmap.data$allele.frequencies,
          geoDist = Hapmap.data$geoDist, 
          coords = Hapmap.data$coords,
          make.figs = F,
          save.files = F)

system()

admix <- out$chain_1$MAP$admix.proportions
rownames(admix) <- unique(dd$CLST)
admix2 <- admix[order(admix[,1]),]
ll <- read.table("~/Documents/Research/SpurginBosse_Hapmap/Data/LatLongAllPops.txt")
ll <- ll[order(admix[,1]),]

par(mar = c(12,4,2,2))
barplot(t(admix2),las = 2)



map(xlim = c(-10,40),ylim = c(30,70))
for(i in 1:nrow(admix2))
{
  floating.pie(ll$V3[i],ll$V2[i],admix2[i,])
}
