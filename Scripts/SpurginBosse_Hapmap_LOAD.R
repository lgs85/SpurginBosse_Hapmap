library(ggplot2)
library(ggmap)
library(rworldmap)
library(magrittr)
library(reshape)
library(Rmisc)
library(geosphere)
library(ecodist)

ll <- read.table("Data/LatLongAllPops.txt",header = F)


for(i in 2:10)
{
  assign(paste0("admix",i),read.table(paste0("Data/HapMapMajorPruned.", i, ".Q")))
}

pops <- read.table("Data/HapMapMajor.fam",stringsAsFactors = F)
p_markers <- read.table("Data/HapMapMajorPruned.bim",stringsAsFactors = F)
markers <- read.table("Data/HapMapMajor.bim",stringsAsFactors = F)
pd <- read.table("Data/PairwiseFST.txt",stringsAsFactors = F)
het <- read.table("Data/PopHet.txt",header = T)
ldmean <- read.table("Data/LD.txt",header = F)
ld <- read.table("Data/HapMapLD.txt",header = F,stringsAsFactors = F)
cv <- read.table("Data/CV_error.txt",header = T)
