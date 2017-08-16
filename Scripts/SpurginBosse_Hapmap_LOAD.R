library(ggplot2)
library(ggmap)
library(rworldmap)
library(magrittr)
library(reshape)
library(Rmisc)
library(geosphere)
library(ecodist)

ll <- read.table("Data/LatLongAllPops.txt",header = F,stringsAsFactors = F)


for(i in 2:10)
{
  assign(paste0("admix",i),read.table(paste0("Data/HapMapMajorPruned.", i, ".Q")))
}

pops <- read.table("Data/HapMapMajor.fam",stringsAsFactors = F)
p_markers <- read.table("Data/HapMapMajorPruned.bim",stringsAsFactors = F)
markers <- read.table("Data/HapMapMajor.bim",stringsAsFactors = F)
pd <- read.table("Data/PairwiseFST.txt",stringsAsFactors = F,header = T)
het <- read.table("Data/PopHet.txt",header = T)
ldmean <- read.table("Data/LD.txt",header = F)
ld <- read.table("Data/HapMapLD.txt",header = F,stringsAsFactors = F)
cv <- read.table("Data/CV_error.txt",header = T)
fst_admix <- read.table("Data/Admix.windowed.weir.fst",header = T)
fst_cen <- read.table("Data/CenEur.windowed.weir.fst",header = T) 
fst_UKFIN <- read.table("Data/UK_Fin.windowed.weir.fst",header = T)
fst_SARCRE <- read.table("Data/Sar_Cre.windowed.weir.fst",header = T) 
recomb <- read.table("Data/500kb_recombination.txt",header = T)
