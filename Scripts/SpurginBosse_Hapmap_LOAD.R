library(ggplot2)
library(ggmap)
library(rworldmap)
library(magrittr)
library(reshape)
library(Rmisc)
library(geosphere)
library(ecodist)
library(kableExtra)

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
recomb <- read.table("Data/500kb_recombination.txt",header = T)
tu <- read.table("Data/Turkey.fst",header = T,stringsAsFactors = F)
outlierhaps <- read.table("Data/Outliers_for_Mirte_HAP_statistics1.txt",header = T)
pca <- read.table("Data/HapMap.eigenvec", stringsAsFactors = F)


#Windowed stats
fn <- list.files("Data/Windowed_stats")
popname <- substr(fn,1,nchar(fn)-7)

for(i in c(1:length(fn)))
{
  cd <-  read.csv(gzfile(paste0("Data/Windowed_stats/",popname[i],".csv.gz")))
  colnames(cd)[6:9] <- c("pi_pop1","pi_Turkey","dxy","FST")
  cd$pop1 <- rep(popname[i],nrow(cd))
  cd$zFST <- (cd$FST - mean(cd$FST))/sd(cd$FST)
  cd$zdxy <- (cd$dxy - mean(cd$dxy))/sd(cd$dxy)
  cd$zpi <- (cd$pi_pop1 - mean(cd$pi_pop1))/sd(cd$pi_pop1)
  cd$order <- c(1:nrow(cd))
  if(i == 1) dw <- cd else dw <- rbind(dw,cd)
}

rm(fn,popname,cd)

