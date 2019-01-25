library(reshape)
library(tidyverse)
library(ggmap)
library(rworldmap)
library(Rmisc)
library(geosphere)
library(ecodist)
library(kableExtra)
library(cowplot)

theme_set(theme_bw())

ll <- read_tsv("Data/LatLongAllPops.txt",col_names = F)


for(i in 2:10)
{
  assign(str_c("admix",i),read_delim(str_c("Data/HapMapMajorPruned.", i, ".Q"),delim = " ",col_names = F))
}




pops <- read_delim("Data/HapMapMajor.fam", delim = " ",col_names = F)
p_markers <- read_tsv("Data/HapMapMajorPruned.bim", col_names = F)
markers <- read_tsv("Data/HapMapMajor.bim", col_names = F)
pd <- read_tsv("Data/PairwiseFST.txt")
ld <- read_delim("Data/HapMapLD.txt", delim = " ", col_names = F)
cv <- read_tsv("Data/CV_error.txt")
recomb <- read_delim("Data/recomb_kai_500kb.txt", delim = " ")
tu <- read_delim("Data/Turkey.fst", delim = " ")
outlierhaps <- read_tsv("Data/Outliers_for_Mirte_HAP_statistics1.txt")
pca <- read_delim("Data/HapMap.eigenvec", delim = " ", col_names = F)


#Windowed stats
fn <- list.files("Data/Windowed_stats")
popname <- str_sub(fn,1,str_length(fn)-7)

for(i in c(1:length(fn)))
{
  cd <-  read_csv(gzfile(str_c("Data/Windowed_stats/",popname[i],".csv.gz")))
  cd <- cd %>% 
    dplyr::rename(pi_pop1 = 6,
                       pi_Turkey = 7,
                       dxy = 8,
                       FST = 9) %>%
    mutate(pop1 = popname[i],
           zFST = (FST - mean(FST))/sd(FST),
           zdxy = (dxy - mean(dxy))/sd(dxy),
           zpi = (pi_pop1 - mean(pi_pop1))/sd(pi_pop1),
           order = c(1:nrow(cd)))

  if(i == 1) dw <- cd else dw <- rbind(dw,cd)
}

rm(fn,popname,cd)

