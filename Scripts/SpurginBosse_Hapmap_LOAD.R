library(tidyverse)
library(ggmap)
library(rworldmap)
library(geosphere)
library(ecodist)
library(kableExtra)
library(cowplot)
library(ggrepel)
library(magrittr)

#Set theme
theme_set(theme_bw())

#Latitude and longitude
ll <- read_tsv("Data/LatLongAllPops.txt",col_names = F)


#Admixture
files <- list.files("Data",pattern = "*.Q")
admix <- tibble(Q = files) %>%
  mutate(contents = map(Q, ~ read_delim(file.path("Data", .), delim = " ",col_names = F)))


pops <- read_delim("Data/HapMapMajor.fam", delim = " ",col_names = F)
p_markers <- read_tsv("Data/HapMapMajorPruned.bim", col_names = F)
markers <- read_tsv("Data/HapMapMajor.bim", col_names = F)
pd <- read_tsv("Data/PairwiseFST.txt",col_names = F)
ld <- read_delim("Data/HapMapLD.txt", delim = " ", col_names = F)
cv <- read_tsv("Data/CV_error.txt")
recomb <- read_delim("Data/recomb_jon_500kb.txt", delim = "\t")
tu <- read_delim("Data/Turkey.fst", delim = " ")
outlierhaps <- read_tsv("Data/Outliers_for_Mirte_HAP_statistics1.txt")
pca <- read_delim("Data/HapMapMajor.eigenvec", delim = " ", col_names = F)
gd500 <- read_csv("Data/gene_density500kb.csv",col_types = "cddddd")
gd10 <- read_csv("Data/gene_density10kb.csv",col_types = "cddddd")
fst500 <- read_delim("Data/HapMapMajor500kb.windowed.fst",delim = "\t",col_types = "cdddd")
fst10 <- read_delim("Data/HapMapMajor10kb.windowed.fst",delim = "\t",col_types = "cdddd")
rsb <- read_tsv("Data/Hapmap_rsb.txt")

#Windowed stats
files <- list.files("Data/Windowed_stats",pattern = "*.csv.gz")

dw <- tibble(pop1 = str_remove(files,".csv.gz")) %>%
  mutate(data = map(files, ~ read_csv(gzfile(file.path("Data/Windowed_stats",.))) %>%
                      dplyr::rename(pi_pop1 = 6,
                                    pi_Turkey = 7,
                                    dxy = 8,
                                    FST = 9) %>%
                      mutate(zFST = (FST - mean(FST))/sd(FST),
                             zdxy = (dxy - mean(dxy))/sd(dxy),
                             zpi = (pi_pop1 - mean(pi_pop1))/sd(pi_pop1),
                             order = c(1:n())))) %>%
  unnest(cols = data)


