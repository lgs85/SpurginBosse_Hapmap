library(tidyverse)
recomb <- read_delim("gt_hapmap_physical_genetic_pos.txt",delim = "\t")
recomb$pos500 <- (floor(recomb$physical_pos/500000)*500000)+1

out <- recomb %>%
  group_by(chrom,pos500) %>%
  summarise(Mean_cM = mean(RR))

write_delim(out,"../../../SpurginBosse_Hapmap/Data/recomb_jon_500kb.txt", delim = "\t")
