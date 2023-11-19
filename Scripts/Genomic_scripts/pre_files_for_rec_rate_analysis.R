library(tidyverse)
files <- list.files(pattern = "*.map")

dd <- map2(files,files, ~ read_delim(.x,delim = " ", col_names = F) %>%
             mutate(chrom = str_split(.y,pattern = "_|\\.")[[1]][2])) %>%
  bind_rows() %>%
  rename(physical_pos = X1, RR = X2,genetic_pos = X3)

write_delim(dd,"gt_hapmap_physical_genetic_pos.txt", delim = "\t")





dd <- read.table("~/Documents/Research/SpurginBosse_Hapmap/Data/HapMapMajor.bim")
head(dd)
output <- cbind(paste0("chr",dd$V1),dd$V4)
head(output)
output[output == "chr30"] <- "chr1A"
output[output == "chr33"] <- "chr4A"
output[output == "chr35"] <- "chrUn"
output[output == "chr36"] <- "chrZ"

write.table(output,"gt_hapmap_markers.txt", sep = "\t", row.names = F,col.names = F,quote = F)

head(dd)
