library(data.table)
library(ggplot2)

dd <- fread("relative_sort_centimorgans_for_650k_snps_kees_v1.txt")

head(dd)

ggplot(dd,aes(x = V2, y = V4))+
  geom_line()+
  facet_wrap(~V3)
ggplot(dd,aes(x = V2, y = V7))+
  geom_line()+
  facet_wrap(~V3)

output <- data.frame(chromosome = dd$V3,physical_pos = dd$V2, genetic_pos = dd$V4,stringsAsFactors = F)
write.table(output,"gt_hapmap_physical_genetic_pos.txt", sep = "\t", row.names = F, quote = F)





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
