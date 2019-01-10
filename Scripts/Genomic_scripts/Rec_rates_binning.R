library(plyr)
recomb <- read.table("Data/rec_rates_kai.txt",header = T,stringsAsFactors = F)
head(recomb)
recomb$chrom <- substr(recomb$chrom,4,nchar(recomb$chrom))
recomb$pos500 <- (floor(recomb$pos/500000)*500000)+1
head(recomb,200)

out <- ddply(recomb,
      .(chrom,pos500),
      summarise,
      Mean_cM = mean(rec_rate))

write.table(out,"Data/recomb_kai_500kb.txt",row.names = F,quote = F)

