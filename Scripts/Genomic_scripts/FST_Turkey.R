rm(list=ls())

pd <- read.table("../../Data/PairwiseFST.txt",header = T,stringsAsFactors = F)
recomb <- read.table("../../Data/500kb_recombination.txt",header = T)


pd <- subset(pd,pop1 == "Turkey"|pop2 == "Turkey")

for(i in 1:nrow(pd))
{
  if(pd$pop1[i] == "Turkey")
  {
    pd$pop1[i] <- pd$pop2[i]
    pd$pop2[i] <- "Turkey"
  }
}

pops <- unique(unlist(pd[order(pd$FST),"pop1"]))


pops <- pops[!(pops %in% c("Turkey","Antwerp_Belgium","Harjavalta_Finland","La_Rouviere_France",
                           "Font_Roja_Spain","Cambridge_UK","Radolfzell_Germany","Bulgaria","Romania",
                           "Vienna_Austria","Tartu_Estonia","Westerheide","Zurich_Switzerland","Velky_Kosir_Czech_Republic"))]




for (i in 1:length(pops))
{
  system(paste0("vcftools --vcf ~/Documents/Research/Great_tit_hapmap/vcftools/HapMapMajor.vcf --out temp --fst-window-size 500000 --fst-window-step 500000 --weir-fst-pop ~/Documents/Research/Great_tit_hapmap/vcftools/popfiles/Turkey.txt --weir-fst-pop ~/Documents/Research/Great_tit_hapmap/vcftools/popfiles/", pops[i], ".txt"))
  temp <- read.table("temp.windowed.weir.fst",header = T,stringsAsFactors = F)
  temp$Pop <- pops[i]
  
  if(i == 1)
  {
    write.table(temp,"temp.fst",row.names = F,col.names = T,quote = F)
    system("cp temp.fst Turkey.fst")
  } else
  {
    write.table(temp,"temp.fst",row.names = F,col.names = F, quote = F)
    system("cat Turkey.fst temp.fst > temp.file")
    system("mv temp.file Turkey.fst")
  }
}

system("mv Scripts/Genomic_scripts/Turkey.fst Data")
system("rm Scripts/Genomic_scripts/temp.*")



dd <- read.table("Data/Turkey.fst",header = T,stringsAsFactors = F)



inall <- Reduce(intersect, list(paste(recomb$CHROM,recomb$BIN_START),
                                paste(dd$CHROM,dd$BIN_START)))

dd <- subset(dd,paste(dd$CHROM,dd$BIN_START) %in% inall)
dd$r <- NA

for(i in 1:nrow(recomb))
{
  dd$r[paste(dd$CHROM,dd$BIN_START) == paste(recomb$CHROM[i],recomb$BIN_START[i])] <- recomb$MEAN_cM[i]
}



q <- quantile(dd$r,0.5)
dd$rf <- ifelse(dd$r < q,"Low","Not low")

dd$Pop <- factor(dd$Pop,levels = unique(dd$Pop))

dd <- subset(dd,CHROM < 36)
dd$MEAN_FST[dd$MEAN_FST < 0] <- 0
library(ggplot2)

dd$x <- NA
pops <- unique(dd$Pop)
for(i in 1:length(pops))
{
  dd$x[dd$Pop == pops[i]] <- 1:sum(dd$Pop == pops[i])
}


Fig4A <- ggplot(dd,aes(x = x,y = MEAN_FST,col = rf))+
  geom_line(aes(group=1))+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    #strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank())+
  xlab("")+
  ylab(expression(italic(F)[ST])) + 
  facet_wrap(~Pop,ncol = 2) +
  theme(axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.text.x = element_text(size = 6),
        legend.position = "none")+
  scale_colour_manual(values = c("navy","grey"))

Fig4A

subset(dd,MEAN_FST > 0) %>%
ggplot(aes(MEAN_FST))+
  geom_density()+
  facet_wrap(~Pop,ncol = 4,scales = "free_y")

dd2 <- subset(subset(dd,r < 100),r > -1)
ggplot(dd2,aes(x = r,y = MEAN_FST))+
  geom_jitter()+
  facet_wrap(~Pop,ncol = 2)



head(tu)

Fig4A <- ggplot(tu,aes(x = x,y = MEAN_FST,col = FST_F))+
  geom_line(aes(group = NA))+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank())+
  xlab("")+
  ylab(expression(italic(F)[ST])) + 
  facet_wrap(~Pop,ncol = 2) +
  theme(axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.text.x = element_text(size = 6),
        legend.position = "none")+
  scale_colour_manual(values = c("navy","grey"))

Fig4A


x <- subset(tu,FST_F == "outlier")
x$newbin <- round(x$BIN_START,-7)
x1 <- subset(x,!duplicated(paste(CHROM,newbin,Pop)))
x1$u_bin <- paste(x1$CHROM,x1$newbin)

subset(x1,paste(CHROM,newbin) == "1 0")
x2 <- table(x1$u_bin)
x3 <- subset(x1,u_bin %in% names(x2[x2 == 10]))
(table(x3$Pop,x3$u_bin))

hist(x2,breaks = 20)

