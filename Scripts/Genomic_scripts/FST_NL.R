rm(list=ls())

pd <- read.table("../../Data/PairwiseFST.txt",header = T,stringsAsFactors = F)
recomb <- read.table("../../Data/500kb_recombination.txt",header = T)


pd <- subset(pd,pop1 == "Westerheide"|pop2 == "Westerheide")

for(i in 1:nrow(pd))
{
  if(pd$pop1[i] == "Westerheide")
  {
    pd$pop1[i] <- pd$pop2[i]
    pd$pop2[i] <- "Westerheide"
  }
}

pops <- unique(unlist(pd[order(pd$FST),"pop1"]))


pops <- pops[!(pops %in% c("Turkey","Antwerp_Belgium","Harjavalta_Finland","La_Rouviere_France",
                           "Font_Roja_Spain","Cambridge_UK","Radolfzell_Germany","Bulgaria","Romania",
                           "Vienna_Austria","Tartu_Estonia","Westerheide","Zurich_Switzerland","Velky_Kosir_Czech_Republic"))]




for (i in 1:length(pops))
{
  system(paste0("vcftools --vcf ~/Documents/Research/Great_tit_hapmap/vcftools/HapMapMajor.vcf --out temp --chr 33 --weir-fst-pop ~/Documents/Research/Great_tit_hapmap/vcftools/popfiles/Westerheide.txt --weir-fst-pop ~/Documents/Research/Great_tit_hapmap/vcftools/popfiles/", pops[i], ".txt"))
  temp <- read.table("temp.weir.fst",header = T,stringsAsFactors = F)
  temp$Pop <- pops[i]
  
  if(i == 1)
  {
    write.table(temp,"temp.fst",row.names = F,col.names = T,quote = F)
    system("cp temp.fst Westerheide.fst")
  } else
  {
    write.table(temp,"temp.fst",row.names = F,col.names = F, quote = F)
    system("cat Westerheide.fst temp.fst > temp.file")
    system("mv temp.file Westerheide.fst")
  }
}



dd <- read.table("Westerheide.fst",header = T,stringsAsFactors = F)

library(ggplot2)

dd <- subset(dd,!(Pop %in% c("Sardinia","Crete","Vlieland_NL")))

ggplot(dd,aes(y = WEIR_AND_COCKERHAM_FST,x = POS))+
  geom_line(col = "grey")+
  annotate(geom = "text",x = 11968430,y = 0.19,label = "X")+
  theme_bw()+
  facet_wrap(~Pop)

dd$WEIR_AND_COCKERHAM_FST[dd$WEIR_AND_COCKERHAM_FST < 0] <- 0

x1 <- subset(dd,POS > 11914600 & POS < 11996961) %>%
  ddply(
        ("Pop"),
        summarize,
        meanFST = mean(WEIR_AND_COCKERHAM_FST,na.rm = T))

x2 <- subset(dd,!(POS %in% x1$POS)) %>%
  ddply(
    ("Pop"),
    summarize,
    meanFST = mean(WEIR_AND_COCKERHAM_FST,na.rm = T))

par(mar = c(5,5,2,2))
plot(x1$meanFST~x2$meanFST,
     xlab = "FST (Chromosome 4A)",
     ylab = "FST (COL4A5)",
     cex.lab = 1.6,cex.axis = 1.4,
     pch = 19,
     cex = 1.4)
abline(0,1,lty = 2 )
text(x = 0.01,y = 0.105,labels = "Wytham woods",cex = 1.2)
text(x = 0.022,y = 0.135,labels = "Loch Lomond",cex = 1.2)


meanfst <- tapply(dd$MEAN_FST,dd$Pop,mean)[dd$Pop]
sdfst <- tapply(dd$MEAN_FST,dd$Pop,sd)[dd$Pop]

dd$zFST <- (dd$MEAN_FST - meanfst)/sdfst

ggplot(dd,aes(y = zFST,x = BIN_START))+
  geom_line(col = "grey")+
  annotate(geom = "text",x = 11968430,y = 6,label = "X")+
  theme_bw()+
  xlim(c(1e07,1.5e07))+
  facet_wrap(~Pop)
