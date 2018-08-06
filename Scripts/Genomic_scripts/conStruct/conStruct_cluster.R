
library(data.table)
library(fields)
library(conStruct)
library(plotrix)
library(rworldmap)

# Make an input dataset ---------------------------------------------------


dd <- fread("plink/HapMapMajorPruned.frq.strat",header = T)

m <- matrix(dd$MAF,nrow = length(unique(dd$CLST)), ncol = length(unique(dd$SNP)),byrow = F)
rownames(m) <- unique(dd$CLST)
colnames(m) <- unique(dd$SNP)


ll <- read.table("LatLongAllPops.txt")
llx <- as.matrix(ll[,c("V3","V2")])

d1 <- rdist.earth(llx)

Hapmap.data <- list(allele.frequencies = m,coords = llx,geoDist = d1)

for(i in c(2:5))
{

out <- conStruct(spatial = TRUE, 
          K = i, 
          freqs = Hapmap.data$allele.frequencies,
          geoDist = Hapmap.data$geoDist, 
          coords = Hapmap.data$coords,
          n.iter = 50000,
          make.figs = F,
          save.files = F)



  admix <- out$chain_1$MAP$admix.proportions
  rownames(admix) <- unique(dd$CLST)
  admix2 <- admix[order(admix[,1]),]
  ll <- read.table("LatLongAllPops.txt")
  ll <- ll[order(admix[,1]),]
  
  
  pdf(paste0("K",i,".pdf"),width = 6,height = 6)
  par(mar = c(2,2,2,2))
  map(xlim = c(-10,40),ylim = c(30,70))
  for(i in 1:nrow(admix2))
  {
    floating.pie(ll$V3[i],ll$V2[i],admix2[i,])
  }
  
  plot(out$chain_1$posterior$lpd,type = "l")
  
  dev.off()
  
  write.table(admix,paste0("K",i,".txt"),row.names = T,col.names = F,quote = F)
}
