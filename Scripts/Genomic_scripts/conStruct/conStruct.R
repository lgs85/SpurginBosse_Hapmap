
library(data.table)
library(fields)
library(conStruct)
library(plotrix)
library(rworldmap)

# Make an input dataset ---------------------------------------------------

#Filter on relatedness


#Allele freqs
system("plink --bfile ~/Documents/Research/Great_tit_hapmap/PlinkFiles/HapMapMajorPruned --rel-cutoff 0.05 --family --freq --out HapMapMajorPruned --autosome-num 36")
system("rm *.log")
system("rm *.nosex")

dd <- fread("HapMapMajorPruned.frq.strat",header = T)
system("rm HapMapMajorPruned.frq.strat")

m <- matrix(dd$MAF,nrow = length(unique(dd$CLST)), ncol = length(unique(dd$SNP)),byrow = F)
rownames(m) <- unique(dd$CLST)
colnames(m) <- unique(dd$SNP)


ll <- read.table("~/Documents/Research/SpurginBosse_Hapmap/Data/LatLongAllPops.txt")
llx <- as.matrix(ll[,c("V3","V2")])

d1 <- rdist.earth(llx)

Hapmap.data <- list(allele.frequencies = m,coords = llx,geoDist = d1)


#Look at vignettes
vignette(topic="format-data",package="conStruct")
vignette(topic="run-conStruct",package="conStruct")


out <- conStruct(spatial = TRUE, 
          K = 2, 
          freqs = Hapmap.data$allele.frequencies,
          geoDist = Hapmap.data$geoDist, 
          coords = Hapmap.data$coords,
          n.iter = 500,
          make.figs = F,
          save.files = F)


admix <- out$chain_1$MAP$admix.proportions
rownames(admix) <- unique(dd$CLST)
admix2 <- admix[order(admix[,1]),]
ll <- read.table("~/Documents/Research/SpurginBosse_Hapmap/Data/LatLongAllPops.txt")
ll <- ll[order(admix[,1]),]

par(mar = c(12,4,2,2))
barplot(t(admix2),las = 2)


par(mar = c(2,2,2,2))
map(xlim = c(-10,40),ylim = c(30,70))
for(i in 1:nrow(admix2))
{
  floating.pie(ll$V3[i],ll$V2[i],admix2[i,])
}

plot(out$chain_1$posterior$lpd,type = "l")





par(mfrow=c(1,3),mar=c(4,3,1.5,1))
plot(c(0,rnorm(500,1,0.2)),type='l',
     xlab="",yaxt='n',ylab="")
mtext(side=2,text="parameter estimate",padj=-1)
mtext(side=3,text="(a) looks good",padj=-0.1)
plot(c(0,rnorm(500,c(log(seq(0,1,length.out=500))),0.2)),type='l',
     xlab="",yaxt='n',ylab="")
mtext(side=1,text="mcmc iterations",padj=2.6)
mtext(side=3,text="(b) hasn't converged",padj=-0.1)
plot(c(0,rnorm(150,1,0.2),rnorm(200,3,0.2),rnorm(150,1,0.2)),type='l',
     xlab="",yaxt='n',ylab="")
mtext(side=3,text="(c) multi-modal",padj=-0.1)

