p1 <- c("Westerheide","Wytham_UK")
p2 <- c("Montpellier_France","Seewisen_Germany","Gotland_Sweden","Harjavalta_Finland")
dir.create("FST_for_Mirte")

for(i in 1:length(p1))
{
  for(j in 1:length(p2))
  {
    cp1 <- p1[i]
    cp2 <- p2[j]
    write.table(c(cp1,cp2),"temp.txt",row.names = F,col.names = F,quote = F)
    system(paste0("plink --bfile PlinkFiles/HapMapMajor --fst --family --keep-fam temp.txt --out FST_for_Mirte/", cp1,"_",cp2, " --autosome-num 36"))
  }
}

cp1 <- p1[1]
cp2 <- p1[2]
write.table(c(cp1,cp2),"temp.txt",row.names = F,col.names = F,quote = F)
system(paste0("plink --bfile PlinkFiles/HapMapMajor --fst --family --keep-fam temp.txt --out FST_for_Mirte/", cp1,"_",cp2, " --autosome-num 36"))

system("rm FST_for_Mirte/*.log")
system("rm FST_for_Mirte/*.nosex")
system("rm FST_for_Mirte/*.nosex")

library(data.table)
dd <- fread("FST_for_Mirte/Wytham_UK_Gotland_Sweden.fst")
head(dd)
boxplot(dd$FST)
