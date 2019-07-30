# Population names --------------------------------------------------------
colnames(ll) <- c("Pop", "Lat", "Long", "Country")

ll$Country <- factor(
  ll$Country,
  levels = c(
    "Scotland",
    "England",
    "Spain",
    "France",
    "Belgium",
    "Netherlands",
    "Netherlands (Vlieland)",
    "Switzerland",
    "Germany",
    "Corsica",
    "Sardinia",
    "Italy",
    "Austria",
    "Czech Republic",
    "Hungary",
    "Sweden",
    "Finland",
    "Estonia",
    "Crete",
    "Balkans",
    "Turkey",
    "Russia"
  )
)

pops <- dplyr::rename(pops,Pop = X1) %>%
        left_join(select(ll,c(Pop,Country)),join = Pop) %>%
  select(Country,X2,X3,X4,X5,X6,Pop)


# IBD ---------------------------------------------------------------------

colnames(pd) <- c("pop1","pop2","FST")

for(i in 1:nrow(pd))
{
  d1 <- subset(ll,Pop == pd$pop1[i])
  d2 <- subset(ll,Pop == pd$pop2[i])
  pd$dist[i] <- distGeo(c(d1$Long,d1$Lat),c(d2$Long,d2$Lat))
}

islands <- c("Crete","Corsica","Sardinia")

pd <- 
  pd %>%
  left_join(dplyr::rename(ll,pop1 = Pop) %>%
              select(c(pop1,Country))) %>%
  dplyr::rename(p1 = Country) %>%
  left_join(dplyr::rename(ll,pop2 = Pop) %>%
              select(c(pop2,Country))) %>%
  dplyr::rename(p2 = Country) %>%
  mutate(Island = ifelse(p1 %in% islands | p2 %in% islands,
                         "Island",
                         "Mainland"))

# LD ----------------------------------------------------------------------

ld <- select(ld,-X5)
colnames(ld) <- c("dist","R2","n","Pop")

ld <- mutate(ld,Pop = str_trim(Pop,side = "left"))

ld2 <- ld %>% 
  group_by(Pop,dist) %>%
  dplyr::summarise(meanR2 = mean(R2)) %>%
  mutate(Pop = str_replace(Pop,"Czech_Republic","Czech Republic")) %>%
  mutate(Pop = str_replace(Pop,"Netherlands_Vlieland","Netherlands (Vlieland)")) %>%
  mutate(Pop = factor(Pop,levels = levels(ll$Country)))
  

# Overall FST recombination and gene density ------------------------------
fst500 <- left_join(fst500,dplyr::rename(recomb,CHROM = chrom,WINDOW_START=pos500)) %>%
  left_join(gd500) %>%
  drop_na()

fst10 <- left_join(fst10,gd10) %>%
  drop_na()

# Recombination -----------------------------------------------------------

dw$pop1 <- str_replace(dw$pop1,"Czech_Republic","Czech Republic")
dw$pop1 <- str_replace(dw$pop1,"Netherlands_Vlieland","Netherlands (Vlieland)")
dw$pop1 <- factor(dw$pop1,
                  levels = levels(fct_drop(ll$Country[ll$Country!="Turkey"])))

dw <- dw %>%
  mutate(Window = str_c(scaffold,start,sep = " ")) %>%
  left_join(mutate(recomb,Window = str_c(chrom,pos500,sep = " ")) %>%
              select(c(Window,Mean_cM))) %>%
  left_join(mutate(gd500,Window = str_c(CHROM,WINDOW_START,sep = " ")) %>%
              select(c(Window,GENE_BP))) %>%
  dplyr::rename(MEAN_cM = Mean_cM) %>%
  mutate(MEAN_cM = replace_na(MEAN_cM,0))





# Outlier regions ---------------------------------------------------------
temp <- filter(dw,scaffold != 36,
               pop1 != "Balkans") %>%
  mutate(FST = replace(FST, FST < 0, 0),
         pop1 = fct_drop(pop1))
fstorder <- names(tapply(temp$FST,temp$pop1,mean)[order(tapply(temp$FST,temp$pop1,mean))])


rm(temp)



temp <- dw %>%
  filter(scaffold != 36,
         pop1 != "Balkans") %>%
  mutate(FST = replace(FST, FST < 0, 0),
         pop1 = fct_drop(pop1)) %>%
  filter(zFST > 10) %>%
  group_by(Window) %>%
        dplyr::summarise(
        n_hits = n(),
        meanfst = mean(zFST),
        r = mean(MEAN_cM))

outliers <- dw %>% 
  filter(Window %in% temp$Window,
         zFST > 10) %>%
  group_by(Window) %>%
  dplyr::summarise(nhits = n(),
            Countries = str_c(pop1,collapse = ", "))

rm(temp)

temp <- filter(dw,scaffold != 36,
               pop1 != "Balkans") %>%
  mutate(FST = replace(FST, FST < 0, 0),
         pop1 = fct_drop(pop1))


temp2 <- filter(temp,zFST > 10) %>%
  group_by(Window) %>%
        dplyr::summarise(n_hits = n())

x1 <- filter(temp,
             zFST > 10,
             Window %in% temp2$Window) %>%
  group_by(Window) %>%
  dplyr::mutate(nhits = n()) %>%
  ungroup(Window) %>%
  dplyr::mutate(nhf = ifelse(nhits > 2,"Shared","Unique"))

rm(temp,temp2)



# Outlier haplotype and nucleotide analysis -------------------------------
outlierhaps <- outlierhaps %>%
  mutate(Window = str_c(CHR,POS,sep = " "),
         nhf = ifelse(N_hits > 2,"Shared","Unique")) %>%
  filter(Window != "36 63500001") %>%
  left_join(dw %>% 
              group_by(Window) %>%
              dplyr::summarise(mp = mean(pi_Turkey))) %>%
  dplyr::rename(pi = mp)


# Pop order for fst outlier plots -----------------------------------------

temp <- filter(pd,p1 == "Turkey" | p2 == "Turkey") %>%
  mutate_if(is.factor,as.character) %>%
  arrange(dist) %>%
  mutate(p3 = ifelse(p1 == "Turkey",p2,p1)) %>%
  filter(!duplicated(p3))

dist_order <- temp$p3


# PCA ---------------------------------------------------------------------

pca <- dplyr::rename(pca,Pop = X1) %>%
  left_join(select(ll,c(Pop,Country)),join = Pop)

