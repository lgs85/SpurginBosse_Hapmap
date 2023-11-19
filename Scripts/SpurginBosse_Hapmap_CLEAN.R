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
    "Romania/Bulgaria",
    "Turkey",
    "Russia"
  )
)

pops <- dplyr::rename(pops,Pop = X1) %>%
        left_join(select(ll,c(Pop,Country)),join = Pop) %>%
  select(Country,X2,X3,X4,X5,X6,Pop)


# IBD ---------------------------------------------------------------------

islands <- c("Crete","Corsica","Sardinia")
colnames(pd) <- c("pop1","pop2","FST")

pd %<>%
  mutate(dist = flatten_dbl(map2(pop1,pop2, ~ distGeo(c(filter(ll,Pop == .x)$Long,
                                            filter(ll,Pop == .x)$Lat),
                                          c(filter(ll,Pop == .y)$Long,
                                            filter(ll,Pop == .y)$Lat))))) %>%
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

ld %<>% select(dist = X1,R2 = X2,n = X3,Pop = X4) %>%
  mutate(dist = ceiling(dist/1000)*1000,
         Pop = str_trim(Pop,side = "left"))%>%

  group_by(Pop,dist) %>%
  dplyr::summarise(meanR2 = mean(R2)) %>%
  ungroup() %>%
  mutate(Pop = recode(Pop,"Czech_Republic" = "Czech Republic",
                      "Netherlands_Vlieland" = "Netherlands (Vlieland)",
                      "Balkans" = "Romania/Bulgaria")) %>%
  mutate(Pop = factor(Pop,levels = levels(ll$Country)))
  

# Overall FST recombination and gene density ------------------------------
recomb %<>% mutate(chrom = as.character(chrom)) %>%
  rename(CHROM = chrom,WINDOW_START=pos500)
  
fst500 %<>% left_join(recomb) %>%
  left_join(gd500) %>%
  drop_na()

fst10 %<>% left_join(gd10) %>%
  drop_na()

# Recombination and gene density -----------------------------------------------------------

dw %<>%
  mutate(pop1 = recode(pop1,"Czech_Republic" = "Czech Republic", 
                       "Netherlands_Vlieland" = "Netherlands (Vlieland)",
                       "Balkans" = "Romania/Bulgaria")) %>%
  mutate(pop1 = factor(pop1,levels = levels(fct_drop(ll$Country[ll$Country!="Turkey"])))) %>%
  left_join(recomb %>% 
              rename(scaffold = CHROM,start = WINDOW_START) %>% 
              filter(!scaffold %in% c("Un","Z")) %>%
              mutate(scaffold = as.numeric(recode(scaffold, "1A" = "30", "4A" = "33"))),
            by = c("scaffold","start")) %>%
  left_join(gd500 %>% 
              rename(scaffold = CHROM,start = WINDOW_START) %>%
              select(scaffold,start,GENE_BP) %>%
              filter(!scaffold %in% c("Un","Z")) %>%
              mutate(scaffold = as.numeric(recode(scaffold, "1A" = "30", "4A" = "33"))),
            by = c("scaffold","start")) %>%
  dplyr::rename(MEAN_cM = Mean_cM) %>%
  mutate(MEAN_cM = replace_na(MEAN_cM,0)) %>%
  mutate(logRR = log10(MEAN_cM+1)) %>%
  group_by(pop1) %>%
  mutate(binRR = ceiling(logRR*10)/10)%>%
  ungroup()


# Outlier regions ---------------------------------------------------------

fstorder <- dw %>%
  filter(scaffold != 36,
         pop1!= "Romania/Bulgaria") %>%
  mutate(FST = replace(FST,FST < 0,0),
         pop1 = fct_drop(pop1)) %>%
  group_by(pop1) %>%
  summarise(meanFST = mean(FST)) %>%
  arrange(meanFST) %>%
  pull(pop1)



outliers <- dw %>%
  filter(scaffold != 36,
         pop1 != "Romania/Bulgaria") %>%
  mutate(FST = replace(FST, FST < 0, 0),
         pop1 = fct_drop(pop1)) %>%
  filter(zFST > 10) %>%
  group_by(scaffold,start) %>%
        dplyr::summarise(
        n_hits = n(),
        meanfst = mean(zFST),
        r = mean(MEAN_cM),
        Countries = str_c(pop1,collapse = ", ")) %>%
  left_join(select(dw,scaffold,start,MEAN_cM, binRR)) %>%
  distinct()

outlier_lines <- outliers %>%
  mutate(outlier_type = ifelse(str_count(Countries,",") > 1,"Shared","Unique")) %>%
  separate_rows(Countries,sep = ", ") %>%
  rename(pop1 = Countries) %>%
  left_join(dw,by = c("scaffold","start","pop1","MEAN_cM","binRR"))


# Outlier haplotype and nucleotide analysis -------------------------------
outlierhaps <- outlierhaps %>%
  mutate(Window = str_c(CHR,POS,sep = " "),
         nhf = ifelse(N_hits > 2,"Shared","Unique")) %>%
  filter(Window != "36 63500001") %>%
  left_join(dw %>% 
              mutate(Window = paste(scaffold,start)) %>%
              group_by(Window) %>%
              dplyr::summarise(mp = mean(pi_Turkey))) %>%
  dplyr::rename(pi = mp)


# Pop order for fst outlier plots -----------------------------------------

temp <- filter(pd,p1 == "Turkey" | p2 == "Turkey") %>%
  filter(p1 != "Romania/Bulgaria" & p2 != "Romania/Bulgaria") %>%
  mutate_if(is.factor,as.character) %>%
  arrange(dist) %>%
  mutate(p3 = ifelse(p1 == "Turkey",p2,p1)) %>%
  filter(!duplicated(p3))

dist_order <- temp$p3


# PCA ---------------------------------------------------------------------

pca <- dplyr::rename(pca,Pop = X1) %>%
  left_join(select(ll,c(Pop,Country)),
            by = "Pop")

