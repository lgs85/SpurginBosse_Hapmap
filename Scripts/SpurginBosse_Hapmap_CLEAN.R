

# Population names --------------------------------------------------------

countries <- pops$V1

countries[countries %in% c("Antwerp_Belgium")] <- "Belgium"
countries[countries %in% c("Cambridge_UK","Wytham_UK")] <- "England"
countries[countries %in% c("Font_Roja_Spain","Mariola_Spain")] <- "Spain"
countries[countries %in% c("Gotland_Sweden")] <- "Sweden"
countries[countries %in% c("Harjavalta_Finland","Oulu_Finland")] <- "Finland"
countries[countries %in% c("La_Rouviere_France" ,"Montpellier_France")] <- "France"
countries[countries %in% c("Loch_Lomond_Scotland")] <- "Scotland"
countries[countries %in% c("Pilis_Mountains_Hungary")] <- "Hungary"
countries[countries %in% c("Pirio_Muro_Corsica")] <- "Corsica"
countries[countries %in% c("Radolfzell_Germany","Seewisen_Germany")] <- "Germany"
countries[countries %in% c("Roekelse_Bos","Vlieland_NL","Westerheide")] <- "Netherlands"
countries[countries %in% c("Tartu_Estonia")] <- "Estonia"
countries[countries %in% c("Velky_Kosir_Czech_Republic")] <- "Czech Republic"
countries[countries %in% c("Vienna_Austria")] <- "Austria"
countries[countries %in% c("Zurich_Switzerland" )] <- "Switzerland"                                                               
countries[countries %in% c("Zvenigorod_Russia")] <- "Russia"                                                               
countries[grep("Benetutti|Cabras",pops$V2)] <- "Sardinia"
countries[countries %in% c("Romania", "Bulgaria")] <- "Balkans"      


pops$V1 <- factor(countries,
                  levels = c(
                    "Scotland",
                    "England",
                    "Spain",
                    "France",
                    "Belgium",
                    "Netherlands",
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
                    "Russia"))

rm(countries)



# IBD ---------------------------------------------------------------------

colnames(pd) <- c("p1","p2","FST")
colnames(ll) <- c("Pop","Lat","Long")
for(i in 1:nrow(pd))
{
  d1 <- subset(ll,Pop == pd$p1[i])
  d2 <- subset(ll,Pop == pd$p2[i])
  pd$dist[i] <- distGeo(c(d1$Long,d1$Lat),c(d2$Long,d2$Lat))
}

islands <- c("Pirio_Muro_Corsica","Crete","Sardinia")
pd$Island <- ifelse(pd$p1 %in% islands | pd$p2 %in% islands, "Island","Not Island")

# Heterozygosity ----------------------------------------------------------

# hetlong <- melt(het) %>%
#   reshape::rename(c(variable = "pop", value = "he")) %>% #plyr also has a rename function
#   mutate(chr = rep(p_markers$V1,length(unique(pop))))