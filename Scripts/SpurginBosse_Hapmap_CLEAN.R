

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
countries[countries %in% c("Romania", "Bulgaria","Turkey")] <- "Balkans"      


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
                    "Russia"))

rm(countries)



# IBD ---------------------------------------------------------------------

m <- as.matrix(pd)
ibd <- melt(m)[melt(lower.tri(m))$value,]
colnames(ibd) <- c("pop1","pop2","dist")
ibd$fst <- fst_pop$V3
ibd$dkm <- ibd$dist/1000
rm(pd,m,fst_pop)

