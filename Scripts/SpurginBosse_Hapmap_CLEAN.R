

# Population names --------------------------------------------------------

pops$o_pop <- pops$V1
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
countries[countries %in% c("Roekelse_Bos","Westerheide")] <- "Netherlands"
countries[countries %in% c("Vlieland_NL")] <- "Netherlands (Vlieland)"
countries[countries %in% c("Tartu_Estonia")] <- "Estonia"
countries[countries %in% c("Velky_Kosir_Czech_Republic")] <- "Czech Republic"
countries[countries %in% c("Vienna_Austria")] <- "Austria"
countries[countries %in% c("Zurich_Switzerland" )] <- "Switzerland"                                                               
countries[countries %in% c("Zvenigorod_Russia")] <- "Russia"                                                               
countries[countries %in% c("Romania", "Bulgaria")] <- "Balkans"      


pops$V1 <- factor(countries,
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
                    "Russia"))

rm(countries)



# IBD ---------------------------------------------------------------------

colnames(pd) <- c("p1","p2","FST","recomb_r")
colnames(ll) <- c("Pop","Lat","Long")
for(i in 1:nrow(pd))
{
  d1 <- subset(ll,Pop == pd$p1[i])
  d2 <- subset(ll,Pop == pd$p2[i])
  pd$dist[i] <- distGeo(c(d1$Long,d1$Lat),c(d2$Long,d2$Lat))
}

islands <- c("Pirio_Muro_Corsica","Crete","Sardinia","Vlieland_NL","Gotland_Sweden")
pd$Island <- ifelse(pd$p1 %in% islands | pd$p2 %in% islands, "Island","Not Island")

# LD ----------------------------------------------------------------------
ll$LD <- ldmean$V2

colnames(ld) <- c("dist","R2","n","pop")

countries <- ld$pop

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
countries[countries %in% c("Roekelse_Bos","Westerheide")] <- "Netherlands"
countries[countries %in% c("Vlieland_NL")] <- "Netherlands (Vlieland)"
countries[countries %in% c("Tartu_Estonia")] <- "Estonia"
countries[countries %in% c("Velky_Kosir_Czech_Republic")] <- "Czech Republic"
countries[countries %in% c("Vienna_Austria")] <- "Austria"
countries[countries %in% c("Zurich_Switzerland" )] <- "Switzerland"                                                               
countries[countries %in% c("Zvenigorod_Russia")] <- "Russia"                                                               
countries[countries %in% c("Romania", "Bulgaria")] <- "Balkans"      


ld$pop <- factor(countries,
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
                    "Russia"))

ld2 <- ddply(ld,
             .(pop,dist),
             summarise,
             meanR2 = mean(R2))

rm(countries)

# Heterozygosity ----------------------------------------------------------

het <- het[complete.cases(het),]
hetlong <- data.frame(het = unlist(het),pop = rep(colnames(het),each = nrow(het)),row.names = NULL)
hetmeans <- data.frame(pop = colnames(het),het = apply(het,2,mean),se = apply(het,2,se)*1.96,row.names = NULL)

countries <- as.character(paste(hetlong$pop))

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
countries[countries %in% c("Roekelse_Bos","Westerheide")] <- "Netherlands"
countries[countries %in% c("Vlieland_NL")] <- "Netherlands (Vlieland)"
countries[countries %in% c("Tartu_Estonia")] <- "Estonia"
countries[countries %in% c("Velky_Kosir_Czech_Republic")] <- "Czech Republic"
countries[countries %in% c("Vienna_Austria")] <- "Austria"
countries[countries %in% c("Zurich_Switzerland" )] <- "Switzerland"                                                               
countries[countries %in% c("Zvenigorod_Russia")] <- "Russia"                                                               
countries[countries %in% c("Romania", "Bulgaria")] <- "Balkans"      


hetlong$pop <- factor(countries,
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
                        "Russia"))
rm(countries)


