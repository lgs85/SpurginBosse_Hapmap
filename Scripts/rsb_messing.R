library(tidyverse)

data_path <- "../Great_tit_hapmap/rsb"
fstfiles <- list.files(data_path,pattern = "Fst*")
rsbfiles <- list.files(data_path,pattern = "*Rsb*")

fst <- tibble(comparison = fstfiles) %>%
  mutate(contents = map(comparison, ~ read_tsv(file.path(data_path, .)))) %>%
  mutate(comparison = str_split(comparison,"\\Fst_|\\.",simplify = TRUE)[,2]) %>%
  unnest(cols = contents)

rsb <- tibble(comparison = rsbfiles) %>%
  mutate(contents = map(comparison, ~ read_tsv(file.path(data_path, .),
                                              skip = 1,
                                              col_names = c("SNP","CHR","POS","Rsb","logp")))) %>%
  mutate(comparison = str_split(comparison,"Rsb_",simplify = TRUE)[,2]) %>%
  unnest(cols = contents)

  

recomb2 <- recomb %>% 
  rename(CHR = chrom,window = pos500) %>% 
  filter(!CHR %in% c("Un","Z")) %>%
  mutate(CHR = as.numeric(recode(CHR, "1A" = "30", "4A" = "33")))

dd <- left_join(rsb,fst,by = c("CHR","SNP","POS","comparison")) %>%
  filter(!is.nan(FST)) %>%
  group_by(comparison) %>%
  mutate(zFST = (FST - mean(FST))/sd(FST)) %>%
  ungroup() %>%
  mutate(window = floor(POS/500000)*500000+1) %>%
  group_by(comparison,CHR,window) %>%
  summarise(meanFST = mean(zFST, na.rm = T),
            meanRsb = mean(Rsb, na.rm = T)) %>%
  left_join(recomb2,by = c("CHR","window"))

  dd %>% ggplot(aes(x = log10(Mean_cM+1),y = abs(meanRsb)))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~comparison)

f <- filter(dd,meanFST > quantile(meanFST,0.95))
r <- filter(dd,meanRsb > quantile(meanRsb,0.95,na.rm = T))

boxplot(log10(f$Mean_cM),log10(r$Mean_cM))
