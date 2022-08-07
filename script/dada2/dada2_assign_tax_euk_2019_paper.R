
library(dada2)
packageVersion("dada2")
# https://benjjneb.github.io/dada2/index.html
# Assign taxonomy - DADA2
# FASTA to tabular = https://usegalaxy.org/

folder_path <- "C:/Users/rrocha/Documents/R/phd/2019/data/"
#folder_path <- '~/Documents/R/phd/2019/data/'
#setwd(folder_path)
seqtab.nochim.arc <- readRDS(paste0(folder_path,"sequences_fasta_a_16S/dada2/seqtab.nochim.RDS"))
seqtab.nochim.bac <- readRDS(paste0(folder_path,"sequences_fasta_16S/dada2/seqtab.nochim.RDS"))
seqtab.nochim.euk <- readRDS(paste0(folder_path,"sequences_fasta_18S/dada2/seqtab.nochim.RDS"))


doParallel::registerDoParallel()

# Taxonomic reference data @ <https://benjjneb.github.io/dada2/training.html>
# 16S ----
refFasta.16s <- '~/R/phd/silva_nr99_v138.1_train_set.fa.gz' # updated Mar 10 ,2021
#refFasta.16s <- '~/Documents/R/phd/silva_nr99_v138.1_train_set.fa.gz'
# 18S ----
refFasta.18s <- '~/R/phd/silva_132.18s.99_rep_set.dada2.fa'
pr2_refFasta.18s <- '~/R/phd/pr2_version_4.14.0_SSU_dada2.fasta.gz'


# 2019 data ----


# ARCHAEA ----
taxa_ASV_arc_MH2019 <- assignTaxonomy(seqtab.nochim.arc, refFasta.16s, minBoot = 50, tryRC = FALSE,
                                      outputBootstraps = FALSE, 
                                      taxLevels = c("Kingdom", "Phylum", "Class",
                                                    "Order", "Family", "Genus", "Species"), 
                                      multithread = T, verbose = FALSE)
write.csv(taxa_ASV_arc_MH2019, '/data/tax_arc_2019.csv')

# BACTERIA -----
taxa_ASV_bac_MH2019 <- assignTaxonomy(seqtab.nochim.bac, refFasta.16s, minBoot = 50, tryRC = FALSE,
                                      outputBootstraps = FALSE, 
                                      taxLevels = c("Kingdom", "Phylum", "Class",
                                                    "Order", "Family", "Genus", "Species"), 
                                      multithread = T, verbose = FALSE)

write.csv(taxa_ASV_bac_MH2019, '/data/tax_bac_2019.csv')


# EUKARYOTES -----
taxa_ASV_euk_MH2019 <- assignTaxonomy(seqtab.nochim.euk, refFasta.18s, minBoot = 50, tryRC = FALSE,
                                      outputBootstraps = FALSE, 
                                      taxLevels = c("Kingdom", "Phylum", "Class",
                                                    "Order", "Family", "Genus", "Species"), 
                                      multithread = T, verbose = T)


write.csv(taxa_ASV_euk_MH2019, '/data/tax_euk_2019.csv')


# EUKARYOTES PR2 -----
taxa_ASV_euk_MH2019_pr2 <- assignTaxonomy(seqtab.nochim.euk, pr2_refFasta.18s, minBoot = 50, tryRC = FALSE,
                                      outputBootstraps = FALSE, 
                                      taxLevels = c("Kingdom","Supergroup","Division","Class",
                                                    "Order","Family","Genus","Species"), 
                                      multithread = T, verbose = T)


write.csv(taxa_ASV_euk_MH2019_pr2, '/data/tax_euk_2019_pr2.csv')


# making data frames ----
library(tidyverse)

# table with samples id and extraction id
extract_id <- read_csv(paste0(folder_path,"/meta_env/extraction_ID.csv")) 
extract_id <- extract_id %>% arrange(ID_extraction)
extract_id <- extract_id %>% 
  mutate(sample_extr = str_c("RRPS-MH-", ID_extraction)) %>%
  select(sample_extr, Samples)
sample.names <- extract_id %>% pull(Samples)
sample.extr <- extract_id %>% pull(sample_extr)

# to upload sequences information to NCBI
# meta_date_collection <- read_csv(paste0(folder_path,"/meta_env.csv")) %>% select(sample_id, date_collected)
# meta <- read_csv(paste0(folder_path,"/meta_impute.csv")) 
# meta_extr <- extract_id %>% 
#                 rename(sample_id = Samples) %>% 
#                 left_join(meta) %>% 
#                 left_join(meta_date_collection) %>%
#                 mutate(lat_long = str_c(lat, long, sep = "_"))
# meta_extr %>% write_csv(paste0(folder_path,"/meta_env/extraction_meta_seq_ID.csv")) 

 
## Arc -----
asv_arc <- t(seqtab.nochim.arc) %>% as.data.frame() %>% 
    rownames_to_column("seq")  %>% as_tibble() %>%
  rename_with(~ sample.names[which(sample.extr == .x)], .cols = sample.extr)  # 28,632 ASVs

asv_arc %>%  
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  # rowwise() %>% mutate(total = sum(c_across(where(is.numeric)))) %>% 
  filter(total ==1)  # 175 ASVs
asv_arc %>%  mutate(total = reduce(select(., where(is.numeric)), `+`)) %>%
  filter(total ==2) # 3,822 ASVs

write_csv(asv_arc, paste0(folder_path,'ASV_arc_2019.csv'))

tax_arc <- read_csv(paste0(getwd(),'/data/tax_arc_2019.csv')) %>% rename(seq = 1)
tax_arc %>% nrow
tax_arc %>% group_by(Kingdom) %>% count(Phylum) %>% View()
asv_arc <- asv_arc %>% left_join(tax_arc)
asv_arc %>% nrow()
# filter contaminants (Eukaryota; mithocondria and chloroplasts, all assigned to Kingdom Bacteria)
bac_in_arc <- tax_arc  %>% filter(Kingdom %in% c("Bacteria", 'Eukaryota')) # remove bacteria from the dataset
bac_in_arc %>% count(Family, sort = T)
# clean
asv_arc <- asv_arc %>% filter(Kingdom == "Archaea") %>% drop_na(Phylum) # 8,553 archaeal ASVs
asv_arc %>% 
  count(Class, sort = T)
# add ASV ids column
asv_arc <- asv_arc %>% mutate(id = str_c('a_asv_',1:nrow(.))) %>%
  relocate(id) 

write_csv(asv_arc, paste0(folder_path,'arc_2019.csv')) 


## Bac ----

asv_bac <- t(seqtab.nochim.bac) %>% as.data.frame() %>% 
  rownames_to_column("seq")  %>% as_tibble() %>%
  rename_with(~ sample.names[which(sample.extr == .x)], .cols = sample.extr) # 25,505 ASVs

asv_bac %>% 
   mutate(total = rowSums(across(where(is.numeric)))) %>%
 # rowwise() %>% mutate(total = sum(c_across(where(is.numeric)))) %>% 
  filter(total ==1) # 4,209 ASVs
asv_bac %>%  mutate(total = reduce(select(., where(is.numeric)), `+`)) %>%
  filter(total ==2) # 3,161 ASVs

write_csv(asv_bac, paste0(folder_path,'ASV_bac_2019.csv'))
tax_bac <- read_csv(paste0(getwd(),'/data/tax_bac_2019.csv')) %>% rename(seq = 1)

tax_bac %>% nrow()
tax_bac %>% group_by(Kingdom) %>% count(Phylum) %>% View()
asv_bac <- asv_bac %>% left_join(tax_bac)
asv_bac %>% nrow() # 25505
asv_bac %>% group_by(Kingdom) %>% count(Phylum) %>% View() # checking
# filter non-bacteria
asv_bac <- asv_bac %>% filter(Kingdom == "Bacteria")
asv_bac %>% count(Kingdom)
asv_bac %>% count(Phylum, sort = T)
# check NAs at the Class level
asv_bac %>% filter(is.na(Class)) %>% count(Phylum) %>% View
# remove Phylum NAs
asv_bac <- asv_bac %>% drop_na(Phylum)
asv_bac <- asv_bac %>%
  mutate(id = str_c('b_asv_',1:nrow(.))) %>%
  relocate(id) 

write_csv(asv_bac, paste0(folder_path,'bac_2019.csv')) 


## Euk - 18S ----
asv_euk <- t(seqtab.nochim.euk) %>% as.data.frame() %>% 
  rownames_to_column("seq")  %>% as_tibble() %>%
  rename_with(~ sample.names[which(sample.extr == .x)], .cols = sample.extr) # 13,365 ASVs

asv_euk %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  # rowwise() %>% mutate(total = sum(c_across(where(is.numeric)))) %>% 
  filter(total ==1) # 39 ASVs
asv_euk %>%  
  mutate(total = reduce(select(., where(is.numeric)), `+`)) %>%
  filter(total ==2) # 1,663 ASVs

write_csv(asv_euk, paste0(folder_path,'ASV_euk_2019.csv'))
tax_euk <- read_csv(paste0(getwd(),'/data/tax_euk_2019.csv')) %>% rename(seq = 1)

tax_euk %>% nrow()
tax_euk %>% group_by(Kingdom) %>% count(Phylum) %>% View()
asv_euk <- asv_euk %>% full_join(tax_euk)
asv_euk %>% nrow() # 13365
asv_euk %>% group_by(Kingdom) %>% count(Phylum) %>% View() # checking
# filter non-eukaryota
asv_euk <- asv_euk %>% filter(Kingdom == "Eukaryota")
asv_euk %>% count(Kingdom)
asv_euk %>% count(Phylum, sort = T)
# check NAs at the Class level
asv_euk %>% filter(is.na(Class)) %>% count(Phylum) %>% View
# remove Phylum NAs
asv_euk <- asv_euk %>% drop_na(Phylum) # left 7,740 ASVs
asv_euk <- asv_euk %>%
  mutate(id = str_c('e_asv_',1:nrow(.))) %>%
  relocate(id) 

write_csv(asv_euk, paste0(folder_path,'euk_2019.csv')) 



# OBS: change samples names
# asv_arc <- asv_arc %>% rename(C_DP1 = C_Surf, C_DP2 = C_MID, C_DP3 = C_Bottom, 
#                               T2_D1_DP1 = F_surf, T2_D1_DP2 = F_Mid, T2_D1_DP3 = F_Bot)



# PR2 version 4.14.0 (https://pr2database.github.io/pr2database/index.html)
# <https://vaulot.github.io/tutorials/R_dada2_tutorial.html>
pr2_file <- '~/R/phd/silva_132.18s.99_rep_set.dada2.fa'

seqtab.nochim.euk <- readRDS(paste0(folder_path,"sequences_fasta_18S/dada2/seqtab.nochim.RDS"))

PR2_tax_levels <-  c("Kingdom","Supergroup","Division","Class","Order","Family","Genus","Species")

taxa_ASV_euk_MH2019_pr2 <- assignTaxonomy(seqtab.nochim, refFasta = pr2_file, taxLevels = PR2_tax_levels, 
                       minBoot = 50, outputBootstraps = TRUE, verbose = TRUE, multithread = T)

write.csv(taxa_ASV_euk_MH2019_pr2, '/data/tax_euk_2019_pr2.csv')



