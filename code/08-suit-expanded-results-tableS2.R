#### rubberxbiodiversityCB 
#### 08-suit-expanded-results-tableS2.R #### 
rm(list=ls())

library(raster)
library(dplyr)
library(tidyr)
library(data.table)

library(scales) #for hue_pal
library(ggplot2)
library(cowplot)

###### DF of cells with suit, rich/vuln suit_vuln_vals3 #######
### Africa
list.files('output/afr/biodiversity_rasters/', pattern="std_mask_ext.tif")

mat_rich <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_richness_std_mask_ext.tif')
mat_vulnA <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_all_std_mask_ext.tif')
mat_vulnT <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_threat_std_mask_ext.tif')

mat_suit <- raster('output/afr/various_rasters/extendedsuit_afr_0-6.tif')
mat_pa <- raster('output/afr/various_rasters/protected_areas_afr.tif')
mat_acc <- raster('output/afr/various_rasters/accessibility_afr.tif')
mat_carb <- raster('output/afr/various_rasters/carbon_afr.tif')

#### Get mat_forest from mat_land_use
mat_land_use_cci <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_forest <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='forest', subsWithNA=FALSE)


### Mask unsuitable LU from mat_suit 
mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=0, updatevalue=0)


### Stack
suit_vuln_stack_afr <- stack(mat_suit, mat_pa, mat_rich, mat_vulnA, mat_vulnT, mat_forest, mat_acc, mat_carb)
suit_vuln_spdf_afr <- rasterToPoints(suit_vuln_stack_afr, spatial=TRUE, progress='text')

suit_vuln_vals_afr <- as.data.frame(suit_vuln_spdf_afr) 

suit_vuln_vals_afr <- suit_vuln_vals_afr %>% 
  rename(suit = names(suit_vuln_vals_afr)[1], 
         pa = names(suit_vuln_vals_afr)[2], 
         rich = names(suit_vuln_vals_afr)[3], 
         vulnA = names(suit_vuln_vals_afr)[4],
         vulnT = names(suit_vuln_vals_afr)[5],
         forest = names(suit_vuln_vals_afr)[6],
         acc = names(suit_vuln_vals_afr)[7],
         carb = names(suit_vuln_vals_afr)[8]) %>%
  mutate(conc = 0, region = 'afr')

head(suit_vuln_vals_afr)



# SSEA 
list.files('output/ssea/biodiversity_rasters/', pattern="std_mask_ext.tif")

mat_rich <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_richness_std_mask_ext.tif')
mat_vulnA <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_all_std_mask_ext.tif')
mat_vulnT <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_threat_std_mask_ext.tif')

mat_suit <- raster('output/ssea/various_rasters/extendedsuit_ssea_0-6.tif')
mat_pa <- raster('output/ssea/various_rasters/protected_areas_ssea.tif')
mat_conc <- raster('output/ssea/various_rasters/concessions_ssea.tif')
mat_acc <- raster('output/ssea/various_rasters/accessibility_ssea.tif')
mat_carb <- raster('output/ssea/various_rasters/carbon_ssea.tif')


#### Get mat_forest from mat_land_use
mat_land_use_cci <- raster('output/ssea/various_rasters/land_use_ssea.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_forest <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='forest', subsWithNA=FALSE)


### Mask unsuitable LU from mat_suit 
mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=0, updatevalue=0)


### Stack raster
suit_vuln_stack_ssea <- stack(mat_suit, mat_pa, mat_rich, mat_vulnA, mat_vulnT, mat_forest, mat_acc, mat_carb, mat_conc )

suit_vuln_spdf_ssea <- rasterToPoints(suit_vuln_stack_ssea, spatial=TRUE, progress='text') #%>% st_as_sf() #takes a minute using sf

suit_vuln_vals_ssea <- as.data.frame(suit_vuln_spdf_ssea) 

suit_vuln_vals_ssea <- suit_vuln_vals_ssea %>% 
  rename(suit = names(suit_vuln_vals_ssea)[1], 
         pa = names(suit_vuln_vals_ssea)[2], 
         rich = names(suit_vuln_vals_ssea)[3], 
         vulnA = names(suit_vuln_vals_ssea)[4],
         vulnT = names(suit_vuln_vals_ssea)[5],
         forest = names(suit_vuln_vals_ssea)[6],
         acc = names(suit_vuln_vals_ssea)[7],
         carb = names(suit_vuln_vals_ssea)[8],
         conc = names(suit_vuln_vals_ssea)[9]) %>%
  mutate(region = 'ssea')

head(suit_vuln_vals_ssea)


rm(list=setdiff(ls(), c("GISfolder", "suit_vuln_vals_afr", "suit_vuln_vals_ssea", "suit_vuln_stack_afr", "suit_vuln_stack_ssea")))


### Combine
suit_vuln_vals <- rbind(suit_vuln_vals_afr, suit_vuln_vals_ssea)

suit_vuln_vals <- suit_vuln_vals %>% 
  #mutate(suitclass = cut(suit, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) %>% 
  mutate(vulnAclass = cut(vulnA, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8,  1), right=TRUE)) %>% 
  mutate(vulnTclass = cut(vulnT, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8,  1), right=TRUE)) %>% 
  mutate(richclass = cut(rich, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) 


# Exclude when both suit and rich/vuln are NAs
# Only incld min suitability, i.e. suit >0
suit_vuln_vals2 <- suit_vuln_vals %>% 
  dplyr::filter(!(is.na(.$suit)==TRUE & is.na(.$rich)==TRUE  & is.na(.$vulnA)==TRUE  & is.na(.$vulnT)==TRUE)) %>%
  dplyr::filter(suit > 0) #125527

# Exclude PAs, exclude suit <0. Include only non-NA suit AND non-NA (rich OR vuln) #98999obs
suit_vuln_vals3 <- suit_vuln_vals %>% 
  dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$rich)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
  dplyr::filter(suit > 0) %>% 
  dplyr::filter(pa == 0) %>%
  dplyr::filter(conc == 0) #exclude PAs and conc 

head(suit_vuln_vals3) #102311



#### Write suit_vuln_vals3.csv ####
fwrite(suit_vuln_vals3, 'output/suit_vuln_vals3_ext.csv')
suit_vuln_vals3ext <- fread('output/suit_vuln_vals3_ext.csv')

suit_vuln_vals3ext %>% dplyr::filter(suit >=2 ) %>% group_by(region) %>% summarize(n=n())


#####################  Table S2 (area of all classes)  ###################################
head(suit_vuln_vals3)
unique(suit_vuln_vals3$vulnTclass)
suit_vuln_vals3[suit_vuln_vals3$vulnT == 0, ] #checking for no 0s

# Master table
aoc_tbl <- expand.grid(vulnTclass=unique(suit_vuln_vals3$vulnTclass), suit=c(6:2), region=c('afr', 'ssea'))


#Table S1
aoc_tbl_vulnT <- suit_vuln_vals3 %>% 
  group_by(region, suit, vulnTclass) %>% 
  summarize(Mha = n()/100) %>% 
  arrange(region)

aoc_tbl <- left_join(aoc_tbl, aoc_tbl_vulnT)

aoc_tbl_sum <- aoc_tbl %>% dplyr::select(-region) %>% group_by(suit, vulnTclass) %>% summarize_all(sum, na.rm=TRUE) %>% mutate(region='z.both')
aoc_tbl_sum <- bind_rows(aoc_tbl, aoc_tbl_sum) %>% arrange(region, suit, vulnTclass)

aoc_tbl_wide <- aoc_tbl_sum %>% spread(vulnTclass, Mha, fill='-') %>% arrange(region, desc(suit))

write.csv(aoc_tbl_wide, 'output/results/TableS2.csv', row.names=FALSE)



#### Numbers for the Results ####

# Of land available for rubber expansion, just 0.1 Mha in Africa and none in Asia/New Guinea had high bioclimatic suitability for rubber (suitability >0.8) (Figure 1).
suit_vuln_vals3 %>% filter(suit == 6) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #43.3, 147 Mha in afr and ssea highly suitable
suit_vuln_vals3 %>% filter(suit == 5) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #277 and 135 Mha high suitable

# Among these highly suitable areas, none were minimal in extinction vulnerability (vulnerability ≤0.2) (Figure 1 and Table S1; see also Text S1).
suit_vuln_vals3ext %>% filter(suit==6 & vulnT <= 0.2) %>% group_by(region, suit, vulnTclass) %>% summarize(n = n(), Mha = n()*0.01) %>% arrange(region, suit)
# >> win-win: 0.29, 22.6 in afr and ssea

# We identified 19.5 Mha of land meeting our criteria for ‘areas of compromise’ (Table S1, see also Text S1). 
suit_vuln_vals3 %>% filter(suit >= 2 & vulnT <= 0.4) %>% summarize(n = n(), Mha = n()*0.01) 
# 733.79 Mha

# Africa had fewer areas of compromise (8.7 Mha) compared to Asia and New Guinea (11.9 Mha), 
suit_vuln_vals3ext %>% filter(suit >= 2 & vulnT <= 0.4) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #601 Mha afr and 133 Mha in ssea. Afr has lot more.



# For Text 2 Result (vulnA)
# Of land available for rubber expansion, just 0.1 Mha in Africa and none in Asia/New Guinea had high bioclimatic suitability for rubber (suitability >0.8) (Figure 1).
suit_vuln_vals3 %>% filter(suit == 6) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #43.3 and 147
suit_vuln_vals3 %>% filter(suit == 5) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #277 and 135 Mha high suitable

# Among these highly suitable areas, none were minimal in extinction vulnerability (vulnerability ≤0.2) (Figure 1 and Table S1; see also Text S1).
suit_vuln_vals3 %>% filter(suit==6 & vulnA <= 0.2) %>% group_by(region, suit, vulnAclass) %>% summarize(n = n(), Mha = n()*0.01) %>% arrange(region, suit)
# >> win-win: 0.3, 3.27 in afr and ssea

# We identified 19.5 Mha of land meeting our criteria for ‘areas of compromise’ (Table S1, see also Text S1). 
suit_vuln_vals3 %>% filter(suit >= 2 & vulnA <= 0.4) %>% summarize(n = n(), Mha = n()*0.01) 
# 295.89 Mha

# Africa had fewer areas of compromise (8.7 Mha) compared to Asia and New Guinea (11.9 Mha), 
suit_vuln_vals3 %>% filter(suit >= 2 & vulnA <= 0.4) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) #185 Mha afr and 111 Mha in ssea. Afr has lot more.


