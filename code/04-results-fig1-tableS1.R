#### rubberxbiodiversityCB 
#### 04-results-fig1-tableS1.R #### 
rm(list=ls())

library(raster)
library(dplyr)
library(tidyr) #for gather
library(data.table)

library(scales) #for hue_pal
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())


###### DF of cells with suit, rich/vuln suit_vuln_vals3 #######
### + Africa ####
list.files('output/afr/biodiversity_rasters/')

mat_rich <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_richness_std_mask.tif')
mat_vulnA <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_all_std_mask.tif')
mat_vulnT <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_threat_std_mask.tif')

mat_suit <- raster('output/afr/various_rasters/suitability_afr.tif')
mat_pa <- raster('output/afr/various_rasters/protected_areas_afr.tif')
mat_acc <- raster('output/afr/various_rasters/accessibility_afr.tif')
mat_carb <- raster('output/afr/various_rasters/carbon_afr.tif')
mat_country <- raster('output/afr/various_rasters/afr_countryID.tif')


#### Get mat_forest from mat_land_use
mat_land_use_cci <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_forest <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='forest', subsWithNA=FALSE)


### Mask unsuitable LU from mat_suit 
mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=0, updatevalue=0)


### Stack
suit_vuln_stack_afr <- stack(mat_suit, mat_pa, mat_rich, mat_vulnA, mat_vulnT, mat_forest, mat_acc, mat_carb, mat_country)
suit_vuln_spdf_afr <- rasterToPoints(suit_vuln_stack_afr, spatial=TRUE, progress='text')

suit_vuln_vals_afr <- as.data.frame(suit_vuln_spdf_afr) 

suit_vuln_vals_afr <- suit_vuln_vals_afr %>% 
  rename(suit = names(suit_vuln_vals_afr)[1], 
         pa = names(suit_vuln_vals_afr)[2], 
         richness = names(suit_vuln_vals_afr)[3], 
         vulnA = names(suit_vuln_vals_afr)[4],
         vulnT = names(suit_vuln_vals_afr)[5],
         forest = names(suit_vuln_vals_afr)[6],
         acc = names(suit_vuln_vals_afr)[7],
         carb = names(suit_vuln_vals_afr)[8],
         countryID = names(suit_vuln_vals_afr)[9]   ) %>%
  mutate(conc = 0, region = 'afr')

head(suit_vuln_vals_afr)

#### + Checking for points missing country info (SSEA only; DO ONCE) ####
# Exclude PAs, exclude suit <0. Include only non-NA suit AND non-NA (rich OR vuln) #39235 obs
countryNA <- suit_vuln_vals_afr %>% 
  dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$rich)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
  dplyr::filter(suit > 0) %>% 
  dplyr::filter(pa == 0) %>%
  dplyr::filter(conc == 0) %>% 
  dplyr::filter(is.na(.$countryID)==TRUE) #9 points with country NA 

#countryNA_sf <- st_as_sf(countryNA, coords=c("x", "y"), crs= st_crs(suit_vuln_stack_afr))
#plot(countryNA_sf["conc"])

#st_write(countryNA_sf, "output/temp/afr_points_countryNA.shp") #shpfile for ArcMap
#write.csv(countryNA, "output/temp/afr_points_countryNA.csv")


### Check these points in ArcMap and fix them manually
countryNA_corr <- read.csv("data/afr_points_countryNA_corrected.csv") %>% dplyr::select(-X)

all.equal(countryNA_corr[ , 3:10], countryNA[, 1:8]) #[1] "Component “acc”: Mean relative difference: 1.181546e-06"
countryNA <- cbind(countryNA, countryID.corrected = countryNA_corr$countryID.corrected)

suit_vuln_vals_afr2 <- left_join(suit_vuln_vals_afr, countryNA)
suit_vuln_vals_afr2 %>% dplyr::filter(is.na(.$countryID)==TRUE & is.na(.$countryID.corrected)==FALSE) %>% nrow() #9, correct
suit_vuln_vals_afr2 <- suit_vuln_vals_afr2 %>% mutate(countryID = ifelse( (is.na(.$countryID)=TRUE & is.na(.$countryID.corrected)==FALSE), countryID.corrected, countryID))

table(suit_vuln_vals_afr$countryID)
table(suit_vuln_vals_afr2$countryID.corrected)
table(suit_vuln_vals_afr2$countryID)

unique(suit_vuln_vals_afr2$countryID) 

# Replace countryID with country code
countrycode <- read.csv("output/afr/various_rasters/afr_countryID.csv")

suit_vuln_vals_afr2 <- left_join(suit_vuln_vals_afr2, countrycode) %>% rename(country = NAME_0)
unique(suit_vuln_vals_afr2$country)

suit_vuln_vals_afr <- suit_vuln_vals_afr2 %>% dplyr::select(-countryID.corrected, -countryID, -GID_0)

head(suit_vuln_vals_afr)

# # check for no more country NAs:
# countryNA <- suit_vuln_vals_afr %>% 
#   dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$rich)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
#   dplyr::filter(suit > 0) %>% 
#   dplyr::filter(pa == 0) %>%
#   dplyr::filter(conc == 0) %>% 
#   dplyr::filter(is.na(.$country)==TRUE) 


# + SSEA ####
list.files('output/ssea/biodiversity_rasters/')

mat_rich <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_richness_std_mask.tif')
mat_vulnA <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_all_std_mask.tif')
mat_vulnT <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_threat_std_mask.tif')

mat_suit <- raster('output/ssea/various_rasters/suitability_ssea.tif')
mat_pa <- raster('output/ssea/various_rasters/protected_areas_ssea.tif')
mat_conc <- raster('output/ssea/various_rasters/concessions_ssea.tif')
                                                  
mat_acc <- raster('output/ssea/various_rasters/accessibility_ssea.tif')
mat_carb <- raster('output/ssea/various_rasters/carbon_ssea.tif')
mat_country <- raster('output/ssea/various_rasters/ssea_countryID.tif')


#### Get mat_forest from mat_land_use
mat_land_use_cci <- raster('output/ssea/various_rasters/land_use_ssea.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_forest <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='forest', subsWithNA=FALSE)


### Mask unsuitable LU from mat_suit 
mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit <- raster::mask(mat_suit, mat_land_use, maskvalue=0, updatevalue=0)

### Stack raster
suit_vuln_stack_ssea <- stack(mat_suit, mat_pa, mat_rich, mat_vulnA, mat_vulnT, mat_forest,  mat_acc, mat_carb, mat_country, mat_conc )

suit_vuln_spdf_ssea <- rasterToPoints(suit_vuln_stack_ssea, spatial=TRUE, progress='text') #%>% st_as_sf() #takes a minute using sf

suit_vuln_vals_ssea <- as.data.frame(suit_vuln_spdf_ssea) 

suit_vuln_vals_ssea <- suit_vuln_vals_ssea %>% 
  rename(suit = names(suit_vuln_vals_ssea)[1], 
         pa = names(suit_vuln_vals_ssea)[2], 
         richness = names(suit_vuln_vals_ssea)[3], 
         vulnA = names(suit_vuln_vals_ssea)[4],
         vulnT = names(suit_vuln_vals_ssea)[5],
         forest = names(suit_vuln_vals_ssea)[6],
         acc = names(suit_vuln_vals_ssea)[7],
         carb = names(suit_vuln_vals_ssea)[8],
         countryID = names(suit_vuln_vals_ssea)[9],
         conc = names(suit_vuln_vals_ssea)[10]) %>%
  mutate(region = 'ssea')

head(suit_vuln_vals_ssea)


#### + Checking for points missing country info (SSEA only; DO ONCE) ####
# Exclude PAs, exclude suit <0. Include only non-NA suit AND non-NA (rich OR vuln) #39235 obs
countryNA <- suit_vuln_vals_ssea %>% 
  dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$rich)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
  dplyr::filter(suit > 0) %>% 
  dplyr::filter(pa == 0) %>%
  dplyr::filter(conc == 0) %>% 
  dplyr::filter(is.na(.$countryID)==TRUE) #44 points with country NA

countryNA_sf <- st_as_sf(countryNA, coords=c("x", "y"), crs= st_crs(suit_vuln_stack_ssea))
plot(countryNA_sf["conc"])

st_write(countryNA_sf, "output/temp/ssea_points_countryNA.shp") #shpfile for ArcMap
write.csv(countryNA, "output/temp/ssea_points_countryNA.csv")


### Check these points in ArcMap and fix them manually
countryNA_corr <- read.csv("data/ssea_points_countryNA_corrected.csv") %>% dplyr::select(-X)

all.equal(countryNA_corr[ , 3:10], countryNA[, 1:8]) #[1] "Component “acc”: Mean relative difference: 2.469414e-08"
countryNA <- cbind(countryNA, countryID.corrected = countryNA_corr$countryID.corrected)

suit_vuln_vals_ssea2 <- left_join(suit_vuln_vals_ssea, countryNA)
suit_vuln_vals_ssea2 %>% dplyr::filter(is.na(.$countryID)==TRUE & is.na(.$countryID.corrected)==FALSE) %>% nrow() #44, correct
suit_vuln_vals_ssea2 <- suit_vuln_vals_ssea2 %>% mutate(countryID = ifelse( (is.na(.$countryID)=TRUE & is.na(.$countryID.corrected)==FALSE), countryID.corrected, countryID))

table(suit_vuln_vals_ssea$countryID)
table(suit_vuln_vals_ssea2$countryID.corrected)
table(suit_vuln_vals_ssea2$countryID)

unique(suit_vuln_vals_ssea2$countryID) 
# Replace countryID with country code
countrycode <- read.csv("output/ssea/various_rasters/ssea_countryID.csv")

suit_vuln_vals_ssea2 <- left_join(suit_vuln_vals_ssea2, countrycode) %>% rename(country = NAME_0)
unique(suit_vuln_vals_ssea2$country) 

# check
# countryNA_check <- suit_vuln_vals_ssea2 %>% 
#   dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$rich)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
#   dplyr::filter(suit > 0) %>% 
#   dplyr::filter(pa == 0) %>%
#   dplyr::filter(conc == 0) %>% 
#   dplyr::filter(is.na(.$countryID)==TRUE) #no more points with country NA!

suit_vuln_vals_ssea <- suit_vuln_vals_ssea2 %>% dplyr::select(-countryID.corrected, -countryID, -GID_0)





### + Combine Afr and SSEA ####
head(suit_vuln_vals_ssea) #597132
head(suit_vuln_vals_afr) #561816

rm(list=setdiff(ls(), c("GISfolder", "suit_vuln_vals_afr", "suit_vuln_vals_ssea", "suit_vuln_stack_afr", "suit_vuln_stack_ssea")))

suit_vuln_vals <- rbind(suit_vuln_vals_afr, suit_vuln_vals_ssea)

# Exclude when both suit and richness/vuln are NAs
# Only incld min suitability, i.e. suit >0
suit_vuln_vals2 <- suit_vuln_vals %>% 
  dplyr::filter(!(is.na(.$suit)==TRUE & is.na(.$richness)==TRUE  & is.na(.$vulnA)==TRUE  & is.na(.$vulnT)==TRUE)) %>%
  dplyr::filter(suit > 0) #281150 obs

# Exclude PAs, exclude suit <0. Include only non-NA suit AND non-NA (richness OR vuln)
suit_vuln_vals3 <- suit_vuln_vals %>% 
  dplyr::filter(is.na(.$suit)==FALSE) %>% filter(is.na(.$richness)==FALSE | is.na(.$vulnA)==FALSE | is.na(.$vulnT)==FALSE ) %>%
  dplyr::filter(suit > 0) %>% 
  dplyr::filter(pa == 0) %>%
  dplyr::filter(conc == 0) #exclude PAs and conc #236117 obs 

suit_vuln_vals3 <- dplyr::select(suit_vuln_vals3, -pa, -conc)


#### Write suit_vuln_vals3.csv  ####
fwrite(suit_vuln_vals3, 'output/suit_vuln_vals3.csv')



##### Assign Levels of compromise ####
suit_vuln_vals3 <- fread( 'output/suit_vuln_vals3.csv')

suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(comprom.vulnT.1 =  ifelse((vulnT<=0.2 & suit >0.8), 1,  #production driven compromise (compromise on vuln, preserve suit)
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 3,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 5,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      99)))))),
         
         comprom.vulnT.2 =  ifelse((vulnT<=0.2 & suit >0.8), 1, #conserrvation driven compromise (compromise on suit, preserve vuln)
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 4,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 5, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      99)))))), 
         
         comprom.vulnT.3 =  ifelse((vulnT<=0.2 & suit >0.8), 1, #equal weight to both compromise
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 3, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 4, 
                                                                      99)))))) )

suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(comprom.vulnA.1 =  ifelse((vulnA<=0.2 & suit >0.8), 1,  #production driven compromise (compromise on vuln, preserve suit)
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 3,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 5,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      99)))))),
         
         comprom.vulnA.2 =  ifelse((vulnA<=0.2 & suit >0.8), 1, #conserrvation driven compromise (compromise on suit, preserve vuln)
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 4,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 5, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      99)))))), 
         
         comprom.vulnA.3 =  ifelse((vulnA<=0.2 & suit >0.8), 1, #equal weight to both compromise
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 3, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 4, 
                                                                      99)))))) )

suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(comprom.rich.1 =  ifelse((richness<=0.2 & suit >0.8), 1,  #production driven compromise (compromise on vuln, preserve suit)
                                  ifelse((richness<=0.2 & suit>0.6 & suit<=0.8), 3,
                                         ifelse((richness<=0.2 & suit>0.4 & suit<=0.6), 5,
                                                ifelse((richness<=0.4 & suit >0.8), 2,
                                                       ifelse((richness<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                              ifelse((richness<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                     99)))))),
         
         comprom.rich.2 =  ifelse((richness<=0.2 & suit >0.8), 1, #conserrvation driven compromise (compromise on suit, preserve vuln)
                                  ifelse((richness<=0.2 & suit>0.6 & suit<=0.8), 2,
                                         ifelse((richness<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                ifelse((richness<=0.4 & suit >0.8), 4,
                                                       ifelse((richness<=0.4 & suit>0.6 & suit<=0.8), 5, 
                                                              ifelse((richness<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                     99)))))), 
         
         comprom.rich.3 =  ifelse((richness<=0.2 & suit >0.8), 1, #equal weight to both compromise
                                  ifelse((richness<=0.2 & suit>0.6 & suit<=0.8), 2,
                                         ifelse((richness<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                ifelse((richness<=0.4 & suit >0.8), 2,
                                                       ifelse((richness<=0.4 & suit>0.6 & suit<=0.8), 3, 
                                                              ifelse((richness<=0.4 & suit>0.4 & suit<=0.6), 4, 
                                                                     99)))))) )

table(suit_vuln_vals3$comprom.vulnT.1, suit_vuln_vals3$comprom.vulnT.2)
table(suit_vuln_vals3$comprom.vulnT.1, suit_vuln_vals3$comprom.vulnT.3)

# Assigning levels of risk
suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(risk.rich = ifelse((richness>0.6 & suit<=0.4), 1, 0),
         risk.vulnA = ifelse((vulnA>0.6 & suit<=0.4), 1, 0),
         risk.vulnT = ifelse((vulnT>0.6 & suit<=0.4), 1, 0) ) 

head(suit_vuln_vals3)





################### Table S1 (area of AOC)  #####################################
head(suit_vuln_vals3)

# Master table
aoc_tbl <- expand.grid(region=c('afr', 'ssea'), comprom1=1:6) %>% mutate(comprom2=rep(c(1,4,2,5,3,6), each=2), comprom3=rep(c(1,2,2,3,3,4), each=2))

str(suit_vuln_vals3)


#Table S1
aoc_tbl_vulnT <- suit_vuln_vals3 %>% filter(comprom.vulnT.1 >0) %>% 
  group_by(region, comprom.vulnT.1, comprom.vulnT.2, comprom.vulnT.3) %>% 
  summarize(Mha = n()/100) %>% 
  dplyr::select(region, comprom1 = comprom.vulnT.1, comprom2 = comprom.vulnT.2, comprom3 = comprom.vulnT.3, Mha.vulnT=Mha) %>% 
  arrange(region, comprom1) 

aoc_tbl_vulnA <- suit_vuln_vals3 %>% filter(comprom.vulnA.1 >0) %>% 
  group_by(region, comprom.vulnA.1, comprom.vulnA.2, comprom.vulnA.3) %>% 
  summarize(Mha = n()/100) %>% 
  dplyr::select(region, comprom1 = comprom.vulnA.1, comprom2 = comprom.vulnA.2, comprom3 = comprom.vulnA.3, Mha.vulnA=Mha) %>% 
  arrange(region, comprom1) 

aoc_tbl_rich <- suit_vuln_vals3 %>% filter(comprom.rich.1 >0) %>% 
  group_by(region, comprom.rich.1, comprom.rich.2, comprom.rich.3) %>% 
  summarize(Mha = n()/100) %>% 
  dplyr::select(region, comprom1 = comprom.rich.1, comprom2 = comprom.rich.2, comprom3 = comprom.rich.3, Mha.rich=Mha) %>% 
  arrange(region, comprom1) 

aoc_tbl <- left_join(aoc_tbl, aoc_tbl_vulnT)
aoc_tbl <- left_join(aoc_tbl, aoc_tbl_vulnA)
aoc_tbl <- left_join(aoc_tbl, aoc_tbl_rich)

aoc_tbl_sum <- aoc_tbl %>% dplyr::select(-region) %>% group_by(comprom1, comprom2, comprom3) %>% summarize_all(sum, na.rm=TRUE) %>% mutate(region='z.both')
aoc_tbl_sum <- bind_rows(aoc_tbl, aoc_tbl_sum) %>% arrange(region, comprom1)

aoc_tbl_wide <- aoc_tbl_sum %>% gather(vuln, Mha, Mha.vulnT, Mha.vulnA, Mha.rich) %>% spread(region, Mha, fill='-') %>% arrange(vuln, comprom1) 
aoc_tbl_wide <- aoc_tbl_wide %>% group_by(vuln) %>% mutate(cumsum=cumsum(z.both))

#write.csv(aoc_tbl_wide, 'output/results/TableS1_wide.csv', row.names=FALSE)


### + Risk table S1 (Lose-lose)
aor_tbl <- expand.grid(region=c('afr', 'ssea'), risk=1)

aor_tbl_vulnT <- suit_vuln_vals3 %>% filter(risk.vulnT >0) %>% 
  group_by(region, risk.vulnT) %>% 
  summarize(Mha.vulnT = n()/100) %>%
  rename(risk=risk.vulnT)

aor_tbl_vulnA <- suit_vuln_vals3 %>% filter(risk.vulnA >0) %>% 
  group_by(region, risk.vulnA) %>% 
  summarize(Mha.vulnA = n()/100) %>%
  rename(risk=risk.vulnA)

aor_tbl_rich <- suit_vuln_vals3 %>% filter(risk.rich >0) %>% 
  group_by(region, risk.rich) %>% 
  summarize(Mha.rich = n()/100) %>%
  rename(risk=risk.rich)

aor_tbl <- left_join(aor_tbl, aor_tbl_vulnT)
aor_tbl <- left_join(aor_tbl, aor_tbl_vulnA)
aor_tbl <- left_join(aor_tbl, aor_tbl_rich)

aor_tbl_sum <- aor_tbl %>% dplyr::select(-region) %>% group_by(risk) %>% summarize_all(sum, na.rm=TRUE) %>% mutate(region='z.both')
aor_tbl_sum <- bind_rows(aor_tbl, aor_tbl_sum) %>% arrange(region, risk)

aor_tbl_wide <- aor_tbl_sum %>% gather(vuln, Mha, Mha.vulnT, Mha.vulnA, Mha.rich) %>% spread(region, Mha, fill='-') %>% arrange(vuln, risk) 
aor_tbl_wide <- aor_tbl_wide %>% group_by(vuln) %>% mutate(cumsum=cumsum(z.both))

tblS1 <- rbind(aoc_tbl_wide, aor_tbl_wide)

write.csv(tblS1, 'output/results/TableS1.csv', row.names=FALSE)

View(tblS1)


#### Numbers for the Results ####
suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(suitclass = cut(suit, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) %>% 
  mutate(vulnAclass = cut(vulnA, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8,  1), right=TRUE)) %>% 
  mutate(vulnTclass = cut(vulnT, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8,  1), right=TRUE)) %>% 
  mutate(richclass = cut(richness, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) 


# Of land available for rubber expansion, just 0.1 Mha in Africa and none in Asia/New Guinea had high bioclimatic suitability for rubber (suitability >0.8) (Figure 1).
suit_vuln_vals3 %>% filter(suit > 0.8) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 
suit_vuln_vals3 %>% filter(suit > 0.6) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 

# Among these highly suitable areas, none were minimal in extinction vulnerability (vulnerability ≤0.2) (Figure 1 and Table S1; see also Text S1).
suit_vuln_vals3 %>% filter(suit > 0.8 & vulnA <= 0.2) %>% group_by(region, suitclass, vulnAclass, comprom.vulnT.1) %>% summarize(n = n(), Mha = n()*0.01) %>% arrange(region, suitclass, comprom.vulnT.1)

# We identified 18.8 Mha of land meeting our criteria for ‘areas of compromise’ (Table S1, see also Text S1). 
suit_vuln_vals3 %>% filter(suit > 0.4 & vulnT <= 0.4) %>% summarize(n = n(), Mha = n()*0.01) 

# Africa had fewer areas of compromise (7.3 Mha) compared to Asia and New Guinea (11.5 Mha), 
suit_vuln_vals3 %>% filter(suit > 0.4 & vulnT <= 0.4) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 



# For Text S1 Result (vulnA)
# Of land available for rubber expansion, just 0.1 Mha in Africa and none in Asia/New Guinea had high bioclimatic suitability for rubber (suitability >0.8) (Figure 1).
suit_vuln_vals3 %>% filter(suit > 0.8) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 
suit_vuln_vals3 %>% filter(suit > 0.6) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 

# Among these highly suitable areas, none were minimal in extinction vulnerability (vulnerability ≤0.2) (Figure 1 and Table S1; see also Text S1).
suit_vuln_vals3 %>% filter(suit > 0.8 & vulnA <= 0.2) %>% group_by(region, suitclass, vulnAclass, comprom.vulnA.1) %>% summarize(n = n(), Mha = n()*0.01) %>% arrange(region, suitclass, comprom.vulnA.1)

# We identified 14.1 Mha of land meeting our criteria for ‘areas of compromise’ (Table S1, see also Text S1). 
suit_vuln_vals3 %>% filter(suit > 0.4 & vulnA <= 0.4) %>% summarize(n = n(), Mha = n()*0.01) 

# Africa had fewer areas of compromise (2.5 Mha) compared to Asia and New Guinea (11.6 Mha), 
suit_vuln_vals3 %>% filter(suit > 0.4 & vulnA <= 0.4) %>% group_by(region) %>% summarize(n = n(), Mha = n()*0.01) 


#### Number of species in analysis (Methods) ####
dflist <- c(list.files("output/afr", "_spp_in_", full.names=TRUE), list.files("output/ssea", "_spp_in_", full.names=TRUE))
dflist <- grep( "threat", dflist,  value=TRUE)

dflist <- c(list.files("output/afr", "_spp_in_", full.names=TRUE), list.files("output/ssea", "_spp_in_", full.names=TRUE))
dflist <- grep( "threat", dflist,  value=TRUE, invert=TRUE) #all spp

spplist <- lapply(dflist[c(1,5)], read.csv)
spplist <- rbindlist(spplist)
length(unique(spplist$x)) #789 amph threat, 2534

spplist <- lapply(dflist[c(2,6)], read.csv)
spplist <- rbindlist(spplist)
length(unique(spplist$x)) #737 birds, 6087

spplist <- lapply(dflist[c(3,7)], read.csv)
spplist <- rbindlist(spplist)
length(unique(spplist$x)) #1055 mammals, 3136

spplist <- lapply(dflist[c(4,8)], read.csv)
spplist <- rbindlist(spplist)
length(unique(spplist$x)) #862 reptiles, 3062




############# Fig 1 Interpolate Point Density Plot #############
suit_vuln_vals_gg <- suit_vuln_vals3 %>% dplyr::select(suit, richness, vulnA, vulnT, region) #236117

suit_vuln_vals_gg %>% group_by(region) %>% summarize(n=n())

suit_vuln_vals_gg_afr <- filter(suit_vuln_vals_gg, region=='afr', suit > 0.01)
suit_vuln_vals_gg_ssea <- filter(suit_vuln_vals_gg, region=='ssea', suit > 0.01)

nrow(suit_vuln_vals_gg_afr); nrow(suit_vuln_vals_gg_ssea) #afr: from 125134 to 42408; ssea: from 110983 to 22149
suit_vuln_vals_gg %>% group_by(region) %>% summarize(n=sum(suit<=0.01)) # 82726 dropped from afr; 88834 dropped from ssea

gg_bin_vulnT_afr <- ggplot(data=suit_vuln_vals_gg_afr, aes(y=suit, x=vulnT)) + 
  stat_bin_2d(binwidth = c(.02, .02)) + #, aes(fill = ..density..)
  scale_fill_gradient(high = "#132B43", low = "#56B1F7") +
  #scale_fill_gradient(high = "grey80", low="black") +
  geom_hline(yintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  geom_vline(xintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(-0.05,1), expand=expansion(mult=0, add=0.05), labels=c(0,0.2,0.4,0.6,0.8,'')) + 
  ylab('Rubber bioclimatic suitability') + xlab('Extinction vulnerability') +
  theme_cowplot(font_size=10) +
  theme(legend.position=c(0.8,0.8))

gg_bin_vulnT_ssea <- ggplot(data=suit_vuln_vals_gg_ssea, aes(y=suit, x=vulnT)) + 
  stat_bin_2d(binwidth = c(.02, .02)) +
  scale_fill_gradient(low = "#ff9b9b", high="darkred") +
  #scale_fill_gradient(high = "grey80", low="black") +
  geom_hline(yintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  geom_vline(xintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(-0.05,1), expand=expansion(mult=0, add=0.05), labels=c(0,0.2,0.4,0.6,0.8,'')) + 
  ylab('Rubber bioclimatic suitability') + xlab('Extinction vulnerability') +
  theme_cowplot(font_size=10) +
  theme(legend.position=c(0.8,0.8))

# AOC - explainer
aor.rect <- data.frame(xmin=0.61, xmax=1, ymin=0.0, ymax=0.39)
aor.text <- data.frame(x=0.8, y=0.2, text=c("Lose-\nLose" ))

aoc.rect <- data.frame(xmin=0, xmax=0.39, ymin=0.41, ymax=1)
aoc.text <- data.frame(x=c(0.16, 0.17), y=c(0.7,0.5), text=c("Area of", "Compromise" ))

ww.rect <- data.frame(xmin=0.01, xmax=0.19, ymin=0.81, ymax=0.99)
ww.text <- data.frame(x=0.1, y=0.9, text="Win-\nWin")

gg_fig1a_bin <- gg_bin_vulnT_afr + 
  geom_rect(data=ww.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aoc.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aor.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_text(data=ww.text,  aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aoc.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aor.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) 

gg_fig1b_bin <- gg_bin_vulnT_ssea + 
  geom_rect(data=ww.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aoc.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aor.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_text(data=ww.text,  aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aoc.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aor.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) 


gg_bin <- plot_grid(gg_fig1a_bin, gg_fig1b_bin, labels=c('A', 'B'), label_size=10,  
                           align = 'h', nrow=1, ncol=2, rel_widths = c(1, 1)) #+ 

cowplot::save_plot("output/results/fig1.png", gg_bin, base_width=6.85, base_height=6.85/2, dpi=300)


############# Text S1 (vulnA) Fig I Interpolate Point Density Plot #############

gg_bin_vulnA_afr <- ggplot(data=suit_vuln_vals_gg_afr, aes(y=suit, x=vulnA)) + 
  stat_bin_2d(binwidth = c(.02, .02)) + #, aes(fill = ..density..)
  scale_fill_gradient(high = "#132B43", low = "#56B1F7") +
  #scale_fill_gradient(high = "grey80", low="black") +
  geom_hline(yintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  geom_vline(xintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(-0.05,1), expand=expansion(mult=0, add=0.05), labels=c(0,0.2,0.4,0.6,0.8,'')) + 
  ylab('Rubber bioclimatic suitability')  + xlab('Extinction vulnerability\n(all species)') +
  theme_cowplot(font_size=10) +
  theme(legend.position=c(0.8,0.8))

gg_bin_vulnA_ssea <- ggplot(data=suit_vuln_vals_gg_ssea, aes(y=suit, x=vulnA)) + 
  stat_bin_2d(binwidth = c(.02, .02)) +
  scale_fill_gradient(low = "#ff9b9b", high="darkred") +
  #scale_fill_gradient(high = "grey80", low="black") +
  geom_hline(yintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  geom_vline(xintercept=seq(0.2,0.8,0.2), linetype=2, alpha=0.3) +
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(-0.05,1), expand=expansion(mult=0, add=0.05), labels=c(0,0.2,0.4,0.6,0.8,'')) + 
  ylab('Rubber bioclimatic suitability')  + xlab('Extinction vulnerability\n(all species)') +
  theme_cowplot(font_size=10) +
  theme(legend.position=c(0.8,0.8))


gg_fig1a_bin <- gg_bin_vulnA_afr + 
  geom_rect(data=ww.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aoc.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aor.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_text(data=ww.text,  aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aoc.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aor.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) 

gg_fig1b_bin <- gg_bin_vulnA_ssea + 
  geom_rect(data=ww.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aoc.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_rect(data=aor.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.3), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.7) +
  geom_text(data=ww.text,  aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aoc.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) +
  geom_text(data=aor.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.3), size=4) 


gg_bin <- plot_grid(gg_fig1a_bin, gg_fig1b_bin, labels=c('A', 'B'), label_size=10,  
                    align = 'h', nrow=1, ncol=2, rel_widths = c(1, 1)) #+ 

cowplot::save_plot("output/results/textS1_fig1.png", gg_bin, base_width=6.85, base_height=6.85/2, dpi=300)









##### ///Extras (not used in paper): -------------------------------

##### /Distrb of points by country: ####
table(suit_vuln_vals3$country)

country_aoc <- suit_vuln_vals3 %>% filter(comprom.vulnT.1 <7) %>% group_by(region, country) %>% summarize(Mha=n()*0.01, pct=n()/nrow(.)*100) %>% arrange(desc(pct))# 47% in Indonesia, 14% PNG, 
View(country_aoc)
write.csv(country_aoc, "output/results/aoc_by_country.csv")

country_suitable <- suit_vuln_vals3 %>% filter(suit >0.6) %>% group_by(region, country) %>% summarize(Mha=n()*0.01, pct=n()/nrow(.)*100) %>% arrange(desc(pct)) #countries with medium-high suitability. PNG not incld. 
# Indo, Guinea, Ethiopia, Liberia have >0.10 Mha areas of med/high suitability

country_lowvuln <- suit_vuln_vals3 %>% filter(vulnT <=0.4) %>% group_by(region, country) %>% summarize(Mha=n()*0.01, pct=n()/nrow(.)*100) %>% arrange(desc(pct)) #low vuln areas 

country_suitable <- suit_vuln_vals3 %>% filter(suit >0.4) %>% group_by(region, country) %>% summarize(Mha=n()*0.01, pct=n()/nrow(.)*100) %>% arrange(desc(pct)) #countries with medium-high suitability. PNG not incld. 
# Indo, Guinea, Ethiopia, Liberia have >0.10 Mha areas of med/high suitability


##### /Suitability in existing concessions ####
conc.gfw <- raster('output/ssea/various_rasters/concessions_gfw.tif')
conc.lifox <- raster('output/ssea/various_rasters/concessions_mmsea_li&fox.tif')
conc.hurni <- raster('output/ssea/various_rasters/concessions_mmsea_hurni.tif')

conc_stack_ssea <- stack(suit_vuln_stack_ssea, conc.gfw, conc.lifox, conc.hurni)

conc_stack_ssea_xy <- xyFromCell(conc_stack_ssea, 1:ncell(conc_stack_ssea))
# plot(conc_stack_ssea)
# freq(conc_stack_ssea)

conc_stack_ssea <- raster::extract(conc_stack_ssea, conc_stack_ssea_xy)

conc_stack_ssea <- as.data.frame(conc_stack_ssea) 
head(conc_stack_ssea)

conc_stack_ssea <- conc_stack_ssea %>% 
  rename(suit = names(conc_stack_ssea)[1], 
         pa = names(conc_stack_ssea)[2], 
         richness = names(conc_stack_ssea)[3], 
         vuln = names(conc_stack_ssea)[4],
         opsuit = names(conc_stack_ssea)[5],
         conc = names(conc_stack_ssea)[6],
         conc.gfw = names(conc_stack_ssea)[7], 
         conc.lifox = names(conc_stack_ssea)[8], 
         conc.hurni = names(conc_stack_ssea)[9])
head(conc_stack_ssea)

conc_stack_ssea <- cbind(conc_stack_ssea_xy, conc_stack_ssea)

conc_stack_ssea <- conc_stack_ssea %>% 
  mutate(suitclass = cut(suit, breaks=c(-0.1, 1e-10, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) %>% 
  mutate(vulnclass = cut(vuln, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) %>% 
  mutate(richclass = cut(richness, breaks=c(-0.1, 0.2, 0.4, 0.6, 0.8, 1), right=TRUE)) 

suit_vuln_conc <- conc_stack_ssea %>% filter(conc == 1)

suit_vuln_conc %>% group_by(suitclass) %>% summarize(area=n()*0.01, pct=area/(nrow(suit_vuln_conc)*0.01)*100 )
# suitclass     area    pct
# <fct>        <dbl>  <dbl>
#   1 (1e-10,0.2] 419.   86.4  
# 2 (0.2,0.4]    49.4  10.2  
# 3 (0.4,0.6]    12.0   2.47 
# 4 (0.6,0.8]     1.74  0.359
# 5 NA            2.81  0.580

suit_vuln_conc.gfw <- conc_stack_ssea %>% filter(conc.gfw == 1)
suit_vuln_conc.lifox <- conc_stack_ssea %>% filter(conc.lifox == 1)
suit_vuln_conc.hurni <- conc_stack_ssea %>% filter(conc.hurni == 1)
plot( y ~ suit, data=suit_vuln_conc, type='n')
points( y ~ suit, data=suit_vuln_conc.gfw)
points( y ~ suit, data=suit_vuln_conc.lifox, col='blue')
points( y ~ suit, data=suit_vuln_conc.hurni, col='red')


head(suit_vuln_conc)
suit_vuln_conc_all <- suit_vuln_conc %>% dplyr::select(x,y,suit, conc.gfw, conc.lifox, conc.hurni) %>% gather(key=conc.data.source, value=conc.present, conc.gfw, conc.lifox, conc.hurni, na.rm=TRUE) %>% mutate(region='ssea', status='Operating') %>% dplyr::select(-conc.present)



#### Suitability of planned rubber ###
library(sf)
conc_sf <- st_read('data/conc_sf_landmatrix.gpkg')
ssea_conc_sf <- st_crop(conc_sf, extent(ssea_countries_wgs)) %>% st_transform(crs=as.character(crs(mat_suit_ssea)))
afr_conc_sf <- st_crop(conc_sf, extent(afr_countries_wgs)) %>% st_transform(crs=as.character(crs(mat_suit_afr)))

levels(conc_sf$status)

ssea_conc_sf_intended <- ssea_conc_sf %>% filter(status == "Intended")
afr_conc_sf_intended <- afr_conc_sf %>% filter(status == "Intended")

ssea_conc_sf <- ssea_conc_sf %>% filter(status == "Intended")
afr_conc_sf <- afr_conc_sf %>% filter(status == "Intended")

head(suit_conc_ssea)

suit_conc_ssea %>% group_by(status) %>% summarise(meanSuit = mean(suit, na.rm=TRUE), minSuit = min(suit), maxSuit = max(suit, na.rm=TRUE))
suit_conc_afr %>% group_by(status) %>% summarise(meanSuit = mean(suit, na.rm=TRUE), minSuit = min(suit), maxSuit = max(suit, na.rm=TRUE))

plot( Y ~ suit, data=suit_conc_ssea)
plot( Y ~ suit, data=suit_conc_afr)

suit_conc_ssea_tocombine <- suit_conc_ssea %>% dplyr::select(x=X,y=Y,suit, status) %>% mutate(conc.data.source='landmatrix', region='ssea')
suit_conc_afr_tocombine <- suit_conc_afr %>% dplyr::select(x=X,y=Y,suit, status) %>% mutate(conc.data.source='landmatrix', region='afr')

st_geometry(suit_conc_ssea_tocombine) <- NULL
st_geometry(suit_conc_afr_tocombine) <- NULL

suit_vuln_conc_all <- rbind(suit_vuln_conc_all, suit_conc_ssea_tocombine, suit_conc_afr_tocombine)

head(suit_vuln_conc_all)

ggplot(data=suit_vuln_conc_all, aes(y=y, x=suit, color=conc.data.source, shape=region)) +
  geom_point()


suit_vuln_conc_all_intended <- suit_vuln_conc_all %>% filter(status == 'Intended')
ggplot(data=suit_vuln_conc_all_intended, aes(y=y, x=suit, color=conc.data.source, shape=region)) +
  geom_point()


#Interesting, all low or zero suit in Asia, in Afr is also low suitability - for intened conc
#Operating and startup conc, mixed suit



###### /Correlation between suitability and richness/vulnerability #######
suit_vuln_vals3 <- fread( 'output/suit_vuln_vals3_may2020.csv')
head(suit_vuln_vals3)

filter(suit_vuln_vals3, suit > 0.8) %>% dplyr::select(vulnT) %>% range() #[1] 0.2666544 0.3681979
loselose <- filter(suit_vuln_vals3, suit <= 0.4, vulnT > 0.6) 
table(loselose$region)
nrow(loselose) #10998->10736

filter(suit_vuln_vals3, suit <= 0.2, vulnT > 0.6) %>% nrow() #10000->9740


# Before removing PAs and concessions 
cor.test(suit_vuln_vals2$suit, suit_vuln_vals2$vulnA, method="spearman" )
cor.test(suit_vuln_vals2$suit, suit_vuln_vals2$vulnT, method="spearman" )


# exclude PAs
cor.test(suit_vuln_vals3$suit, suit_vuln_vals3$richness, method="spearman" )
cor.test(suit_vuln_vals3$suit, suit_vuln_vals3$vulnA, method="spearman" )
cor.test(suit_vuln_vals3$suit, suit_vuln_vals3$vulnT, method="spearman" )


#### afr
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$richness, method="spearman" ) 
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$vulnA, method="spearman" ) 
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='afr', ]$vulnT, method="spearman" ) 
# afr vulnT
# S = 3.7855e+14, p-value < 2.2e-16
# rho 0.03573828 (didn't change from apr2020 dataset)

#### ssea
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$richness, method="spearman" ) 
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$vulnA, method="spearman" )
cor.test(suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$suit, suit_vuln_vals3[suit_vuln_vals3$region=='ssea', ]$vulnT, method="spearman" )
# ssea vuln T 
# S = 1.2472e+13, p-value < 2.2e-16
#  -0.2390279 







