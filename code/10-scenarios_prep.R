#### rubberxbiodiversityCB 
#### 10-scenarios_prep.R ####

rm(list=ls())
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'

library(dplyr)
library(tidyr)
library(raster)
library(fasterize)
library(sf)
library(cowplot)
library(data.table)


###### Load DF, suit_vuln_vals3, made in 21.3-numbersforcell.R 
suit_vuln_vals3 <- fread('output/suit_vuln_vals3.csv')


##### Assign Levels of compromise ####
suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(comprom.vulnT.1 =  ifelse((vulnT<=0.2 & suit >0.8), 1,  #production driven compromise (compromise on vuln, preserve suit)
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 3,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 5,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      9)))))),
         
         comprom.vulnT.2 =  ifelse((vulnT<=0.2 & suit >0.8), 1, #conserrvation driven compromise (compromise on suit, preserve vuln)
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 4,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 5, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      9)))))), 
         
         comprom.vulnT.3 =  ifelse((vulnT<=0.2 & suit >0.8), 1, #equal weight to both compromise
                                   ifelse((vulnT<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnT<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnT<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnT<=0.4 & suit>0.6 & suit<=0.8), 3, 
                                                               ifelse((vulnT<=0.4 & suit>0.4 & suit<=0.6), 4, 
                                                                      9)))))) )

suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(comprom.vulnA.1 =  ifelse((vulnA<=0.2 & suit >0.8), 1,  #production driven compromise (compromise on vuln, preserve suit)
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 3,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 5,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      9)))))),
         
         comprom.vulnA.2 =  ifelse((vulnA<=0.2 & suit >0.8), 1, #conserrvation driven compromise (compromise on suit, preserve vuln)
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 4,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 5, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                                      9)))))), 
         
         comprom.vulnA.3 =  ifelse((vulnA<=0.2 & suit >0.8), 1, #equal weight to both compromise
                                   ifelse((vulnA<=0.2 & suit>0.6 & suit<=0.8), 2,
                                          ifelse((vulnA<=0.2 & suit>0.4 & suit<=0.6), 3,
                                                 ifelse((vulnA<=0.4 & suit >0.8), 2,
                                                        ifelse((vulnA<=0.4 & suit>0.6 & suit<=0.8), 3, 
                                                               ifelse((vulnA<=0.4 & suit>0.4 & suit<=0.6), 4, 
                                                                      9)))))) )

table(suit_vuln_vals3$comprom.vulnT.1, suit_vuln_vals3$comprom.vulnT.2)
table(suit_vuln_vals3$comprom.vulnT.1, suit_vuln_vals3$comprom.vulnT.3)

table(suit_vuln_vals3$comprom.vulnA.1, suit_vuln_vals3$comprom.vulnA.2)
table(suit_vuln_vals3$comprom.vulnA.1, suit_vuln_vals3$comprom.vulnA.3)



####  Create new columns of values to be used in Scenario Analysis ####

# Using rowMeans with na.rm=TRUE is misleading, as it will calculate an average despite missing data in avg.suit.acc, avg.vuln.carb

# Using manual averaging will retain missing data in avg.suit.acc, avg.vuln.carb
suit_vuln_vals3 <- suit_vuln_vals3 %>% 
  mutate(suit.inv=1-suit, 
         avg.suit.acc = (suit.inv+acc)/2,
         avg.vulnA.carb = (vulnA+carb)/2,
         avg.vulnT.carb = (vulnT+carb)/2,
         optim4.vulnA = (suit.inv+vulnA+acc+carb)/4,
         optim4.vulnT = (suit.inv+vulnT+acc+carb)/4)

# checking NAs
colSums(is.na(suit_vuln_vals3)) #correct!

suit_vuln_vals3 %>% filter(is.na(.$carb)==TRUE) %>% group_by(region) %>% summarize(n=n()) #5554 NAs in ssea vs 221 in afr ... 
suit_vuln_vals3 %>% filter(carb==0) %>% group_by(region) %>% summarize(n=n()) #0s (desert/barren)
# I've double checked the carbon rasters, nothing seems wrong with it 
# Keep carbon NA as missing data, these cells will not be considered in the compromise or conservation scenarios. 2.44% of the cells will not be considered



# check if all combos of x and y are distinct
check <- suit_vuln_vals3 %>% distinct(x,y) 
identical(nrow(check), nrow(suit_vuln_vals3)) #If TRUE, correct

suit_vuln_vals3[duplicated(suit_vuln_vals3[, c("x","y")]) ]$x #another method to check for dups, if no dups, great!

# x/y are distinct, can use them as keys. create new column called cell.id
suit_vuln_vals3 <- suit_vuln_vals3 %>% arrange(x, y) %>% mutate(cell.id = 1:nrow(.))
head(suit_vuln_vals3)


suit_vuln_vals3 <- suit_vuln_vals3 %>% dplyr::select(cell.id, x, y, region, forest, country, suit, vulnA, vulnT, comprom.vulnA.1:comprom.vulnA.3, comprom.vulnT.1:comprom.vulnT.3, suit.inv:optim4.vulnT) 

suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]

str(suit_vuln_vals3) #21 vars, 236117 obs

fwrite(suit_vuln_vals3, 'output/suit_vuln_vals3_scenario.csv') #54MB


##### <---- START HERE ONCE MADE suit_vuln_vals3_scenario.csv ####

# Obtaining xy coords for raster::extract of species presence 
suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv') 
head(suit_vuln_vals3)

spdf_afr <- suit_vuln_vals3 %>% filter(region=='afr') %>% dplyr::select(x,y)
spdf_ssea <- suit_vuln_vals3 %>% filter(region=='ssea') %>% dplyr::select(x,y)

mat_vuln_afr <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_all_std_mask.tif')
mat_vuln_ssea <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_all_std_mask.tif')

#plot(spdf_afr)
#plot(spdf_ssea)


#### FOREST DEPENDENT SPECIES WHOSE RANGES ARE ENITRELY WITHIN STUDY REGION ####
## Forest dependent spp list... 
forspp_corr <- read.csv("data/forestdependentspecies_iucnredlist2019.csv") # Derived from Table S3 from Tracewski et al. (2016). Toward quantification of the impact of 21st-century deforestation on the extinction risk of terrestrial vertebrates. Conserv. Biol. 30, 1070–1079.

forspp_corr %>% group_by(class) %>% summarize(n_spp = n_distinct(SCINAME)) #amph 3526; bird 6265; mammal 1357


### + Make polygon with extent of Africa continent ####

#extents in WGS
xmin = -25.5 #W
xmax =  63.5 #E
ymin = -35 #S 
ymax =  19 #N
#5deg buffer was used for cropping suitability layers

afr_bbox_poly <- matrix(c(xmin, ymin,
                          xmax, ymin,
                          xmax, ymax,
                          xmin, ymax,
                          xmin, ymin), ncol=2, byrow=TRUE)

afr_bbox_poly <- st_polygon(list(afr_bbox_poly))
plot(afr_bbox_poly)



### + Make polygon with extent of SSEA ####
xmin =  69.25
xmax =  158.4 #161.6, to include Cornufus guppyi/Solomon islands 
ymin = -12.5 
ymax =  30.5

ssea_bbox_poly <- matrix(c(xmin, ymin,
                           xmax, ymin,
                           xmax, ymax,
                           xmin, ymax,
                           xmin, ymin), ncol=2, byrow=TRUE) 

#cropExtent_wgs <- extent(c(xmin-5,xmax+5,ymin-5,ymax+5))

ssea_bbox_poly <- st_polygon(list(ssea_bbox_poly))
plot(ssea_bbox_poly)




#### (A) Forest dependent Amphibians ##### 

# Load amphibian species range layers
layers <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_amphibians_correctedto2019.gpkg")) #takes 5.4S
head(layers)
layers <- layers %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX")
layers <- layers %>% dplyr::filter(binomial %in% forspp_corr$SCINAME) #3976
layers <- layers %>% dplyr::mutate(presence_richness = 1) 


###Africa
system.time(layers$indicator.afr <- st_within(layers, afr_bbox_poly, sparse = FALSE) ) #3.81s
sppnotonlyinafr <- layers %>% filter(indicator.afr == FALSE) #3357 (entire continent), 3432 (Study area)
spponlyinafr <- layers %>% filter(indicator.afr == TRUE) #619 (entire continent), 544 (study area)

#check that none of the selected spp actually have ranges outside of africa (in the form of separate polygon layers)
spponlyinafr1 <- as.character(unique(spponlyinafr$binomial))
sppnotonlyinafr1 <- as.character(unique(sppnotonlyinafr$binomial))

spptoremove <- spponlyinafr1[spponlyinafr1 %in% sppnotonlyinafr1] #zero
sppnotonlyinafr1[sppnotonlyinafr1 %in% spponlyinafr1] #


###SSEA
system.time(layers$indicator.ssea <- st_within(layers, ssea_bbox_poly, sparse = FALSE) ) #3.92s
sppnotonlyinssea <- layers %>% filter(indicator.ssea == FALSE) #2931 
spponlyinssea <- layers %>% filter(indicator.ssea == TRUE) #1045 

#check that none of the selected spp actually have ranges outside of ssea (in the form of separate polygon layers)
spponlyinssea1 <- as.character(unique(spponlyinssea$binomial)) #858
sppnotonlyinssea1 <- as.character(unique(sppnotonlyinssea$binomial)) #2674

spptoremove <- spponlyinssea1[spponlyinssea1 %in% sppnotonlyinssea1] #7 spp have some portion of their range outside our study 
#sppnotonlyinssea1[sppnotonlyinssea1 %in% spponlyinssea1]
# c("Leptobrachella oshanensis", "Bufo gargarizans", "Eleutherodactylus planirostris",  "Rhacophorus feae", "Odorrana chloronota", "Cornufer guppyi",   "Fejervarya limnocharis")
# "Cornufer guppyi" is one iffy one because Polynesian. For simplicity, 99% of its range is not in our study region, so we don't incld

check <- layers %>% filter(binomial %in% spptoremove)
plot(ssea_bbox_poly)
plot(st_geometry(check), col=rainbow(7), add=TRUE)

st_geometry(check) <- NULL
check.globalrangesize <- check %>% group_by(binomial) %>% summarise(global.range.size = sum(SHAPE_Area) )
check <- left_join(check, check.globalrangesize)
check2 <- mutate(check, prop.in.studyarea = round(SHAPE_Area/global.range.size, 3)) %>% arrange(binomial) %>% dplyr::select(binomial, indicator.ssea, global.range.size, prop.in.studyarea )
View(check2)

check2 %>% filter(indicator.ssea==TRUE, prop.in.studyarea >= 0.99) #for the part of spp range that IS within study region (indicator.ssea==TRUE), is it more than 99% of their global range?
#Ans: none of the 7 spp meet this condition, exclude.


# Remove these from spponlyinssea
spponlyinssea <- spponlyinssea %>% filter(!(binomial %in% spptoremove)) #1038


# # Project shapefile
system.time(layers_aea_afr <- st_transform(spponlyinafr, crs=as.character(crs(mat_vuln_afr))) ) #1s
system.time(layers_aea_ssea <- st_transform(spponlyinssea, crs=as.character(crs(mat_vuln_ssea))) ) #3s

#plot(st_geometry(layers_aea_afr))
#plot(st_geometry(layers_aea_ssea))

localforestamph_afr <- as.character(sort(unique(spponlyinafr$binomial))) #480
localforestamph_ssea <- as.character(sort(unique(spponlyinssea$binomial))) #851 


check <- inner_join(as.tibble(localforestamph_afr), as.tibble(localforestamph_ssea))

#### +loop for joining species to suit_vuln ####

#note: need to clear memory before running. split into afr and ssea to reduce chance of R error (cannot allocate vector).
rm(list=setdiff(ls(), c("forspp_corr", "afr_bbox_poly", "ssea_bbox_poly", "GISfolder", "layers_aea_afr", "layers_aea_ssea", "localforestamph_afr", "localforestamph_ssea", "mat_vuln_afr", "mat_vuln_ssea", "spdf_afr", "spdf_ssea", "suit_vuln_vals3" )))
gc()

#suit_vuln_amph_afr <- suit_vuln_vals3 #%>% filter(comprom>0) #to only run analysis for AOC cells (save time now), or for all available cells for expansion (potentially save time later?)
#rm(suit_vuln_amph, suit_vuln_amph_afr)

# Run loop separately for afr and ssea coz i want to raster stack them later
list_spp_presence_afr <- list()
output <- raster(crs=crs(mat_vuln_afr), ext=extent(mat_vuln_afr) , resolution=res(mat_vuln_afr))


# Afr (amph)
system.time(
  for(i in 1:length(localforestamph_afr)){
    geom <- layers_aea_afr[layers_aea_afr$binomial == localforestamph_afr[i], c('binomial', 'presence_richness')] 
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    list_spp_presence_afr[[i]] <- out
    rm(geom, out)
  }
)
#3.02s to put rasters in a list

stack_spp_presence <- stack(list_spp_presence_afr)
ext <- raster::extract(stack_spp_presence, spdf_afr)
dim(ext) #125134 x 480
suit_vuln_amph_afr <- as.data.frame(cbind(spdf_afr, ext))
names(suit_vuln_amph_afr) <- c('x', 'y', localforestamph_afr)
head(suit_vuln_amph_afr)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_amph_afr>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE) #434
suit_vuln_amph_afr <- suit_vuln_amph_afr[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])] #432
all(colSums( suit_vuln_amph_afr[3:ncol(suit_vuln_amph_afr)]>0 , na.rm=TRUE) > 0 ) #MUST BE TRUE

rm(stack_spp_presence, ext)



# SSEA (amph)
list_spp_presence_ssea <- list()
output <- raster(crs=crs(mat_vuln_ssea), ext=extent(mat_vuln_ssea) , resolution=res(mat_vuln_ssea))

system.time(
    for(i in 1:length(localforestamph_ssea)){
    geom <- layers_aea_ssea[layers_aea_ssea$binomial == localforestamph_ssea[i], c('binomial', 'presence_richness')] 
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    
    list_spp_presence_ssea[[i]] <- out 

    rm(geom, out)
  }
)
#4.72s to put rasters in a list

stack_spp_presence <- stack(list_spp_presence_ssea)
ext <- raster::extract(stack_spp_presence, spdf_ssea)
dim(ext) #110983 x 851
suit_vuln_amph_ssea <- as.data.frame(cbind(spdf_ssea, ext))
names(suit_vuln_amph_ssea) <- c('x', 'y', localforestamph_ssea)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_amph_ssea>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE) #749
suit_vuln_amph_ssea <- suit_vuln_amph_ssea[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])]
all(colSums( suit_vuln_amph_ssea[3:ncol(suit_vuln_amph_ssea)]>0 , na.rm=TRUE) > 0 ) #TRUE


rm(list_spp_presence_ssea, stack_spp_presence, ext)

head(suit_vuln_vals3)
names(suit_vuln_amph_afr)[1:20]
suit_vuln_amph <- suit_vuln_vals3 %>% dplyr::select(cell.id, x, y, region)
suit_vuln_amph <- left_join(suit_vuln_amph, suit_vuln_amph_afr, by=c("x", "y"))
suit_vuln_amph <- left_join(suit_vuln_amph, suit_vuln_amph_ssea, by=c("x", "y"))

str(suit_vuln_amph)
all(colSums( suit_vuln_amph[, 4:ncol(suit_vuln_amph)]>0 , na.rm=TRUE) > 0 ) #TRUE

fwrite(suit_vuln_amph, 'output/temp/suit_vuln_scenario_amph.csv') #208253 obs





#### (B) Forest dependent Birds ##### 
#Remove everything but these objects that we need
rm(list=setdiff(ls(), c("forspp_corr", "afr_bbox_poly", "ssea_bbox_poly", "GISfolder","suit_vuln_vals3", "spdf_afr", "spdf_ssea", "mat_vuln_afr", "mat_vuln_ssea")))
gc()

# Load bird species range layers
system.time(layers_2 <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_wgs.gpkg")) ) #72-109s to load
names(layers_2)


# Filter only forest dependent spp
layers_2 <- layers_2 %>% filter(SCINAME %in% forspp_corr$SCINAME) #9144

table(layers_2$redlistCategory)
layers_2 <- layers_2 %>% dplyr::filter(redlistCategory != "EW") %>% dplyr::filter(redlistCategory != "EX")


#Create new column to indicate spp presence in that cell (for richness rasters)
layers_2 <- layers_2 %>% dplyr::mutate(presence_richness = 1) 


###Africa
system.time(layers_2$indicator.afr <- st_within(layers_2, afr_bbox_poly, sparse = FALSE) ) #19.71s
sppnotonlyinafr <- layers_2 %>% filter(indicator.afr == FALSE) #8177 (Study area)
spponlyinafr <- layers_2 %>% filter(indicator.afr == TRUE) #967 (study area)

#check that none of the selected spp actually have ranges outside of africa (in the form of separate polygon layers)
sppnotonlyinafr1 <- as.character(unique(sppnotonlyinafr$SCINAME)) #5477
spponlyinafr1 <- as.character(unique(spponlyinafr$SCINAME)) #801

spptoremove <- spponlyinafr1[spponlyinafr1 %in% sppnotonlyinafr1] #19
sppnotonlyinafr1[sppnotonlyinafr1 %in% spponlyinafr1]

check <- layers_2 %>% filter(SCINAME %in% spptoremove)
plot(afr_bbox_poly)
plot(st_geometry(check), col=rainbow(9), add=TRUE)

st_geometry(check) <- NULL
check.globalrangesize <- check %>% group_by(SCINAME) %>% summarise(global.range.size = sum(Shape_Area) )
check <- left_join(check, check.globalrangesize)

check2 <- mutate(check, prop.in.studyarea = round(Shape_Area/global.range.size, 3)) %>% arrange(SCINAME) %>% dplyr::select(SCINAME, indicator.afr, global.range.size, prop.in.studyarea )
View(check2)

check2 %>% filter(indicator.afr==TRUE, prop.in.studyarea >= 0.99)
# only one spp "Nesoenas picturatus" has >99% range in study area. remove it from spptoremove
# Nesoenas picturatus 

check <- layers_2 %>% filter(SCINAME == "Nesoenas picturatus")
check[check$indicator.afr==TRUE, ]$Shape_Area / sum(check$Shape_Area) >= 0.99 #Check if area falling within study area is >=99% of their global range

spptoremove <- spptoremove[spptoremove!="Nesoenas picturatus"] # 

# Remove these from spponlyinssea
spponlyinafr <- spponlyinafr %>% filter(!(SCINAME %in% spptoremove)) #949



###SSEA
system.time(layers_2$indicator.ssea <- st_within(layers_2, ssea_bbox_poly, sparse = FALSE) ) #22s
sppnotonlyinssea <- layers_2 %>% filter(indicator.ssea == FALSE) #6975
spponlyinssea <- layers_2 %>% filter(indicator.ssea == TRUE) #2169

#check that none of the selected spp actually have ranges outside of sseaica (in the form of separate polygon layers)
spponlyinssea1 <- as.character(unique(spponlyinssea$SCINAME)) #1833
sppnotonlyinssea1 <- as.character(unique(sppnotonlyinssea$SCINAME)) #4594 

spptoremove <- spponlyinssea1[spponlyinssea1 %in% sppnotonlyinssea1] #168
#sppnotonlyinssea1[sppnotonlyinssea1 %in% spponlyinssea1]

check <- layers_2 %>% filter(SCINAME %in% spptoremove) #451
plot(ssea_bbox_poly)
#plot(st_geometry(check), col=rainbow(57), add=TRUE)

st_geometry(check) <- NULL
check.globalrangesize <- check %>% group_by(SCINAME) %>% summarise(global.range.size = sum(Shape_Area) )
check <- left_join(check, check.globalrangesize)

check2 <- mutate(check, prop.in.studyarea = round(Shape_Area/global.range.size, 3)) %>% arrange(SCINAME) %>% dplyr::select(SCINAME, indicator.ssea, global.range.size, prop.in.studyarea )

check2 %>% filter(indicator.ssea==TRUE, prop.in.studyarea >= 0.99) #for the part of spp range that IS within study region (indicator.ssea==TRUE), is it more than 99% of their global range?
#Ans: none of the 7 spp meet this condition, exclude.
# only one spp "Garrulax caerulatus" has >99% range in study area. remove it from spptoremove

check <- layers_2 %>% filter(SCINAME =="Garrulax caerulatus") #Introduced in (probably Hawaii), yes should include in our analysis
plot(ssea_bbox_poly)
plot(st_geometry(check), col="red", add=TRUE)


spptoremove <- spptoremove[spptoremove!="Garrulax caerulatus"] # Should include in our analysiss..so I manually include it

# Remove these from spponlyinssea
spponlyinssea <- spponlyinssea %>% filter(!(SCINAME %in% spptoremove)) #1943 (from 2169)


## Project shapefile
system.time(layers_aea_afr <- st_transform(spponlyinafr, crs=as.character(crs(mat_vuln_afr))) ) #1.23s
system.time(layers_aea_ssea <- st_transform(spponlyinssea, crs=as.character(crs(mat_vuln_ssea))) ) #6.51s


localforestbird_afr <- as.character(sort(unique(spponlyinafr$SCINAME))) #783
localforestbird_ssea <- as.character(sort(unique(spponlyinssea$SCINAME))) #1666




#### +loop for joining species to suit_vuln ####

#note: need to clear memory before running. split into afr and ssea to reduce chance of R error (cannot allocate vector).
rm(list=setdiff(ls(), c("forspp_corr", "afr_bbox_poly", "ssea_bbox_poly", "GISfolder","suit_vuln_vals3", "spdf_afr", "spdf_ssea", "mat_vuln_afr", "mat_vuln_ssea", "localforestbird_afr", "localforestbird_ssea", "localforestbird_afr", "localforestbird_ssea", "layers_aea_afr", "layers_aea_ssea")))
gc()

#suit_vuln_bird_afr <- suit_vuln_vals3 #%>% filter(comprom>0) #to only run analysis for AOC cells (save time now), or for all available cells for expansion (potentially save time later?)
#rm(suit_vuln_bird, suit_vuln_bird_afr)

# Run loop separately for afr and ssea coz i want to raster stack them later
# Run SSEA (bird) first because requires more memory

list_spp_presence <- list()
output <- raster(crs=crs(mat_vuln_ssea), ext=extent(mat_vuln_ssea) , resolution=res(mat_vuln_ssea))

head(layers_aea_ssea)
system.time(
  for(i in 1:length(localforestbird_ssea)){
    geom <- layers_aea_ssea[layers_aea_ssea$SCINAME == localforestbird_ssea[i], c('SCINAME', 'presence_richness')] 
    
    # Fasterize and convert to df
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    list_spp_presence[[i]] <- out
    rm(geom,out)
  }
)
#8.19s to put rasters in a list, only objects in memory were:

stack_spp_presence <- stack(list_spp_presence)
ext <- raster::extract(stack_spp_presence, spdf_ssea)
dim(ext) #110983 x 1666
suit_vuln_bird_ssea <- as.data.frame(cbind(spdf_ssea, ext))
names(suit_vuln_bird_ssea) <- c('x', 'y', localforestbird_ssea)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_bird_ssea>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE)
suit_vuln_bird_ssea <- suit_vuln_bird_ssea[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])]
all(colSums( suit_vuln_bird_ssea[3:ncol(suit_vuln_bird_ssea)]>0 , na.rm=TRUE) > 0 ) #TRUE

rm(list_spp_presence, stack_spp_presence, ext, onlyGoodcolumns_idx, i, layers_aea_ssea)
gc()

# afr birds
list_spp_presence <- list()
output <- raster(crs=crs(mat_vuln_afr), ext=extent(mat_vuln_afr) , resolution=res(mat_vuln_afr))

system.time(
  for(i in 1:length(localforestbird_afr)){
    geom <- layers_aea_afr[layers_aea_afr$SCINAME == localforestbird_afr[i], c('SCINAME', 'presence_richness')] 
    
    # Fasterize and convert to df
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    list_spp_presence[[i]] <- out
    rm(geom,out)
  }
)
# 3s to put rasters in a list with the rm(geom,out)

stack_spp_presence <- stack(list_spp_presence)
ext <- raster::extract(stack_spp_presence, spdf_afr)
dim(ext) #125134 x 783
suit_vuln_bird_afr <- as.data.frame(cbind(spdf_afr, ext))
names(suit_vuln_bird_afr) <- c('x', 'y', localforestbird_afr)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_bird_afr>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE)
suit_vuln_bird_afr <- suit_vuln_bird_afr[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])]
all(colSums( suit_vuln_bird_afr[3:ncol(suit_vuln_bird_afr)]>0 , na.rm=TRUE) > 0 ) #TRUE

rm(stack_spp_presence, list_spp_presence, ext, output)

suit_vuln_bird <- suit_vuln_vals3 %>% dplyr::select(cell.id, x, y, region)
suit_vuln_bird <- left_join(suit_vuln_bird, suit_vuln_bird_afr, by=c('x', 'y'))
suit_vuln_bird <- left_join(suit_vuln_bird, suit_vuln_bird_ssea, by=c('x', 'y'))

head(names(suit_vuln_bird), 50)
all(colSums( suit_vuln_bird[, 5:ncol(suit_vuln_bird)]>0 , na.rm=TRUE) > 0 ) #TRUE

fwrite(suit_vuln_bird, 'output/temp/suit_vuln_scenario_bird.csv')





#### (C) Forest dependent Mammals #####  
#Remove everything but these objects that we need
rm(list=setdiff(ls(), c("forspp_corr", "afr_bbox_poly", "ssea_bbox_poly", "GISfolder","suit_vuln_vals3", "spdf_afr", "spdf_ssea", "mat_vuln_afr", "mat_vuln_ssea")))
gc()

# Load amphibian species range layers
layers <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_mammals_correctedto2019.gpkg")) #takes 5.4S
head(layers)
layers <- layers %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX")
layers <- layers %>% dplyr::filter(binomial %in% forspp_corr$SCINAME) #3976
layers <- layers %>% dplyr::mutate(presence_richness = 1) 


###Africa
system.time(layers$indicator.afr <- st_within(layers, afr_bbox_poly, sparse = FALSE) ) #0.5s
sppnotonlyinafr <- layers %>% filter(indicator.afr == FALSE) #1867 (Study area)
spponlyinafr <- layers %>% filter(indicator.afr == TRUE) #460 (study area)

#check that none of the selected spp actually have ranges outside of africa (in the form of separate polygon layers)
sppnotonlyinafr1 <- as.character(unique(sppnotonlyinafr$binomial)) #1026
spponlyinafr1 <- as.character(unique(spponlyinafr$binomial)) #328

spptoremove <- spponlyinafr1[spponlyinafr1 %in% sppnotonlyinafr1] #only one
sppnotonlyinafr1[sppnotonlyinafr1 %in% spponlyinafr1]

check <- layers %>% filter(binomial == "Suncus etruscus")
plot(st_geometry(check), col='red')
plot(afr_bbox_poly, add=TRUE) #most of range outside our study area. 

# Remove these from spponlyinssea
spponlyinafr <- spponlyinafr %>% filter(!(binomial %in% spptoremove)) #458

###SSEA
system.time(layers$indicator.ssea <- st_within(layers, ssea_bbox_poly, sparse = FALSE) ) #0.532s
sppnotonlyinssea <- layers %>% filter(indicator.ssea == FALSE) #1142
spponlyinssea <- layers %>% filter(indicator.ssea == TRUE) #1185

#check that none of the selected spp actually have ranges outside of sseaica (in the form of separate polygon layers)
sppnotonlyinssea1 <- as.character(unique(sppnotonlyinssea$binomial)) #877
spponlyinssea1 <- as.character(unique(spponlyinssea$binomial)) #504

spptoremove <- spponlyinssea1[spponlyinssea1 %in% sppnotonlyinssea1] #28
#sppnotonlyinssea1[sppnotonlyinssea1 %in% spponlyinssea1]

check <- layers %>% filter(binomial %in% spptoremove) #
plot(ssea_bbox_poly)
plot(st_geometry(check), col=rainbow(57), add=TRUE)

st_geometry(check) <- NULL
check.globalrangesize <- check %>% group_by(binomial) %>% summarise(global.range.size = sum(SHAPE_Area) )
check <- left_join(check, check.globalrangesize)
check2 <- mutate(check, prop.in.studyarea = round(SHAPE_Area/global.range.size, 3)) %>% filter(indicator.ssea==TRUE, prop.in.studyarea >= 0.99)
# none of these spp have >99% range in study area. 
rm(check.globalrangesize, check, check2)

# Remove these from spponlyinssea
spponlyinssea <- spponlyinssea %>% filter(!(binomial %in% spptoremove)) #1062


# # Project shapefile
system.time(layers_aea_afr <- st_transform(spponlyinafr, crs=as.character(crs(mat_vuln_afr))) ) #0.14s
system.time(layers_aea_ssea <- st_transform(spponlyinssea, crs=as.character(crs(mat_vuln_ssea))) ) #0.3s

#plot(st_geometry(layers_aea_afr))
#plot(st_geometry(layers_aea_ssea))

localforestmammal_afr <- as.character(sort(unique(spponlyinafr$binomial))) #327
localforestmammal_ssea <- as.character(sort(unique(spponlyinssea$binomial))) #476


#### +loop for joining species to suit_vuln ####

rm(layers, sppnotonlyinafr, sppnotonlyinssea, spponlyinafr1, spponlyinssea1, spptoremove)
gc()

# Run loop separately for afr and ssea coz i want to raster stack them later
list_spp_presence <- list()
output <- raster(crs=crs(mat_vuln_afr), ext=extent(mat_vuln_afr) , resolution=res(mat_vuln_afr))

system.time(
  for(i in 1:length(localforestmammal_afr)){
    geom <- layers_aea_afr[layers_aea_afr$binomial == localforestmammal_afr[i], c('binomial', 'presence_richness')] 
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    list_spp_presence[[i]] <- out
    rm(geom, out)
  }
)
#1.42s to put rasters in a list

stack_spp_presence <- stack(list_spp_presence)
ext <- raster::extract(stack_spp_presence, spdf_afr)
dim(ext) #125134 x 327
suit_vuln_mammal_afr <- as.data.frame(cbind(spdf_afr, ext))
names(suit_vuln_mammal_afr) <- c('x', 'y', localforestmammal_afr)
head(suit_vuln_mammal_afr)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_mammal_afr>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE)
suit_vuln_mammal_afr <- suit_vuln_mammal_afr[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])] #307
all(colSums( suit_vuln_mammal_afr[3:ncol(suit_vuln_mammal_afr)]>0 , na.rm=TRUE) > 0 ) #MUST BE TRUE

rm(stack_spp_presence, ext)

# SSEA (mammal)
list_spp_presence <- list()
output <- raster(crs=crs(mat_vuln_ssea), ext=extent(mat_vuln_ssea) , resolution=res(mat_vuln_ssea))

system.time(
  for(i in 1:length(localforestmammal_ssea)){
    geom <- layers_aea_ssea[layers_aea_ssea$binomial == localforestmammal_ssea[i], c('binomial', 'presence_richness')] 
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    list_spp_presence[[i]] <- out
    rm(geom, out)
  }
)
#2.34s to put rasters in a list

stack_spp_presence <- stack(list_spp_presence)
ext <- raster::extract(stack_spp_presence, spdf_ssea)
dim(ext) #110983 x 476
suit_vuln_mammal_ssea <- as.data.frame(cbind(spdf_ssea, ext))
names(suit_vuln_mammal_ssea) <- c('x', 'y', localforestmammal_ssea)

# Subset only spp columns where it is present in our non-NA study area.... 
onlyGoodcolumns_idx  <- colSums(suit_vuln_mammal_ssea>0 , na.rm=TRUE) > 0 #DOES THE COLUMN HAVE POSITIVE VALUES? IF YES, KEEP (TRUE)
suit_vuln_mammal_ssea <- suit_vuln_mammal_ssea[, c(rep(TRUE, 2), onlyGoodcolumns_idx[3:length(onlyGoodcolumns_idx)])] #441
all(colSums( suit_vuln_mammal_ssea[3:ncol(suit_vuln_mammal_ssea)]>0 , na.rm=TRUE) > 0 ) #TRUE

rm(list_spp_presence, stack_spp_presence, ext)

head(suit_vuln_vals3)
names(suit_vuln_mammal_afr)[1:20]
suit_vuln_mammal <- suit_vuln_vals3 %>% dplyr::select(cell.id, x, y, region)
suit_vuln_mammal <- left_join(suit_vuln_mammal, suit_vuln_mammal_afr, by=c("x", "y"))
suit_vuln_mammal <- left_join(suit_vuln_mammal, suit_vuln_mammal_ssea, by=c("x", "y"))

str(suit_vuln_mammal)
all(colSums( suit_vuln_mammal[, 5:ncol(suit_vuln_mammal)]>0 , na.rm=TRUE) > 0 ) #TRUE

fwrite(suit_vuln_mammal, 'output/temp/suit_vuln_scenario_mammal.csv')


##### (D) Combined taxa ####
rm(list=setdiff(ls(), c("suit_vuln_amph", "suit_vuln_bird", "suit_vuln_mammal")))
gc()

# Load df 
suit_vuln_amph <- fread('output/temp/suit_vuln_scenario_amph.csv') 
suit_vuln_bird <- fread('output/temp/suit_vuln_scenario_bird.csv')  
suit_vuln_mammal <- fread('output/temp/suit_vuln_scenario_mammal.csv')  

head(names(suit_vuln_amph),10)
head(names(suit_vuln_bird),10)
head(names(suit_vuln_mammal),10)

# Using data.table functions to merge these big dataframes, for speed
dupcolnames <- colnames(suit_vuln_amph)[1:4]
suit_vuln_amph.dt = data.table(suit_vuln_amph, key = dupcolnames)
suit_vuln_bird.dt = data.table(suit_vuln_bird, key = dupcolnames)
suit_vuln_mammal.dt = data.table(suit_vuln_mammal, key = dupcolnames)

#quick check
setdiff(suit_vuln_amph.dt[ ,1:4], suit_vuln_bird.dt[ ,1:4])
setdiff(suit_vuln_bird.dt[ ,1:4], suit_vuln_amph.dt[ ,1:4])
setdiff(suit_vuln_amph.dt[ ,1:4], suit_vuln_mammal.dt[ ,1:4])

#this is going to be vals_spp
system.time(suit_vuln_spp <- Reduce(function(...) merge(..., all = TRUE), list(suit_vuln_amph.dt,suit_vuln_bird.dt,suit_vuln_mammal.dt)) ) #5-8s

#check if cells got duplicated during join/merge
suit_vuln_spp[duplicated(suit_vuln_spp$cell.id), ]$cell.id #zero
# ncol(suit_vuln_spp)==(437+1704+325-4-4) #simple check of col numbers

suit_vuln_spp <- suit_vuln_spp[order(cell.id, x, y, region)] #nspp = 4310-4
str(suit_vuln_spp)
fwrite(suit_vuln_spp, 'output/temp/suit_vuln_scenario_combspp_incldsppwithnoforestcells.csv')


##### (E) Exclude spp with zero forested cells in their range ####
# Make spp matrix
#suit_vuln_spp <- fread('output/temp/suit_vuln_scenario_combspp_incldsppwithnoforestcells.csv') #few seconds
scen_spp <- suit_vuln_spp[, c("cell.id","x", "y", "region"):=NULL] #4306 spp -> 4305spp
head(colnames(scen_spp))
scen_spp <- data.matrix(scen_spp) #6.1Gb if replace NAs with 0s/3.8Gb if not

rm(list=setdiff(ls(), c("scen_spp")))
gc() # takes up too much RAM if don't remove suit_vuln_spp


# Load suit_vuln_vals3
suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv') 
suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]
str(suit_vuln_vals3)


# Sum of cells where spp occurs & where cell is forested
spp_ranges = colSums(suit_vuln_vals3$forest>0 & scen_spp, na.rm=TRUE) 


# Select only species with at least one cell in forested
sppnoforestedrange = which(spp_ranges==0) # 41 spp
sppnoforestedrange.names = dput(names(sppnoforestedrange))
# c("Arthroleptis tanneri", "Geotrypetes pseudoangeli", "Gephyromantis eiselti", 
# "Gephyromantis thelenae", "Mantidactylus zolitschka", "Mertensophryne howelli", 
# "Werneria tandyi", "Adenomus dasi", "Ansonia siamensis", "Cophixalus cryptotympanum", 
# "Echinotriton chinhaiensis", "Fejervarya greenii", "Ichthyophis paucidentulus", 
# "Leptobrachella pluvialis", "Litoria becki", "Nyctimystes persimilis", 
# "Platymantis hazelae", "Platymantis negrosensis", "Pseudophilautus cavirostris", 
# "Pseudophilautus fulvus", "Pseudophilautus sarasinorum", "Pseudophilautus stictomerus", 
# "Raorchestes griet", "Uperodon mormorata", "Pternistis ochropectus", 
# "Sheppardia montana", "Alopecoenas xanthonurus", "Corvus kubaryi", 
# "Leucopsar rothschildi", "Zosterornis nigrorum", "Lepilemur microdon", 
# "Lepilemur randrianasoloi", "Lepilemur septentrionalis", "Microcebus bongolavensis", 
# "Prolemur simus", "Rhinolophus sakejiensis", "Crocidura negrina", 
# "Hipposideros crumeniferus", "Rhinolophus madurensis", "Sundasciurus davensis", 
# "Vandeleuria nolthenii")

sppwithforestedrange = which(spp_ranges>0) #(4264 spp with afr_CCI)
sppwithforestedrange.names = names(sppwithforestedrange)


suit_vuln_spp <- fread('output/temp/suit_vuln_scenario_combspp_incldsppwithnoforestcells.csv')
suit_vuln_spp <- suit_vuln_spp[ , ..sppwithforestedrange.names] #filter only spp columns with >0 forested cells in their range (data.table indexing)

fwrite(suit_vuln_spp, 'output/suit_vuln_scenario_combspp.csv') 

# Ok to delete files in output/temp/ now, as they are huge files


# # Make small vers of dataset for testing 
# suit_vuln_spp <- fread('output/suit_vuln_scenario_combspp.csv') 
# suit_vuln_spp_test <- suit_vuln_spp[, 1:100]
# 
# fwrite(suit_vuln_spp_test, 'output/suit_vuln_scenario_combspp_100.csv') 




###### Numbers for Predicted Rubber Expansion ######

## Projected increase in rubber area to meet predicted industry demand by 2027 (2017 baseline)
# 2027 Demand = 16.79 mil tonnes, 2017 demand = 13.22 mil tonnes
# Using simple conversion factors from EWT = Minimum and maximum yields of current plantations on mainland Southeast Asia, based on adjusted tapped area (Table S3 in EWT2015), 0.915 t ha-1 yr-1 and 1.452 t ha-1 yr-1
irsg2027_low <- (16.79-13.22)/1.452   # 2.46 Mha
irsg2027_high <- (16.79-13.22)/0.915   # 3.90 Mha


## using Warren-Thomas 2015 Projections for 2024 (2017 baseline)
# numbers from Table S8
# Additional area needed from 2010 to 2024, adjusted for intensification of existing rubber in Malaysia/Indonesia and displacement by oil palm = 3.822345-8.864975 Mha 
# This is the amount needed from 2017 baseline:

fao <- read.csv('data/FAOSTAT_yield_NR_World_2020-04-21.csv') #### FAO Rubber Harvested Area by Year 
#   “FAO. FAOSTAT. License: CC BY-NC-SA 3.0 IGO. Extracted from: http://www.fao.org/faostat/en/#data/QC. Data of Access: 21-04-2020.”

rubberArea_milha_2010 <- fao %>% dplyr::filter(Element == 'Area harvested', Year == 2010, Area=="World") %>% mutate(Value=Value/1000000) %>% dplyr::select(Value) 
rubberArea_milha_2017 <- fao %>% dplyr::filter(Element == 'Area harvested', Year == 2017, Area=="World") %>% mutate(Value=Value/1000000) %>% dplyr::select(Value) 

ewt2024_lowest <- rubberArea_milha_2010+3.822345-rubberArea_milha_2017 #1.66-6.70 mil ha
ewt2024_highest <- rubberArea_milha_2010+8.864975-rubberArea_milha_2017 #


#These numbers will be used to quantify species impacts at several stages of conversion in the scenarios




######### Example code for running scenario simulations directly in R #####
rm(list=ls())
gc()

# source('code/13.0-unrestricted_scenarios_fxn.R') # different wd, can't directly source unless you change the directories there

# Written for use in HPC, adapt as required for your HPC system

## Description of variables:
# vals_mod is a data.matrix/table/frame with 3 columns: suit, forest, and the criteria variable scenario is based on. each row represents a cell in the grid.
# vals_spp is a data.matrix/table/frame with as many columns as there are species, and each row indicates presence/absence of that species in the cell
# spp_ranges is a vector of  the original range of all spp (colsums(vals_spp), or sums of 1s)
# ranges_ = range remaining after each (forested) cell is converted
# nrows is number of cells to convert. I set the default to 700 cells = 7 Mha
# res is lost area (no. of cells converted), sum of forested range lost (1cell=100km2=10,000ha=0.01Mha), sum of % range loss, no. of spp lost any amount of range, no. of spp lost >=10% of range, average suitability of converted cells 


##### Function for converting based on a single criterion
scen_results <- function(vals_mod, vals_spp, spp_ranges, nrows=700){
  vals_spp = cbind(vals_mod, sample.int(nrow(vals_mod)), vals_spp) #cbind scenario vals, random numbe column, spp distrb
  
  vals_spp = vals_spp[order(vals_spp[, 3], vals_spp[, 4]), ] #sort by scenario values then random
  
  vals_mod = vals_spp[ , 1:2] #extract first column which is suitability, to calc avg suit in res. extract 2nd column which is forest
  
  vals_spp = vals_spp[ , 5:(ncol(vals_spp)) ] #select only columns of sppnames, rows are sorted by conversion order. 
  
  # Initialize objects for the loop
  ranges_ = spp_ranges #initial forested ranges_ before subtracting
  lost_area = 0
  res = res_0
  
  #for every cell converted (row i) in first 10K, find spp which occurs in that cell (non-NA), then minus 1 cell from that species' original range
  for (i in 1:nrows){
    spp_which_occur = which(is.na(vals_spp[i,])==FALSE & vals_spp[i,]>0 & vals_mod[i,2]==1) # & if spp occur in cell i and cell i is forested
    ranges_[spp_which_occur] = ranges_[spp_which_occur]-1 #remove 1 from the range of spp that occur in this cell(row)
    lost_area = lost_area + 1
    
    #record the info for every 10 cells
    if(i%%10 == 0){
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges <= 0.9), sum(vals_mod[1:i])/i ) )  
    }
    
    #this code is for Expected range loss under the different scenarios for all forest dependent species, at different amounts of rubber expansion (EWT-2024, Industry-2027)
    
    if(i==245){
      spp_ranges_remaining_low = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==390){
      spp_ranges_remaining_high = ranges_
      # res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    if(i==166){
      spp_ranges_remaining_low_EWT = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==670){
      spp_ranges_remaining_high_EWT = ranges_
      #  res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    #rm(spp_which_occur)
    
  }
  output <- list(res, spp_ranges_remaining_low, spp_ranges_remaining_high, spp_ranges_remaining_low_EWT, spp_ranges_remaining_high_EWT)
  return(output) #output
}





#### Function for 2 criteria to convert on
# inputs same as above but
# vals_mod is a data.matrix/table/frame with 4 columns: suit, forest, and the two criteria variables the scenario is based on. each row represents a cell in the grid.

scen_results_nvar2 <- function(vals_mod, vals_spp, spp_ranges, nrows=700){
  vals_spp = cbind(vals_mod, sample.int(nrow(vals_mod)), vals_spp) #cbind scenario vals, random numbe column, spp distrb
  
  vals_spp = vals_spp[order(vals_spp[, 3], vals_spp[, 4],  vals_spp[, 5]), ] #sort by scenario values then random
  
  vals_mod = vals_spp[ , 1:2] #extract first column which is suitability, to calc avg suit in res. extract 2nd column which is forest
  
  vals_spp = vals_spp[ , 6:(ncol(vals_spp)) ] #select only columns of sppnames, rows are sorted by conversion order. 
  
  # Initialize objects for the loop
  ranges_ = spp_ranges #initial forested ranges_ before subtracting
  lost_area = 0
  res = res_0
  
  #for every cell converted (row i) in first 10K, find spp which occurs in that cell (non-NA), then minus 1 cell from that species' original range
  for (i in 1:nrows){
    
    spp_which_occur = which(is.na(vals_spp[i,])==FALSE & vals_spp[i,]>0 & vals_mod[i,2]==1) # & if spp occur in cell i and cell i is forested
    ranges_[spp_which_occur] = ranges_[spp_which_occur]-1 #remove 1 from the range of spp that occur in this cell(row)
    lost_area = lost_area + 1
    
    #record the info for every 10 cells
    if(i%%10 == 0){
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) )
    }
    
    #this code is for Expected range loss under the different scenarios for all forest dependent species, at different amounts of rubber expansion (EWT-2024, Industry-2027)
    
    if(i==245){
      spp_ranges_remaining_low = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==390){
      spp_ranges_remaining_high = ranges_
      # res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    if(i==166){
      spp_ranges_remaining_low_EWT = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==670){
      spp_ranges_remaining_high_EWT = ranges_
      #  res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    #rm(spp_which_occur)
    
  }
  output <- list(res, spp_ranges_remaining_low, spp_ranges_remaining_high, spp_ranges_remaining_low_EWT, spp_ranges_remaining_high_EWT)
  return(output) #output
}




###### This section is for loading libraries/data/parameters used in all scenarios simulations ##### 

#### Load libraries ####
library(dplyr)
library(data.table)
library(parallel)

#### Load data ####

# Spp matrix
scen_spp <- fread('output/suit_vuln_scenario_combspp.csv') #on PC
scen_spp <- data.matrix(scen_spp) #6.1Gb if replace NAs with 0s/3Gb if not

# suit vuln vals
suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv') #on PC
suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]

# spp_ranges, res_0
spp_ranges = colSums(suit_vuln_vals3$forest>0 & scen_spp, na.rm=TRUE)
res_0 = matrix(c(0,0,0,0,0,0), nrow=1)


####### Scenario 1 (example for 1 rep) #####

#### Extract needed columns for simulation 
scen_in <- suit_vuln_vals3[ , .(suit, forest, avg.suit.acc)]
scen_in <- data.matrix(scen_in) #2.9mb as matrix


##### + Start conversion simulation #####
system.time(scen_suit_rep <- scen_results(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges) ) #10s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/scen_df1_test.csv') #country simulation results

spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/spprangelosstbl_scen1_test.csv')


####### Scenario 2 (example for 1 rep) #####

#### Extract needed columns for simulation 
scen_in <- suit_vuln_vals3[ , .(suit, forest, avg.vulnT.carb)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
system.time(scen_suit_rep <- scen_results(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges) ) 

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/scen_df2_test.csv') #country simulation results

spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/spprangelosstbl_scen2_test.csv')





##### Scenario 3a (example for sorting on 2 criteria; 1 rep) #######

#### Extract needed columns for simulation 
scen_in <- suit_vuln_vals3[ , .(suit, forest, comprom.vulnT.1, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
system.time(scen_suit_rep <- scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)) 

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/scen_df3a_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/spprangelosstbl_scen3a_test.csv')



##### Scenario 3b (example for sorting on 2 criteria; 1 rep) #######

#### Extract needed columns for simulation 
scen_in <- suit_vuln_vals3[ , .(suit, forest, comprom.vulnT.2, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
system.time(scen_suit_rep <- scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)) 

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/scen_df3b_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/spprangelosstbl_scen3b_test.csv')



##### Scenario 3c (example for sorting on 2 criteria; 1 rep) #######

#### Extract needed columns for simulation 
scen_in <- suit_vuln_vals3[ , .(suit, forest, comprom.vulnT.3, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
system.time(scen_suit_rep <- scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges))

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/scen_df3c_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/spprangelosstbl_scen3c_test.csv')

