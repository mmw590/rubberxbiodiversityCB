# CLEAN CODE FOR OPEN ACCESS #
# >>>

# 21.1a-afr_rasters.R #### 
rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(sf)
library(fasterize)
library(lwgeom)
library(ggplot2)
library(cowplot)



# Set folder containing GIS input files
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'


# inputs required ####
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')
read.csv('data/listofafricancountries.csv')
input <- raster(paste0(GISfolder, 'LandCover CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'))
input <- raster(paste0(GISfolder, 'Rubber Suitability/RubberSuit.tif'))
input <- raster(paste0(GISfolder, 'HCS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif'))
input <- raster(paste0(GISfolder, 'accessibility/accessibility_to_cities_2015_v1.0/accessibility_to_cities_2015_v1.0.tif'))
input <- raster(paste0(GISfolder, "WDPA/WDPA_July2018/WDPA_July2018_combraster.tif")) #5.3GB, took me 761s=12m OR 335s=6mins

system.time(layers <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW.gdb"))) #takes a few mins
iucn_ass_tax <- read.csv('data/HBW-BirdLife_Checklist_v3_Nov18/HBW-BirdLife_List_of_Birds_v3.csv')
birdsnotinredlist <- read.csv('data/redlist/birdsnotinredlist_repaired.csv')




# Create output folders ####
if (dir.exists(file.path('.', 'output'))) {
  cat("folder already exists")
} else{ (dir.create(file.path('.', 'output'))) }

if (!dir.exists(file.path('output/', 'temp'))) {
  (dir.create(file.path('output/', 'temp'))) }

if (!dir.exists(file.path('output', 'afr'))) {
  (dir.create(file.path('output', 'afr'))) }

if (!dir.exists(file.path('output/afr', 'biodiversity_rasters_cci'))) {
  (dir.create(file.path('output/afr', 'biodiversity_rasters_cci'))) }

if (!dir.exists(file.path('output/afr', 'various_rasters'))) {
  (dir.create(file.path('output/afr', 'various_rasters'))) }

if (!dir.exists(file.path('output/results', 'results_afr'))) {
  (dir.create(file.path('output/results', 'results_afr'))) }

if (!dir.exists(file.path('output/dfs_for_figs', 'afr'))) {
  (dir.create(file.path('output/dfs_for_figs', 'afr'))) }




#### Determining Extent and Projection -----
# One method to calculate the standard parallels is by determining the range in latitude in degrees north to south and dividing this range by six. The "one-sixth rule" places the first standard parallel at one-sixth the range above the southern boundary and the second standard parallel minus one-sixth the range below the northern limit. There are other possible approaches.

# extents in WGS 
xmin = -25.5 #W
xmax =  63.5 #E
ymin = -35 #S 
ymax =  19 #N

refExtent_wgs <- extent(c(xmin,xmax,ymin,ymax))

standard_parallel_1 = ymin+((ymax-ymin)/6) #-26
standard_parallel_2 = ymax-((ymax-ymin)/6) #10
latitude_of_center = (xmin+xmax)/2 #19
longitude_of_center = (ymin+ymax)/2 #-8

refProj_aea <- paste0("+proj=aea +lat_1=", standard_parallel_1, " +lat_2=", standard_parallel_2, " +lat_0=", latitude_of_center, " +lon_0=", longitude_of_center, " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")



### + Use Afr polygon to transform to aea, and get refExtent_aea ####
listofafricancountries <- read.csv('data/listofafricancountries.csv')
listofafricancountries <- listofafricancountries$x

gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')
gadm_afr <- gadm_0 %>% dplyr::filter(GID_0 %in% listofafricancountries) #0.11s
st_crs(gadm_0)
afr_wgs <- st_crop(gadm_afr, refExtent_wgs)

unique(afr_wgs$NAME_0)
length(unique(afr_wgs$GID_0)) #only 54 (Egypt, Western Sahara, Morocco, Libya, Tunisia excluded)

afr_aea <- st_transform(afr_wgs, crs=as.character(refProj_aea))
# plot(st_geometry(afr_aea))

# st_write(afr_countries, 'output/afr/various_rasters/afr_countries_aea.gpkg') 
# afr_countries <- st_read('output/afr/various_rasters/afr_countries_aea.gpkg')



#### + Make raster (ref grid) ----

# Create a blank raster layer with desired proj (aea), extent, resolution
target_ds <- raster(crs=refProj_aea, ext=extent(afr_aea), res=10000, vals=0) #res is pixel size, vals is no data value
# dim = 612 x 918 = 561816 cells [excld Middle East]
# extent     : -1933681, 7246319, -6120582, -582.2639  (xmin, xmax, ymin, ymax)   

target_ds2 <- fasterize(st_collection_extract(afr_aea, "POLYGON"), target_ds, background=0)
plot(target_ds2)

writeRaster(target_ds2, filename = 'output/afr/various_rasters/africa.tif', datatype='INT1U', overwrite=TRUE, options=c("COMPRESS=LZW")) #create base grid




# PROJECTING & RESAMPLING RASTERS TO MATCH REF GRID ----
reference <- raster('output/afr/various_rasters/africa.tif') #Path to reference file
referenceProj = as.character(crs(reference)) #aea
referenceExt = extent(reference)
referenceRes = res(reference)

# define output raster
output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)

cropExtent_wgs <- extent(c(-50,90,-40,40)) #for pre-cropping global rasters to speed up projectRaster


# +LandUse (CCI) ####
input <- raster(paste0(GISfolder, 'LandCover CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'))
system.time(input <- crop(input, cropExtent_wgs) ) #126s
system.time(output.lu <- projectRaster(input, output, method='ngb', filename='output/afr/various_rasters/land_use_africa_CCI.tif', datatype='INT1U', overwrite=TRUE)) #10s aft crop; 228s without crop. Cropping saves ~88s
plot(output.lu)

output.lu
#dimensions : 612, 918, 561816  (nrow, ncol, ncell)
#extent     : -1933681, 7246319, -6120582, -582.2639  (xmin, xmax, ymin, ymax)


# +Suitability ####
input <- raster(paste0(GISfolder, 'Rubber Suitability/RubberSuit.tif'))
system.time(input <- crop(input, cropExtent_wgs) ) #14s
system.time(output.suit <- projectRaster(input, output, method='bilinear', filename='output/afr/various_rasters/suitability_afr.tif', overwrite=TRUE)) #7s


# +Carbon ####
input <- raster(paste0(GISfolder, 'HCS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif'))
input  #0-633
plot(input) #need to rescale to 0-1

#http://www.timassal.com/?p=859
rasterRescale<-function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

system.time(input.rsc <- rasterRescale(input)) #2 mins
plot(input.rsc)

system.time(output.carb <- projectRaster(input.rsc, output, method='bilinear', filename='output/afr/various_rasters/carbon_afr.tif', overwrite=TRUE)) #15s

#with precropping
system.time(input.rsc2 <- crop(input.rsc, cropExtent_wgs) ) #15s
system.time(output.carb2 <- projectRaster(input.rsc2, output, method='bilinear', filename='output/afr/various_rasters/carbon_afr_cropwgs.tif')) #7s

all.equal(output.carb, output.carb2, tolerance=0.5) #FALSE

plot(output.carb)
hist(output.carb)
plot(output.carb2)



# +Accessibility ####
input <- raster(paste0(GISfolder, 'accessibility/accessibility_to_cities_2015_v1.0/accessibility_to_cities_2015_v1.0.tif'))
plot(input)
range(values(input)) #-9999, 41556 - although metadata said the min value should be 0! remove all cells =-9999. Not sure what to do about cells > -9999 and < 0.

# Replace non zero values with NA
input[input <0] <- NA #this takes a few mins
plot(input)

system.time(input.rsc <- rasterRescale(input)) #266.62s
plot(input.rsc)
range(values(input.rsc), na.rm=TRUE) 

input.rsc2 <- crop(input.rsc, cropExtent_wgs) 
hist(input.rsc2)
range(values(input.rsc2), na.rm=TRUE) 

system.time(output.acc <- projectRaster(input.rsc2, output, method='bilinear', filename='output/afr/various_rasters/accessibility_afr.tif', overwrite=TRUE)) #5.32s after crop

plot(output.acc)
hist(output.acc)

rm(output.lu, output.suit, output.opsuit, output.carb, output.acc)



##### +Protected Areas ----
input <- raster(paste0(GISfolder, "WDPA/WDPA_July2018/WDPA_July2018_combraster.tif")) #5.3GB, took me 761s=12m OR 335s=6mins

plot(input)

# Reclassify to binary
reclassify.df <- data.frame(oldval=c(0,1,2), newval=c(0,1,1)) %>% arrange(oldval)
system.time(input <- subs(input, reclassify.df, by='oldval', which='newval',subsWithNA=FALSE))

## Project to aea and resample to ref grid
system.time(output.pa <- projectRaster(input, output, method='ngb', filename='output/afr/various_rasters/protected_areas_afr.tif', datatype='INT1U', overwrite=TRUE)) #375s
output.pa
plot(output.pa)

#rm objects to clear up memory
rm(output.pa, input, reclassify.df)





##### +Country  ####   
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')
gadm_afr_cropExtentwgs <- st_crop(gadm_0, cropExtent_wgs)
system.time(gadm_afr_cropExtentaea <- st_transform(gadm_afr_cropExtentwgs, crs=referenceProj)) #1.5s for cropped

plot(gadm_afr_cropExtentwgs["GID_0"])
plot(gadm_afr_cropExtentaea["GID_0"])

gadm_afr_cropExtentaea$countryID <- 1:nrow(gadm_afr_cropExtentaea) #102 countries

output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)

afr_countries_rst <- fasterize(st_collection_extract(gadm_afr_cropExtentaea, "POLYGON"), output, field="countryID", background=NA)
afr_countries_rst 
plot(afr_countries_rst)
unique(values(afr_countries_rst)) #58 countries and NA
writeRaster(afr_countries_rst, filename = 'output/afr/various_rasters/afr_countryID.tif', datatype='INT1U', overwrite=TRUE, options=c("COMPRESS=LZW")) #create raster with countryID

# Save countryID lookup table 
gadm_aea_df <- st_drop_geometry(gadm_afr_cropExtentaea)
afr_countries_df <- gadm_aea_df %>% filter(countryID %in% unique(values(afr_countries_rst))) #58 countries
write.csv(afr_countries_df, "output/afr/various_rasters/afr_countryID.csv", row.names=FALSE)


# ## Checking with previously made tbl
# afr_countries_df0 <- read.csv("output/afr/various_rasters/afr_countryID_onlycontinent.csv")
# anti_join(afr_countries_df0, afr_countries_df, by=c("GID_0", "NAME_0")) #French Southern Territories (at -48degN, beyond our ymin of -35; Egypt, Western Sahara, Morocco, Tunisia - all >19deg N


# ## without pre-cropping (don't use bc plot looks strange and somehow Russia is incld?)
# system.time(gadm_aea <- st_transform(gadm_0, crs=referenceProj)) #9s
# gadm_aea$countryID <- 1:nrow(gadm_aea) #256
# 
# afr_countries_rst2 <- fasterize(st_collection_extract(gadm_aea, "POLYGON"), output, field="countryID", background=NA)
# afr_countries_rst2 
# plot(afr_countries_rst2) #weird band through west Afr
# unique(values(afr_countries_rst2)) #just one extra country
# writeRaster(afr_countries_rst2, filename = 'output/afr/various_rasters/africa_countryID_no-precrop.tif', datatype='INT2U', overwrite=TRUE, options=c("COMPRESS=LZW")) #INT2U because >255
# 
# gadm_aea_df <- st_drop_geometry(gadm_aea)
# afr_countries_df2 <- gadm_aea_df %>% filter(countryID %in% unique(values(afr_countries_rst2))) #the one extra country is Russia...uhh
# write.csv(afr_countries_df2, "output/afr/various_rasters/afr_countryID_no-precrop.csv", row.names=FALSE)
# 


rm(afr_countries_df, target_ds, target_ds2, target_ds3, standard_parallel_1, standard_parallel_2, latitude_of_center, longitude_of_center, xmax, xmin, ymax, ymin)
gc()



### +Oil Palm Suitability (not used in paper) ####
input <- raster(paste0(GISfolder, 'Strona et al. 2018/INPUT_DATA/suitability_rasters/rainfed_intermediate_continuous/data.asc'))

# OP concessions
op_conc <- st_read(paste0(GISfolder, 'Global Forest Watch Oil Palm/Oil_palm_concessions_select_countries.shp'))
#plot(st_geometry(op_conc))
str(op_conc)
op_conc_raster <- fasterize(op_conc, input, background=0)
#plot(op_conc_raster)

input <- mask(input, op_conc_raster, maskvalue=1, updatevalue=NA)

system.time(output.opsuit <- projectRaster(input, output, method='bilinear', filename='output/afr/various_rasters/op_suitability_afr.tif', overwrite=TRUE)) #0.58s. datatype="FLT4S", because decimals, continuous scale

plot(output.opsuit)

rm(op_conc, op_conc_raster)




####### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
####### £££££££  MASKING OUT LU ETC FROM RASTERS  £££££££££££ ####
####### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))



####### +Load LU mask (CCI) ########
mat_land_use_cci <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')

reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)
plot(mat_land_use)


rm(reclassify.df, mat_land_use_cci)


### +Load PAs ####
mat_pa <- raster('output/afr/various_rasters/protected_areas_afr.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use, maskvalue=0, updatevalue=NA)
plot(mat_pa)
plot(mat_pa2)

#transform to wgs for plotting 
refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

mat_pa2_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_pa)[1], ncols=dim(mat_pa)[2])
mat_pa2_latlon <- projectRaster(mat_pa2, mat_pa2_latlon, crs=crs_wgs, method='ngb')

plot(mat_pa2_latlon)

# transform to df for plotting
pa_spdf <- rasterToPoints(mat_pa2_latlon)
pa_df <- as.data.frame(pa_spdf)
head(pa_df)

write.csv(pa_df, 'output/dfs_for_figs/afr_cci/pa_df_LU_cci.csv', row.names=FALSE)

rm(mat_pa, mat_pa2_latlon, pa_spdf, pa_df)


### +Load rubber concessions ####
# no raster data for Africa, can't filter those out






# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (1) MAKE BIRD RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), "GISfolder"))

system.time(layers <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW.gdb"))) #takes a few mins

# Preprocessing: Match bird names to IUCN Red List Categories
# Load complete iucn red list of birds and taxonomic names
iucn_ass_tax <- read.csv('data/HBW-BirdLife_Checklist_v3_Nov18/HBW-BirdLife_List_of_Birds_v3.csv')
str(iucn_ass_tax)
iucn_ass_tax <- iucn_ass_tax %>% dplyr::select(SCINAME = Scientific.name, redlistCategory = X2018.IUCN.Red.List.category) %>% distinct()


#### Pre-processing code to find bird spp that were not in the red list dataset, only need to do once
# layers2 <- layers %>% left_join(iucn_ass_tax)
# 
# unique(layers2$className) #only aves thank goodness
# unique(layers2$redlistCategory) #theres NA
# 
# check <- layers2[is.na(layers2$redlistCategory)==TRUE, ] #61 layers no redlist Category
# check_df <- check %>% st_set_geometry(NULL) %>% dplyr::select(SCINAME, redlistCategory, genusName, speciesName)
# check_df <- check_df %>% tidyr::separate(SCINAME, into=c('genusName','speciesName'), sep=" ", remove=FALSE) %>% distinct() #only 23 spp
# 
# write.csv(check_df, 'OUTPUT/check_birdsnotinredlist.csv', row.names=FALSE)


# Skipped this section because not necessary, also takes a long time to fix geometry (30 hours)
# # Crop to Africa #in Strona, they used the africa shapefile from continents....
# continent <- st_read('data/continents/continent.shp')
# africa <- continent[continent$CONTINENT == 'Africa', ]
# 
# identical(st_crs(africa), st_crs(layers) )
# 
# system.time(layers_africa <- st_intersection(layers, africa)) #although coordinates are longitude/latitude, st_intersection assumes that they are planar #11:54 am-
# 
# # although coordinates are longitude/latitude, st_intersection assumes that they are planar
# # Error in CPL_geos_op2(op, st_geometry(x), st_geometry(y)) : 
# #   Evaluation error: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -124.42620849599984 49.775878906000116 at -124.42620849599984 49.775878906000116.
# 
# # Fix Invalid Geometry
# #https://www.r-spatial.org/r/2017/03/19/invalid.html
# #system.time(sf::st_is_valid(layers, NA_on_exception = FALSE, reason = TRUE)) #3977.31s
# system.time(layers5 <- lwgeom::st_make_valid(layers4)) #92951.94s, 30 hours
# 
# system.time(sf::st_is_valid(layers5, NA_on_exception = FALSE, reason = TRUE)) #9230.45s nothing happened so i guess it's valid now
# 
# #rm(layers3)
# system.time(st_write(layers5, dsn=paste0(GISfolder, "Biodiversity Maps/BOTW_geomfixed"), layer="botw_geomfixed.shp"))
# 

# # Reference file
# reference <- raster('output/afr/various_rasters/africa.tif') #Path to reference file
# referenceProj = as.character(crs(reference)) #aea
# referenceExt = extent(reference)
# referenceRes = res(reference)
# rm(reference)

rm(list=setdiff(ls(), "GISfolder"))
gc()

system.time(layers <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW.gdb"))) #takes a few mins
names(layers)

#iucn_ass_tax <- read.csv('data/redlist/redlist_birds_all.csv')
#str(iucn_ass_tax)

# Load complete iucn red list of birds and taxonomic names
iucn_ass_tax <- read.csv('data/HBW-BirdLife_Checklist_v3_Nov18/HBW-BirdLife_List_of_Birds_v3.csv')
str(iucn_ass_tax)
iucn_ass_tax <- iucn_ass_tax %>% dplyr::select(SCINAME = Scientific.name, redlistCategory = X2018.IUCN.Red.List.category) 

# Join to IUCN Red List
layers2 <- layers %>% left_join(iucn_ass_tax)
check <- layers2[is.na(layers2$redlistCategory)==TRUE, ] %>% st_set_geometry(NULL) 
unique(check$SCINAME) #23 spp in BirdLife layers, not in Checklist


#### 'Corrected' BirdLife spp list, matched to their synonyms used in the HBW, so they could be matched to their redlistCategory 
birdsnotinredlist <- read.csv('data/redlist/birdsnotinredlist_repaired.csv')
birdsnotinredlist <- left_join(birdsnotinredlist, iucn_ass_tax) %>% dplyr::select(SCINAME.old, SCINAME, redlistCategory)

# Replace BirdLife old spp names with their RedList synonyms
layers_ori <- layers
layers$SCINAME2 <- birdsnotinredlist$SCINAME[match(unlist(layers$SCINAME), birdsnotinredlist$SCINAME.old)]
layers$SCINAME <- as.character(layers$SCINAME)
layers$SCINAME2 <- as.character(layers$SCINAME2)
layers[is.na(layers$SCINAME2)==FALSE, ]$SCINAME <- layers[is.na(layers$SCINAME2)==FALSE, ]$SCINAME2

# Join to IUCN Red List
layers <- layers %>% left_join(iucn_ass_tax)

# Checking
check <- layers[is.na(layers$redlistCategory)==TRUE, ] %>% st_set_geometry(NULL) %>% dplyr::select(SCINAME, SCINAME2, redlistCategory)
check2 <- layers[is.na(layers$SCINAME2)==FALSE, ] %>% st_set_geometry(NULL) %>% dplyr::select(SCINAME, SCINAME2, redlistCategory)
#both good! no more NA redlistCategory!


#Select only relevant columns
layers2 <- layers %>% dplyr::select(SISID:DATE_, PRESENCE:SEASONAL, VERSION:Shape_Area, redlistCategory)

# Exclude extinct spp
layers2 <- layers2 %>% dplyr::filter(redlistCategory != 'EX') %>% dplyr::filter(redlistCategory != 'EW')

table(layers2$redlistCategory)
#_ DD birds in Afr
#see where are they? Are they in PNG?


# Calculating average threat score to assign to DD
iucn_DD_sc <- iucn_ass_tax %>% dplyr::filter(redlistCategory != 'EX', redlistCategory != 'EW', redlistCategory != 'DD') %>%
  mutate(redlistCategory = recode(redlistCategory, 'CR (PE)'='CR', 'CR (PEW)'='CR') ) %>%
  mutate(iucn_sc = recode(redlistCategory, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) )

table(iucn_DD_sc$redlistCategory, iucn_DD_sc$iucn_sc)

iucn_DD_sc_pct <- iucn_DD_sc %>% group_by(redlistCategory) %>% summarize(percentage=n()/nrow(iucn_ass_tax)*100)

iucn_DD_sc_pct

#However, while data-deficient birds are predicted to be no more threatened than assessed species (Butchart and Bird, 2010), 23% of bird species globally are listed as NT to CR.
2.05+4.30+9.28+7.32

#This puts the average threat score at 3.84, which we rounded up to 4 and used for DD birds. 
mean(iucn_DD_sc$iucn_sc) #3.842882 for LC=2; 2.301953 for LC=0


# Reclassify threat status into values
layers2 <- layers2 %>% dplyr::mutate(iucn_sc = recode(redlistCategory, 'DD' = 4, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, 'CR (PE)'=32, 'CR (PEW)'=32, .default = 0) ) 

# presence=1 for spp richness
layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

#check
table(layers2$category, layers2$iucn_sc)

#save st object as gpkg
rm(layers, layers_ori, birdsnotinredlist, iucn_ass_tax, check, check2)
gc()



#### Save processed BOTW wgs map (for later use in SSEA) ####
system.time(st_write(layers2, dsn = paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_wgs.gpkg"), layer_options =c("OVERWRITE=yes") ) ) #642s or 11mins, 425.6s for overwrite

#system.time(layers2 <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_wgs.gpkg")) ) #109s to load



#### Project shapefile from wgs to aea ####
reference <- raster('output/afr/various_rasters/africa.tif')
referenceProj = as.character(crs(reference)) #cea
referenceExt = extent(reference)
referenceRes = res(reference)

system.time(layers_aea <- st_transform(layers2, crs=as.character(referenceProj)) ) #528s or 8mins, 892s or 15mins.

system.time(st_write(layers_aea, dsn=paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_aea_afr.gpkg"), layer_options =c("OVERWRITE=YES"))) #619s 



####### <---- START HERE - ONCE YOU HAVE SAVED THE BIRD LAYER IN AEA, CAN SKIP ABOVE SECTION ~~~~ ##########
reference <- raster('output/afr/various_rasters/africa.tif')
referenceProj = as.character(crs(reference)) #cea
referenceExt = extent(reference)
referenceRes = res(reference)

#### Load processed bird layers (aea projection) ####
system.time(layers_aea <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_aea_afr.gpkg")) ) #109s to load

layers_aea <- layers_aea %>% dplyr::select(SISID:DATE_, PRESENCE:SEASONAL, VERSION:presence_richness) %>% rename(binomial = SCINAME, category = redlistCategory)
str(layers_aea)

### For each species, rasterize the polygon range shapes, and assign raster values=IUCN threat values


#### (a) Bird richness tif ####
# Get list of bird species
spp <- sort(unique(layers_aea2$binomial))
#spp_df <- read.csv('data/birds_spp_in_afr.csv') #after i've run the loop once, i'll have a shortened list of all birds that occur in Africa so i can just use this to speed things up. 
#spp <- as.character(spp_df$x)

#layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)

### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0


system.time(
  for(i in 1:length(spp)){
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)

      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in Africa.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#1717s=29min

plot(m0) #values from 0 to 700, need higher byte, use INT2U
writeRaster(m0, filename='output/afr/biodiversity_rasters_cci/m0_afr_bird_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


#check species whose ranges are in africa
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 

sppinafr <- unique(setdiff(spp, sppnotinafr2)) 
length(spp)-length(sppinafr)


write.csv(sppinafr, 'data/birds_spp_in_afr.csv', row.names=FALSE)
write.csv(sppnotinafr2, 'data/birds_spp_not_in_afr.csv', row.names=FALSE)



#### (b) Bird vulnerability, tif ####
# Load predicted DD status
dd_pred_bird <- read.csv('data/DD_bird_predictedstatus_clean.csv') %>% dplyr::select(binomial =SCINAME, iucn_sc_pred)
str(dd_pred_bird)
table(dd_pred_bird$iucn_sc_pred)
# iucn_sc_pred 0  3  4 16 
# count        3 21  8  4 

layers_aea2 <- left_join(layers_aea, dd_pred_bird)
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category) 
table(layers_aea2$iucn_sc, layers_aea2$iucn_sc_pred) 

# Checking if DD pred were scored correctly
layers_df <- layers_aea2
st_geometry(layers_df) <- NULL
check <- layers_df %>% distinct(binomial, .keep_all=TRUE)
table(check$iucn_sc, check$category) 
#32 nonthreatened, 8=near threatened, 25=score 12, 4=score16. (all spp)
#nonthreatened(score 3)=, 3=near threatened, 1=score 12, 1=score16. (afr spp)
# iucn_sc (DD) 0  3   4 12  16
# count        3  21  8 20  4 (all spp)
# count        2  10  3 1   1 (afr)

rm(layers_df, check)


# Get list of bird species
#spp <- as.character(sort(unique(layers_aea2$binomial)))
spp_df <- read.csv('data/birds_spp_in_afr.csv')  #made in (a)
spp <- spp_df$x

#layers_aea2 <- layers_aea
str(layers_aea2)

layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

i=1
system.time(
  for(i in 1:length(spp)){
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0) #not just threatened spp
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)

      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not in afr.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
) #1618s

plot(m1) #values from 0 to 700, need higher byte, use INT2U
writeRaster(m1, filename='output/afr/biodiversity_rasters_cci/m0_afr_bird_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

m1b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_bird_vuln_all_DDpred.tif')

setdiff(m1, m1b)
all.equal(m1, m1b) 

#check, should be blank
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #2 spp "Cisticola melanurus"  "Cisticola restrictus"


#### (c) Bird vulnerability, threatened spp only, tif ####
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_threat, layers_aea2$category)

# Get list of bird species
spp_df <- read.csv('data/birds_spp_in_afr.csv') 
# spp_df <- read.csv('data/birds_spp_in_afr_threat.csv') #after i've run the loop once, i'll have a shortened list of all birds that occur in Africa so i can just use this to speed things up. 
spp <- as.character(spp_df$x)


#layers_aea2 <- layers_aea
str(layers_aea2)
layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0


system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #not just threatened spp
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not threatened.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#185s

plot(m2) #values from 0 to 700, need higher byte, use INT2U
writeRaster(m2, filename='output/afr/biodiversity_rasters_cci/m0_afr_bird_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

m2b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_bird_vuln_threat_DDpred.tif')
all.equal(m2, m2b, tolerance=0) #TRUE
plot(m2b)
rm(m2b)

#check species whose ranges are in africa
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #2261

sppinafr_threat <- unique(setdiff(spp, sppnotinafr2)) #284

write.csv(sppinafr_threat, 'data/birds_spp_in_afr_threat.csv', row.names=FALSE) #284


#### +Save as png (quick plots for checking) #####
mat_rich <- raster('output/afr/biodiversity_rasters_cci/m0_afr_bird_richness_DDpred.tif')
png('output/results/results_afr/m0_afr_bird_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_rich)
dev.off()

mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_bird_vuln_all_DDpred.tif')
png('output/results/results_afr/m0_afr_bird_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_bird_vuln_threat_DDpred.tif')
png('output/results/results_afr/m0_afr_bird_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (2) MAKE AMPHIBIAN RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))

layers <- st_read(paste0(GISfolder, "Biodiversity Maps/AMPHIBIANS/AMPHIBIANS.shp")) #takes 5.4S
str(layers) #awesome, there's category = redlist category

#### * Cross-check shpfile df with IUCN 2019 assessment ####
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_amph_all/assessments.csv')
str(iucn_ass_tax)

iucn_ass_tax <- iucn_ass_tax %>% 
  dplyr::mutate(redlistCategory2019 = recode(redlistCategory, "Data Deficient"='DD', "Least Concern"='LC', "Near Threatened"='NT', "Vulnerable"='VU', "Endangered"='EN', "Critically Endangered"='CR', "Extinct"='EX', "Extinct in the Wild"='EW') ) %>% 
  dplyr::select(binomial=scientificName, redlistCategory2019, yearPublished) %>%
  distinct(binomial, .keep_all=TRUE)
#6756 spp incld extinct


# Get df from layers
layers_df <- layers
st_geometry(layers_df) <- NULL

layers_df2 <- layers_df %>% dplyr::select(binomial, category, year_) %>% distinct(binomial, .keep_all=TRUE) #6631 amph spp in shpfile

check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$category)) #182 are in IUCN2019 but not in shpfile
check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$redlistCategory2019)) # 57 names are in shpfile but not in IUCN2019 list
write.csv(check, 'output/check_amph_iucn_toresolve.csv')  

# Manually look up and got the updated 2019 spp names
update.iucnnames <- read.csv('output/check_amph_iucn_corrected.csv') %>% dplyr::select(binomial, SCINAME_IUCN2019) %>% filter(is.na(.$SCINAME_IUCN2019)==FALSE) #55 changes
#2 spp that are not in IUCN 2019, so keep as is in shapefile
#Hypsiboas andinus
#Lyciasalamandra billae ssp. billae

# Correcting spp names in shpfile to IUCN 2019
layers2 <- left_join(layers, update.iucnnames)

layers2$binomial <- as.character(layers2$binomial)
layers2$SCINAME_IUCN2019 <- as.character(layers2$SCINAME_IUCN2019)
layers2 <- layers2 %>% mutate(binomial = ifelse(is.na(.$SCINAME_IUCN2019)==FALSE, SCINAME_IUCN2019, binomial))
#layers2[is.na(layers2$SCINAME_IUCN2019)==FALSE, ]$binomial <- layers2[is.na(layers2$SCINAME_IUCN2019)==FALSE, ]$SCINAME_IUCN2019

# Corrected spp names, checking
layers_df <- layers2
st_geometry(layers_df) <- NULL

layers_df2 <- layers_df %>% dplyr::select(binomial, category, year_) %>% distinct(binomial, .keep_all=TRUE) #6619 amph spp in shpfile

check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$category)) #139 are in IUCN2019 but not in shpfile
check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$redlistCategory2019)) #correct, only 2

# Checking the 139 are in IUCN2019 but not in shpfile - they are mostly reclassified, splits, uncertain taxonomy, so can safely ignore. We don't have shapefiles for those. The only one that could be linked to shpfile is Telmatobius rubigo (prev known as Telmatobius marmoratus), but their redlist category is the same, so don't need to change.
iucn_tax_ori <- read.csv('data/redlist/redlist_species_data_amph_all/taxonomy.csv') %>% dplyr::rename(binomial = scientificName)
check <- left_join(dplyr::select(check, binomial), iucn_tax_ori)

write.csv(check, 'output/check_amph_iucn_toresolve2_tax.csv') 

# Identifying spp whose red list categories needs updating
update.redlist <- full_join(iucn_ass_tax, layers2, by='binomial')  %>% filter(is.na(redlistCategory2019)) #2 NAs
update.redlist <- full_join(iucn_ass_tax, layers2, by='binomial') %>% filter(redlistCategory2019 != category) %>% distinct(binomial, .keep_all=TRUE) #144 spp need red list updates!

# Update redlist categories of mammal layers
layers2 <- left_join(layers2, iucn_ass_tax)
layers2$category <- as.character(layers2$category)
layers2$redlistCategory2019 <- as.character(layers2$redlistCategory2019)
layers2 <- layers2 %>% mutate(category = ifelse(is.na(redlistCategory2019)==FALSE & category!=redlistCategory2019, redlistCategory2019, as.character(category))) # Corrected!


# Checking
check <- layers2 %>% dplyr::select(binomial, category, redlistCategory2019, category)
st_geometry(check) <- NULL
table(check$redlistCategory2019, check$category)
sum(is.na(check$category)) #0
check %>% filter(redlistCategory2019 != category) %>% distinct(binomial, .keep_all=TRUE) %>% nrow() #0, correct!
n_distinct(check$binomial)

rm(iucn_ass_tax, iucn_tax_ori, update.iucnnames, update.redlist, layers_df, layers_df2, check)

# Save corrected shpfiles!
system.time(st_write(layers2, dsn=paste0(GISfolder, "Biodiversity Maps/IUCN_amphibians_correctedto2019.gpkg"), layer_options =c("OVERWRITE=YES"))) #7.42s 


####### * ONCE YOU HAVE SAVED THE AMPH LAYER WITH CORRECT NAMES, CAN SKIP ABOVE SECTION AND START HERE ~~~~ ##########
#### * Load corrected amph shpfiles ####
layers2 <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_amphibians_correctedto2019.gpkg")) #1.93s 


# Calculating average threat score to assign to DD
iucn_DD_sc <- layers2
st_geometry(iucn_DD_sc) <- NULL
table(iucn_DD_sc$category)
iucn_DD_sc <- iucn_DD_sc %>% dplyr::filter(category != 'EX', category != 'EW', category != 'DD') %>%
  mutate(iucn_sc = recode(category, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) %>%
  distinct(binomial, .keep_all=TRUE)

table(iucn_DD_sc$category, iucn_DD_sc$iucn_sc)

iucn_DD_sc_pct <- iucn_DD_sc %>% group_by(category) %>% summarize(percentage=n()/nrow(iucn_DD_sc)*100)
iucn_DD_sc_pct
#47% of amphibian species globally are listed as NT to CR.
10.8+17.3+6.95+11.9

#in our data the average threat score for non-DD species was 8.51 (between VU and EN) for amphibians 
mean(iucn_DD_sc$iucn_sc) #8.51 for LC=2, use DD=9


### Process st layers
layers2 <- layers2 %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX")

#Create new column to indicate spp presence in that cell (for richness rasters)
layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 


# Reference file 
reference <- raster('output/afr/various_rasters/africa.tif') #Path to reference file
referenceProj = as.character(crs(reference)) #aea
referenceExt = extent(reference)
referenceRes = res(reference)
rm(reference)


# # Project shapefile
system.time(layers_aea <- st_transform(layers2, crs=as.character(referenceProj)) ) #14s
str(layers_aea)
#plot(st_geometry(layers_aea))


#### (a) Amph richness, tif  ####

### IMPORTANT - need to return m0 to all zeroes before running the loop!!!

# List of spp
spp <- sort(unique(layers_aea$binomial)) #6598, do this the first time you run the loop
# spp_df <- read.csv('data/amph_spp_in_afr.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 1007sp
# spp <- as.character(spp_df$x)

#layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)

sppnotinafr <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){

    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in afr.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#98s

plot(m0)
writeRaster(m0, filename='output/afr/biodiversity_rasters_cci/m0_afr_amph_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)



#check species whose ranges are in afr
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #5591

length(spp)-length(sppnotinafr2) #1007
sppinafr <- setdiff(spp, sppnotinafr2)
length(unique(sppinafr)) #1007

write.csv(sppinafr, 'data/amph_spp_in_afr.csv', row.names=FALSE)
write.csv(sppnotinafr2, 'data/amph_spp_not_in_afr.csv', row.names=FALSE)



# #### (b) Amph vulnerability, tif #####
dd_pred_amph <- read.csv('data/dd_amph_predictedstatus_clean.csv') %>% dplyr::select(binomial=SCINAME, iucn_sc_pred)
str(dd_pred_amph)

layers_aea2 <- left_join(layers_aea, dd_pred_amph)
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 9, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) 
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category) 

# Checking if DD pred were scored correctly
# layers_df <- layers_aea2
# st_geometry(layers_df) <- NULL
# check <- layers_df %>% distinct(binomial, .keep_all=TRUE)
# table(check$iucn_sc, check$category) #55 nonthreatened, 15=score 8, 7=score 9, 15=score24.
# rm(layers_df, check)

# Get list of spp
#spp <- as.character(sort(unique(layers_aea2$binomial)))
spp_df <- read.csv('data/amph_spp_in_afr.csv') #made in (a)
spp <- as.character(spp_df$x)

#layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in afr.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)

plot(m1)
writeRaster(m1, filename='output/afr/biodiversity_rasters_cci/m0_afr_amph_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_amph_vuln_all_DDpred.tif')
all.equal(m1, m0b, tolerance=0) #TRUE
rm(m0b)

identical(sppinssea, spp2)


#check species whose ranges are in afr
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #should be empty




##### (c) Amph vulnerability, threatened spp only, tif  #####
#layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_threat, layers_aea2$category)
table(layers_aea2$iucn_sc_threat, layers_aea2$iucn_sc)


# Get list of spp
spp_df <- read.csv('data/amph_spp_in_afr.csv')
# spp_df <- read.csv('data/amph_spp_in_afr_threat.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 
# spp <- as.character(spp_df$x)

#layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){

    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not threatened in afr.')    )  
      
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#15s

plot(m2)
writeRaster(m2, filename='output/afr/biodiversity_rasters_cci/m0_afr_amph_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_amph_vuln_threat_DDpred.tif')
all.equal(m2, m0b, tolerance=0) #TRUE
rm(m0b)

identical(sppinssea, spp2)

# when LC/DD = 0,
sppnotinafr2 <- sort(unique(sppnotinafr))
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #687

length(spp)-length(sppnotinafr2) #320

sppinafr <- setdiff(spp, sppnotinafr) #320

write.csv(sppinafr, 'data/amph_spp_in_afr_threat.csv', row.names=FALSE)



#### +Save as png (quick plots for checking) #####

mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_amph_richness_DDpred.tif')
png('output/results/results_afr/m0_afr_amph_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_amph_vuln_all_DDpred.tif')
png('output/results/results_afr/m0_afr_amph_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_amph_vuln_threat_DDpred.tif')
png('output/results/results_afr/m0_afr_amph_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (3) MAKE MAMMAL RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))

layers <- st_read(paste0(GISfolder, "Biodiversity Maps/MAMMALS/MAMMALS.shp")) #takes 22S
str(layers) 
levels(layers$category)


#### Cross-check redlistcategories shpfile df with iucn 2019 assessment ####
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_mammal_all/assessments_corrected.csv')
str(iucn_ass_tax)
levels(iucn_ass_tax$redlistCategory)

iucn_ass_tax <- iucn_ass_tax %>% 
  filter(grepl("Terrestrial",systems)) %>%
  dplyr::mutate(redlistCategory2019 = recode(redlistCategory, "Data Deficient"='DD', "Least Concern"='LC', "Near Threatened"='NT', "Vulnerable"='VU', "Endangered"='EN', "Critically Endangered"='CR', "Extinct"='EX', "Extinct in the Wild"='EW') ) %>% 
  dplyr::select(binomial=scientificName, redlistCategory2019, yearPublished) %>%
  distinct(binomial, .keep_all=TRUE)
#5694 spp incld extinct

# Get df from layers
layers_df <- layers
st_geometry(layers_df) <- NULL

str(layers_df)
layers_df2 <- layers_df %>% 
  dplyr::filter(terrestial == 't') %>% 
  dplyr::select(binomial, category, year_) %>% distinct(binomial, .keep_all=TRUE) #5506 mammals spp in shpfile

check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$category)) #206 are in IUCN2019 but not in shpfile
check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$redlistCategory2019)) # 18 names are in shpfile but not in IUCN2019 list
write.csv(check, 'output/check_mammal_iucn_toresolve.csv')  


# Manually look up spp that were in shpfile but not in IUCN2019 list, and got the updated 2019 spp names
update.iucnnames <- read.csv('output/check_mammal_iucn_corrected.csv') %>% dplyr::select(binomial, SCINAME_IUCN2019) %>% filter(is.na(.$SCINAME_IUCN2019)==FALSE) 
#4 corrected
#The remaining 14 are correct and up to date, so keep as is in shapefile


# Correcting spp names in shpfile to IUCN 2019
layers2 <- left_join(layers, update.iucnnames)

layers2$binomial <- as.character(layers2$binomial)
layers2$SCINAME_IUCN2019 <- as.character(layers2$SCINAME_IUCN2019)
layers2 <- layers2 %>% mutate(binomial = ifelse(is.na(layers2$SCINAME_IUCN2019)==FALSE, SCINAME_IUCN2019, binomial))

# Corrected spp names, checking
layers_df <- layers2
st_geometry(layers_df) <- NULL

layers_df2 <- layers_df %>% 
  dplyr::filter(terrestial == 't') %>% 
  dplyr::select(binomial, category, year_) %>% distinct(binomial, .keep_all=TRUE) #5506 mammals spp in shpfile

check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$redlistCategory2019)) #14, correct
check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$category)) #202 are in IUCN2019 but not in shpfile

# Checking the 202 are in IUCN2019 but not in shpfile. They are mostly reclassified, splits, uncertain taxonomy, so can probably ignore. We don't have shapefiles for those anyway. 
iucn_tax_ori <- read.csv('data/redlist/redlist_species_data_mammal_all/taxonomy.csv') %>% dplyr::rename(binomial = scientificName)
check <- left_join(dplyr::select(check, binomial), iucn_tax_ori)

write.csv(check, 'output/check_mammal_iucn_toresolve2_tax.csv') 
#write.csv(layers_df2, 'output/layers_mammal_df.csv')
# Mormopterus loriae (in shpfile) vs Ozimops loriae (IUCN2019). This name change is more recent than the assessments.csv I downloaded, so I ignore this.


# Identifying spp whose red list categories needs updating
update.redlist <- full_join(iucn_ass_tax, layers2, by='binomial') %>% filter(redlistCategory2019 != category) %>% distinct(binomial, .keep_all=TRUE) #67 spp need red list updates!

# Update redlist categories of mammal layers
layers2 <- left_join(layers2, iucn_ass_tax)
layers2$category <- as.character(layers2$category)
layers2$redlistCategory2019 <- as.character(layers2$redlistCategory2019)
layers2 <- layers2 %>% mutate(category = ifelse(is.na(.$redlistCategory)==FALSE & category!=redlistCategory2019, redlistCategory2019, as.character(category))) # Corrected!

# Checking
check <- layers2 %>% dplyr::select(binomial, category, redlistCategory2019)
st_geometry(check) <- NULL
table(check$redlistCategory2019, check$category)
sum(is.na(check$category))
check %>% filter(redlistCategory2019 != category) %>% distinct(binomial, .keep_all=TRUE) %>% nrow() #0, correct!

rm(iucn_ass_tax, update.iucnnames, update.redlist, layers_df, layers_df2, check)

# Save corrected shpfiles!
system.time(st_write(layers2, dsn=paste0(GISfolder, "Biodiversity Maps/IUCN_mammals_correctedto2019.gpkg"), layer_options =c("OVERWRITE=YES"))) #37s 




#### <---- * Load corrected mammal shpfiles ####
layers2 <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_mammals_correctedto2019.gpkg")) # 

# Calculating average threat score to assign to DD
iucn_DD_sc <- layers2
st_geometry(iucn_DD_sc) <- NULL
table(iucn_DD_sc$category)
iucn_DD_sc <- iucn_DD_sc %>% dplyr::filter(category != 'EX', category != 'EW', category != 'DD') %>%
  mutate(iucn_sc = recode(category, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) %>%
  distinct(binomial, .keep_all=TRUE)

table(iucn_DD_sc$category, iucn_DD_sc$iucn_sc)

iucn_DD_sc_pct <- iucn_DD_sc %>% group_by(category) %>% summarize(percentage=n()/nrow(iucn_DD_sc)*100)

iucn_DD_sc_pct

#32.55% of mammal species globally are listed as NT to CR.
4.19+10.1+7.26+11

#in our data the average threat score for non-DD species was 5.44 (between NT and VU) and 8.48 (between VU and EN) for mammals and amphibians, respectively. 
mean(iucn_DD_sc$iucn_sc) #5.47 for LC=2



#### Processing st layers
layers2 <- layers2 %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX") %>% dplyr::filter(terrestial == 't')

layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

str(layers2)


# # Project shapefile
layers_aea <- st_transform(layers2, crs=as.character(referenceProj))  #25s

#plot(st_geometry(layers_aea))
#plot(layers_aea, max.plot=1)



### (a) Mammal richness, tif ####
### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
str(layers_aea)

#List of spp
spp <- as.character(sort(unique(layers_aea$binomial))) #5602 #run this for the first time
# spp_df <- read.csv('data/mammal_spp_in_afr_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 
# spp <- as.character(spp_df$x)

layers_aea$binomial <- as.character(layers_aea$binomial)
#layers_aea <- layers_aea %>% filter(binomial %in% spp) #Can't use this, as it will exclude the subsp. "Oryx beisa ssp. beisa"  "Oryx beisa ssp. callotis"

sppnotinafr <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){

    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0) 
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)

      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in afr.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
# 
plot(m0)
writeRaster(m0, filename='output/afr/biodiversity_rasters_cci/m0_afr_mammal_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_mammal_richness_DDpred.tif')
# all.equal(m0, m0b, tolerance=0.5) #false
# plot(m0b) #doesn't look visibly diff
# rm(m0b)


#check species whose ranges are in afr
sppnotinafr2 <- sort(unique(sppnotinafr)) #
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #4140

length(spp)-length(sppnotinafr2) #1357
sppinafr <- setdiff(spp, sppnotinafr2) #1357

write.csv(sppinafr, 'data/mammal_spp_in_afr_terrestrial.csv', row.names=FALSE)
write.csv(sppnotinafr2, 'data/mammal_spp_not_in_afr.csv', row.names=FALSE)



# #### (b) Mammal vulnerability, tif #####
dd_pred_mammal <- read.csv('data/DD_mammal_predictedstatus_clean.csv') %>% dplyr::select(binomial = SCINAME , iucn_sc_pred)
str(dd_pred_mammal)

layers_aea2 <- left_join(layers_aea, dd_pred_mammal)
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 6, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) #DD=6, rounded up from 5.47
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)


# Get list of spp
#spp <- as.character(sort(unique(layers_aea2$binomial))) 
spp_df <- read.csv('data/mammal_spp_in_afr_terrestrial.csv') #made in (a)
spp <- as.character(spp_df$x)

layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)

      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not threatened in afr.')    )
      
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#98s
plot(m1)
writeRaster(m1, filename='output/afr/biodiversity_rasters_cci/m0_afr_mammal_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_mammal_vuln_all_DDpred.tif')
# all.equal(m1, m0b, tolerance=0) #TRUE
# plot(m0b)
# rm(m0b)

# checking - sppnotinafr should be empty
length(sort(unique(sppnotinafr))) #correct, only have "" blank




##### (c) Mammal vulnerability, threatened spp only, tif  #####
# Important to have run this line!
#layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)

#If LC, NT, or 'non threatened=3'
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_threat, layers_aea2$category)
table(layers_aea2$iucn_sc_threat, layers_aea2$iucn_sc)

# Get list of spp
spp_df <- read.csv('data/mammal_spp_in_afr_terrestrial.csv') 
# spp_df <- read.csv('data/mammal_spp_in_afr_threat.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 
spp <- as.character(spp_df$x)

layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
     
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], 'is not threatened.')    )  
      
      sppnotinafr[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)
#31s
plot(m2)
writeRaster(m2, filename='output/afr/biodiversity_rasters_cci/m0_afr_mammal_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_mammal_vuln_threat_DDpred.tif')
# all.equal(m2, m0b, tolerance=0)
# plot(m0b)
# rm(m0b)

# checking spp threatened in afr
sppnotinafr2 <- sort(unique(sppnotinafr)) 
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #929

length(spp)-length(sppnotinafr2) #428

sppinafr <- setdiff(spp, sppnotinafr2) #428

write.csv(sppinafr, 'data/mammal_spp_in_afr_threat.csv', row.names=FALSE)



#### +Save as png (quick plots for checking) #####
mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_mammal_richness_DDpred.tif')
png('output/results/results_afr/m0_afr_mammal_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_mammal_vuln_all_DDpred.tif')
png('output/results/results_afr/m0_afr_mammal_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_mammal_vuln_threat_DDpred.tif')
png('output/results/results_afr/m0_afr_mammal_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()




# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (4) MAKE REPTILE RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/REPTILES/REPTILES.shp")) #takes __S
str(layers) 


# #### Cross-check redlist categories shpfile df with iucn 2019 assessment ####
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_reptiles/assessments.csv')
str(iucn_ass_tax)
levels(iucn_ass_tax$redlistCategory)

iucn_ass_tax <- iucn_ass_tax %>%
  filter(grepl("Terrestrial",systems)) %>%
  dplyr::mutate(redlistCategory2019 = recode(redlistCategory, "Data Deficient"='DD', "Least Concern"='LC', "Near Threatened"='NT', "Vulnerable"='VU', "Endangered"='EN', "Critically Endangered"='CR', "Extinct"='EX', "Extinct in the Wild"='EW', "Lower Risk/conservation dependent"='LR/cd', "Lower Risk/least concern"='LR/lc', "Lower Risk/near threatened"='LR/nt' ) ) %>%
  dplyr::select(binomial=scientificName, redlistCategory2019, yearPublished) %>%
  distinct(binomial, .keep_all=TRUE)
# #5694 spp incld extinct

# Get df from layers
layers_df <- layers
st_geometry(layers_df) <- NULL

str(layers_df)
layers_df2 <- layers_df %>% 
  dplyr::filter(terrestial == 'true') %>% 
  dplyr::select(binomial, category, yrcompiled) %>% distinct(binomial, .keep_all=TRUE) #7042 reptiles spp in shpfile

check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$category)) #673 are in IUCN2019 but not in shpfile - these likely to not have species grids, so I don't think I need to update the spp names
check <- full_join(iucn_ass_tax, layers_df2) %>% filter(is.na(.$redlistCategory2019)) # 1 name in shpfile but not in IUCN2019 list - 'Chelonia mydas Hawaiian subpopulation'
layers_df2 %>% filter(binomial == 'Chelonia mydas') #It's endangered globally, but the Hawaiian subpopulation is LC


# Identifying spp whose red list categories needs updating
update.redlist <- full_join(iucn_ass_tax, layers_df2, by='binomial') %>% filter(as.character(redlistCategory2019) != as.character(category)) %>% distinct(binomial, .keep_all=TRUE) #0 spp need red list updates!


# Identifying LR/cd species 
layers_df %>%   dplyr::filter(terrestial == 'true', category =='LR/cd') #only 1, Melanosuchus niger

iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_reptiles/assessments.csv')
LRcd <- iucn_ass_tax %>% filter(scientificName == "Melanosuchus niger")
LRcd$rationale # "Reported to have undergone substantial recovery in several parts of its range. Recent surveys suggest that this species remains widespread and extinction is unlikely'
#LRlc <- iucn_ass_tax %>% filter(redlistCategory == "Lower Risk/least concern")

# Change to LR/cd to NT, LR/lc to LC
str(layers_df)
layers[layers$category == "LR/cd", ]$category <- "NT"
layers[layers$category == "LR/lc", ]$category <- "LC"

rm(check, iucn_ass_tax, layers_df, layers_df2, update.redlist)


# Calculating average threat score to assign to DD
iucn_DD_sc <- layers
st_geometry(iucn_DD_sc) <- NULL
table(iucn_DD_sc$category)

iucn_DD_sc <- iucn_DD_sc %>% dplyr::filter(category != 'EX', category != 'EW', category != 'DD') %>%
  mutate(iucn_sc = recode(category, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) %>%
  distinct(binomial, .keep_all=TRUE)

table(iucn_DD_sc$category, iucn_DD_sc$iucn_sc)

iucn_DD_sc_pct <- iucn_DD_sc %>% group_by(category) %>% summarize(percentage=n()/nrow(iucn_DD_sc)*100)

iucn_DD_sc_pct

#26.29% of reptile species globally are listed as NT to CR.
6.23+7.43+4.5+8.13

#in our data the average threat score for non-DD species was 5.06 (between NT and VU), 5.44 (between NT and VU) and 8.48 (between VU and EN) for reptiles, mammals and amphibians, respectively. 
mean(iucn_DD_sc$iucn_sc) #5.06 when LC=2



#### Processing st layers
layers2 <- layers %>% dplyr::filter(category != "EW", category != "EX") %>% dplyr::filter(terrestial == 'true')

layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

str(layers2)


# # Project shapefile
layers_aea <- st_transform(layers2, crs=as.character(referenceProj))  #25s

#plot(st_geometry(layers_aea))
#plot(layers_aea, max.plot=1)



### (a) Reptile richness, tif ####
### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
str(layers_aea)

#List of spp
spp <- as.character(sort(unique(layers_aea$binomial))) #7028 #run this for the first time
# spp_df <- read.csv('data/reptile_spp_in_afr_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 
# spp <- as.character(spp_df$x)

layers_aea$binomial <- as.character(layers_aea$binomial)

sppnotinafr <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
  
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0) 
    
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in afr.')    )  
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
# 132s= 2+ mins for reptile richness, first time round

plot(m0)
writeRaster(m0, filename='output/afr/biodiversity_rasters_cci/m0_afr_reptile_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_reptile_richness_DDpred.tif')
# all.equal(m0, m0b, tolerance=0) #TRUE
# plot(m0b)
# rm(m0b)


#check species whose ranges are in afr
sppnotinafr2 <- sort(unique(sppnotinafr)) #
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #5993

length(spp)-length(sppnotinafr2) #1035
sppinafr <- setdiff(spp, sppnotinafr2) #1035

write.csv(sppinafr, 'data/reptile_spp_in_afr_terrestrial.csv', row.names=FALSE)
write.csv(sppnotinafr2, 'data/reptile_spp_not_in_afr.csv', row.names=FALSE)


# #### (b) Reptile vulnerability, tif #####
layers_aea2 <- layers_aea
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 5, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) #DD=5, rounded up from 5.06

table(layers_aea2$iucn_sc, layers_aea2$category)


# Get list of spp
spp_df <- read.csv('data/reptile_spp_in_afr_terrestrial.csv') #made in (a)
spp <- as.character(spp_df$x)

#layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in afr.')    )
      
      sppnotinafr[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#74s
plot(m1)
writeRaster(m1, filename='output/afr/biodiversity_rasters_cci/m0_afr_reptile_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_reptile_vuln_all_DDpred.tif')
all.equal(m1, m0b, tolerance=0.5) #TRUE
plot(m0b)
rm(m0b)

# checking - sppnotinafr should be empty
length(sort(unique(sppnotinafr))) #correct, only have "" blank




##### (c) Reptile vulnerability, threatened spp only, tif  #####
# Important to have run this line!
#layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)

#If LC, NT, or 'non threatened=3'
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_threat, layers_aea2$category)
table(layers_aea2$iucn_sc_threat, layers_aea2$iucn_sc)

# Get list of spp
spp_df <- read.csv('data/reptile_spp_in_afr_terrestrial.csv') 
# spp_df <- read.csv('data/reptile_spp_in_afr_threat.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in afr so i can just use this to speed things up. 
spp <- as.character(spp_df$x)

layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinafr <- character(nrow(layers_aea2)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
  
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
     
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], 'is not threatened.')    )  
      
      sppnotinafr[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)
#52s
plot(m2)

writeRaster(m2, filename='output/afr/biodiversity_rasters_cci/m0_afr_reptile_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/afr/biodiversity_rasters_cci_before27apr2020/m0_afr_reptile_vuln_threat_DDpred.tif')
# all.equal(m2, m0b, tolerance=0) #TRUE
# plot(m0b)
# rm(m0b)

# checking spp threatened in afr
sppnotinafr2 <- sort(unique(sppnotinafr)) 
sppnotinafr2 <- sppnotinafr2[sppnotinafr2 != ""] #714

length(spp)-length(sppnotinafr2) #321

sppinafr <- setdiff(spp, sppnotinafr2) #321

write.csv(sppinafr, 'data/reptile_spp_in_afr_threat.csv', row.names=FALSE)



#### +Save as png (quick plots for checking) #####
mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_reptile_richness_DDpred.tif')
png('output/results/results_afr/m0_afr_reptile_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_reptile_vuln_all_DDpred.tif')
png('output/results/results_afr/m0_afr_reptile_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/afr/biodiversity_rasters_cci/m0_afr_reptile_vuln_threat_DDpred.tif')
png('output/results/results_afr/m0_afr_reptile_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()




