gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')
read.csv('data/listofafricancountries.csv')
input <- raster(paste0(GISfolder, 'LandCover CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'))
input <- raster(paste0(GISfolder, 'Rubber Suitability/RubberSuit.tif'))
input <- raster(paste0(GISfolder, 'HCS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif'))
input <- raster(paste0(GISfolder, 'accessibility/accessibility_to_cities_2015_v1.0/accessibility_to_cities_2015_v1.0.tif'))
input <- raster(paste0(GISfolder, "WDPA/WDPA_July2018/WDPA_July2018_combraster.tif")) #5.3GB, took me 761s=12m OR 335s=6mins

input <- raster(paste0(GISfolder, 'Strona et al. 2018/INPUT_DATA/suitability_rasters/rainfed_intermediate_continuous/data.asc'))
op_conc <- st_read(paste0(GISfolder, 'Global Forest Watch Oil Palm/Oil_palm_concessions_select_countries.shp'))

system.time(layers <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW.gdb"))) #takes a few mins
iucn_ass_tax <- read.csv('data/HBW-BirdLife_Checklist_v3_Nov18/HBW-BirdLife_List_of_Birds_v3.csv')
birdsnotinredlist <- read.csv('data/redlist/birdsnotinredlist_repaired.csv')


dd_pred_bird <- read.csv('data/DD_bird_predictedstatus_clean.csv') %>% dplyr::select(binomial =SCINAME, iucn_sc_pred)

layers <- st_read(paste0(GISfolder, "Biodiversity Maps/AMPHIBIANS/AMPHIBIANS.shp")) #takes 5.4S
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_amph_all/assessments.csv')
update.iucnnames <- read.csv('output/check_amph_iucn_corrected.csv')
iucn_tax_ori <- read.csv('data/redlist/redlist_species_data_amph_all/taxonomy.csv') %>% dplyr::rename(binomial = scientificName)

dd_pred_amph <- read.csv('data/dd_amph_predictedstatus_clean.csv') 

reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/MAMMALS/MAMMALS.shp")) 
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_mammal_all/assessments_corrected.csv')
iucn_tax_ori <- read.csv('data/redlist/redlist_species_data_mammal_all/taxonomy.csv')

dd_pred_mammal <- read.csv('data/DD_mammal_predictedstatus_clean.csv')


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/REPTILES/REPTILES.shp"))

iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_reptiles/assessments.csv')



###### SSEA
input <- raster(paste0(GISfolder, "Jeff Fox/Li and Fox 2012/MMSEA_Rubber_2009-2010_MODIS250m.rst"))

input <- raster("C:/Users/bop17mw/Desktop/GIS_Files/Jeff Fox/byyear/mmsea2014.tif")
# NEED TO POST CODE of preprocessing!

input <- st_read(paste0(GISfolder, "Global Forest Watch Tree Plantations/gfw_plantations.shp"))



#### 21.9-rubberdemand.R ####
fao <- read.csv('data/FAOSTAT_yield_NR_World_2018-05-10.csv')


#### 21.5-scenarios.R ####
forspp_corr <- read.csv("data/forestdependentspecies_iucnredlist2019.csv")
