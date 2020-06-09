#### rubberxbiodiversityCB
#### 20-requiredinput.R #### 

# Rough listing of required input files for scripts. 
# Files in the data/ folder are provided. 
# Files in GISfolder are not.


#### 01-afr_rasters.R ####
listofafricancountries <- read.csv('data/listofafricancountries.csv')

gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')

input <- raster(paste0(GISfolder, 'LandCover CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'))
input <- raster(paste0(GISfolder, 'Rubber Suitability/RubberSuit.tif'))
input <- raster(paste0(GISfolder, 'HCS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif'))
input <- raster(paste0(GISfolder, 'accessibility/accessibility_to_cities_2015_v1.0/accessibility_to_cities_2015_v1.0.tif'))
input <- raster(paste0(GISfolder, "WDPA/WDPA_July2018/WDPA_July2018_combraster.tif")) 
# This tif file is not directly downloadable from WDPA, as I processed the shapefiles in ArcGIS, but I have provided my processing steps below, and can also share this tif file upon request.
# Methods, as written in the article: Protected areas (georeferenced polygons and points) were downloaded from the World Database on Protected Areas (WDPA) [38]. We first converted the point data in ArcMap ver. 10.4.1 by applying a geodesic buffer corresponding to the reported size of the protected area, as recommended in the WDPA User Manual [45]. We excluded points with reported areas of zero. We combined and rasterized the polygon and buffered point data, and projected it onto the 100 km2 reference grid. 

reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')


system.time(layers <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW.gdb"))) #takes a few mins
iucn_ass_tax <- read.csv(GISfolder, 'Biodiversity Maps/HBW-BirdLife_Checklist_v3_Nov18/HBW-BirdLife_List_of_Birds_v3.csv')
birdsnotinredlist <- read.csv('data/redlist/birdsnotinredlist_repaired.csv')
dd_pred_bird <- read.csv('data/DD_bird_predictedstatus_clean.csv') %>% dplyr::select(binomial =SCINAME, iucn_sc_pred)


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/AMPHIBIANS/AMPHIBIANS.shp")) #takes 5.4S
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_amph_all/assessments.csv')
update.iucnnames <- read.csv('output/check_amph_iucn_corrected.csv')
dd_pred_amph <- read.csv('data/dd_amph_predictedstatus_clean.csv') 


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/MAMMALS/MAMMALS.shp")) 
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_mammal_all/assessments.csv')
dd_pred_mammal <- read.csv('data/DD_mammal_predictedstatus_clean.csv')


layers <- st_read(paste0(GISfolder, "Biodiversity Maps/REPTILES/REPTILES.shp"))
iucn_ass_tax <- read.csv('data/redlist/redlist_species_data_reptiles/assessments.csv')



#### 02-ssea_rasters.R #### 
input <- raster(paste0(GISfolder, "Li and Fox 2012/MMSEA_Rubber_2009-2010_MODIS250m.rst"))
input <- raster(paste0(GISfolder, "Hurni/byyear/mmsea2014.tif")) #spatial data processed in 00-preprocess-hurni-rubbermaps.R

input <- st_read(paste0(GISfolder, "Global Forest Watch Tree Plantations/gfw_plantations.shp"))



#### 04-results-fig1-tableS1.R #### 
countryNA_corr <- read.csv("data/afr_points_countryNA_corrected.csv")
countryNA_corr <- read.csv("data/ssea_points_countryNA_corrected.csv")



#### 05-maps-fig2-figS1.R ####
conc_sf <- st_read('data/conc_sf_landmatrix.gpkg') #this gpkg was created from data downloaded from landmatrix.org, which is freely available under the Creative Commons Attribution 4.0 International License. Hence I am able to share my processed data. Code and input data used to make this gpkg are in 00-preprocess-landmatrix.R 



#### 06-suit-expanded-rasters-figS2.R #### 
prec.files <- list.files(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_prec/"), ".tif", full.names=TRUE) #119s
prec <-  raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif"))
prec.dq <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_17.tif"))
cru.files <- list.files(paste0(GISfolder, "ClimateData/CRU/"), ".gz", full.names=TRUE)
tavg.wq <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_8.tif"))
tsea <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_4.tif"))




#### 10-scenarios_prep.R ####
forspp_corr <- read.csv("data/forestdependentspecies_iucnredlist2019.csv") #Derived from Table S3 from Tracewski et al. (2016). Toward quantification of the impact of 21st-century deforestation on the extinction risk of terrestrial vertebrates. Conserv. Biol. 30, 1070–1079.

fao <- read.csv('data/FAOSTAT_yield_NR_World_2020-04-21.csv') 
#   “FAO. FAOSTAT. License: CC BY-NC-SA 3.0 IGO. Extracted from: http://www.fao.org/faostat/en/#data/QC. Data of Access: 21-04-2020.”