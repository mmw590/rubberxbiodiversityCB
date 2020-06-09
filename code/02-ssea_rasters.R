#### rubberxbiodiversityCB 
#### 02-ssea_rasters.R #### 
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
#detach(package:lwgeom)

# Set path to folder containing GIS input files
GISfolder <- './GIS_Files/'
#GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'


# Create output folders #####
if (!dir.exists(file.path('output', 'ssea'))) {
  (dir.create(file.path('output', 'ssea'))) }

if (!dir.exists(file.path('output/ssea', 'biodiversity_rasters'))) {
  (dir.create(file.path('output/ssea', 'biodiversity_rasters'))) }

if (!dir.exists(file.path('output/ssea', 'various_rasters'))) {
  (dir.create(file.path('output/ssea', 'various_rasters'))) }

if (!dir.exists(file.path('output/results', 'results_ssea'))) {
  (dir.create(file.path('output/results', 'results_ssea'))) }

if (!dir.exists(file.path('output/dfs_for_figs', 'ssea'))) {
  (dir.create(file.path('output/dfs_for_figs', 'ssea'))) }


#### Determining Extent and Projection #####
xmin =  69.25
xmax =  158.4 
ymin = -12.5 
ymax =  30.5

refExtent_wgs <- extent(c(xmin,xmax,ymin,ymax))

standard_parallel_1 = ymin+(ymax-ymin)/6 #-5.3333
standard_parallel_2 = ymax-(ymax-ymin)/6 #23.33333
latitude_of_center = (xmin+xmax)/2 #113.825
longitude_of_center = (ymin+ymax)/2 #9

refProj_aea <- paste0("+proj=aea +lat_1=", standard_parallel_1, " +lat_2=", standard_parallel_2, " +lat_0=", latitude_of_center, " +lon_0=", longitude_of_center, " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


### + Make polygons of SSEA countries ####
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0') #gadm countries
ssea_wgs <- st_crop(gadm_0, refExtent_wgs)
plot(st_geometry(ssea_wgs))

ssea_aea <- st_transform(ssea_wgs, crs=as.character(refProj_aea))
plot(st_geometry(ssea_aea)) #is tilted.
extent(ssea_aea) #note that the extent will change after projecting to grid, 



# ### + Make raster (ref grid) ----
# Create a blank raster layer with desired proj (aea), extent, resolution
target_ds <- raster(crs=refProj_aea, ext=extent(ssea_aea), res=10000, vals=0) #res is pixel size, vals is no data value

target_ds2 <- fasterize(st_collection_extract(ssea_aea, "POLYGON"), target_ds, background=0)

plot(target_ds2)

writeRaster(target_ds2, filename = 'output/ssea/various_rasters/ssea.tif', datatype='INT1U', overwrite=TRUE, options=c("COMPRESS=LZW")) #base grid




###### PROJECTING & RESAMPLING RASTERS TO MATCH REF GRID ---- ######
reference <- raster('output/ssea/various_rasters/ssea.tif') #Path to reference file
referenceProj = as.character(crs(reference)) #aea
referenceExt = extent(reference)
referenceRes = res(reference)
plot(reference)

# define output raster
output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)

cropExtent_wgs <- extent(c(60,180,-40,50)) #for pre-cropping global rasters to speed up projectRaster

rm(target_ds, target_ds2, refProj_aea, latitude_of_center, longitude_of_center, standard_parallel_1, standard_parallel_2, xmax, xmin, ymax, ymin)



# +LandUse ####
# Reprojecting and resampling the land cover raster to match the ref grid
input <- raster(paste0(GISfolder, 'LandCover CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'))
system.time(input <- crop(input, cropExtent_wgs)) #135s
system.time(output.lu <- projectRaster(input, output, method='ngb', filename='output/ssea/various_rasters/land_use_ssea.tif', datatype='INT1U', overwrite=TRUE)) #11s after pre-cropping; 247s without pre-cropping

#plot(output.lu)
#plot(input)


# +Suitability ####
input <- raster(paste0(GISfolder, 'Rubber Suitability/RubberSuit.tif'))

system.time(output.suit <- projectRaster(input, output, method='bilinear', filename='output/ssea/various_rasters/suitability_ssea.tif', overwrite=TRUE)) #19s. datatype="FLT4S", because decimals, continuous scale

#plot(output.suit)


# +Carbon ####
input <- raster(paste0(GISfolder, 'HCS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif'))
#range(values(input), na.rm=TRUE) #0-633
#plot(input) #need to rescale to 0-1

#http://www.timassal.com/?p=859
rasterRescale<-function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

system.time(input.rsc <- rasterRescale(input)) #2 mins
#plot(input.rsc)

system.time(output.carb <- projectRaster(input.rsc, output, method='bilinear', filename='output/ssea/various_rasters/carbon_ssea.tif', overwrite=TRUE)) #5s
#plot(output.carb)


# +Accessibility ####
input <- raster(paste0(GISfolder, 'accessibility/accessibility_to_cities_2015_v1.0/accessibility_to_cities_2015_v1.0.tif'))
plot(input)
range(values(input)) #-9999, 41556 - although metadata said the min value should be 0! remove all cells =-9999. Not sure what to do about cells > -9999 and < 0.

# Replace non zero values with NA
input[input <0] <- NA #this takes a few mins

system.time(input.rsc <- rasterRescale(input)) #265s
#plot(input.rsc)

system.time(input.rsc2 <- crop(input.rsc, cropExtent_wgs) ) #16s
#plot(input.rsc2)
#hist(input.rsc2)
range(values(input.rsc2), na.rm=TRUE) #0,0.91

system.time(output.acc <- projectRaster(input.rsc2, output, method='bilinear', filename='output/ssea/various_rasters/accessibility_ssea.tif', overwrite=TRUE)) #4s

#plot(output.acc)
#hist(output.acc)

rm(output.lu, output.suit, output.carb, output.acc)




# +Protected Areas #######
input <- raster(paste0(GISfolder, "WDPA/WDPA_July2018/WDPA_July2018_combraster.tif")) #5.3GB, took me 761s=12m OR 335s=6mins

# Reclassify to binary
reclassify.df <- data.frame(oldval=c(0,1,2), newval=c(0,1,1)) %>% arrange(oldval)
input <- subs(input, reclassify.df, by='oldval', which='newval',subsWithNA=FALSE)

system.time(input <- crop(input, cropExtent_wgs) ) #16s

## Project to aea and resample to ref grid
system.time(output.pa <- projectRaster(input, output, method='ngb', filename='output/ssea/various_rasters/protected_areas_ssea.tif', datatype='INT1U', overwrite=TRUE)) #375s
output.pa
plot(output.pa)

#rm objects to clear up memory
rm(output.pa, input, reclassify.df)



# +Rubber Concessions (Raster) - merging different sources ----

### LCC map of Mainland SEA
input <- raster(paste0(GISfolder, "Li and Fox 2012/MMSEA_Rubber_2009-2010_MODIS250m.rst"))
plot(input)
input
reclassify.df <- data.frame(oldlabel = 0:3, newlabel=c(NA,1,1,1) )
input <- subs(input, reclassify.df, by='oldlabel', which='newlabel',subsWithNA=FALSE)

output.conc.lifox <- projectRaster(input, output, method='ngb', filename='output/ssea/various_rasters/concessions_mmsea_li&fox.tif', datatype='INT1U', overwrite=TRUE) #0.39s

plot(output.conc.lifox)
freq(output.conc.lifox)


### LCC map of Mainland SEA, raster (pre-processed), from Hurni et al. 2017
input <- raster(paste0(GISfolder, "Hurni/byyear/mmsea2014.tif")) # pre-processed in 00-preprocess-hurni-rubbermaps.R
sort(unique(values(input))) # 8=rubber

reclassify.df <- data.frame(oldlabel = 1:17, newlabel=c(rep(NA, 7), 1, rep(NA, length(9:17))) )
input <- subs(input, reclassify.df, by='oldlabel', which='newlabel',subsWithNA=FALSE)

output.conc.hurni <- projectRaster(input, output, method='ngb', filename='output/ssea/various_rasters/concessions_mmsea_hurni.tif', datatype='INT1U', overwrite=TRUE) #0.53s
names(output.conc.hurni) <- 'conc'
plot(output.conc.hurni)
freq(output.conc.hurni)


### Tree Plantations from GFW, for Malaysia and Indonesia
input <- st_read(paste0(GISfolder, "Global Forest Watch Tree Plantations/gfw_plantations.shp"))
str(input)

input <- dplyr::filter(input, country %in% c('Malaysia', 'Indonesia')) %>% 
  dplyr::filter(spec_1 == 'Hevea') %>% #where dominant spp is rubber, don't include those where rubber is non-dominant...
  dplyr::mutate(conc = 1)

input_aea <- st_transform(input, referenceProj)

output.conc.gfw <- fasterize(input_aea, output, field='conc', background=NA)
writeRaster(output.conc.gfw, 'output/ssea/various_rasters/concessions_gfw.tif', datatype='INT1U', overwrite=TRUE)
plot(output.conc.gfw)
freq(output.conc.gfw)


# Comparing Li&Fox and Hurni.... 
output.conc.lifox <- raster('output/ssea/various_rasters/concessions_mmsea_li&fox.tif')
output.conc.hurni <- raster('output/ssea/various_rasters/concessions_mmsea_hurni.tif')

conc.check <- as.data.frame( stack(output.conc.lifox, output.conc.hurni) )
head(conc.check)
conc.check <- conc.check %>% 
  rename(lifox = concessions_mmsea_li.fox, hurni = concessions_mmsea_hurni) %>% 
  dplyr::filter(is.na(.$lifox)==FALSE & is.na(.$hurni)==FALSE)

table(conc.check$lifox, conc.check$hurni) #mostly overlap


# Combining (summing) the 3 concession rasters
conc.msea <- stack(output.conc.lifox, output.conc.hurni, output.conc.gfw) %>% sum(., na.rm=TRUE)
freq(conc.msea)
plot(conc.msea)

conc.msea[conc.msea>0] <- 1 #if any map says there's rubber, assume there's rubber there.

writeRaster(conc.msea, filename='output/ssea/various_rasters/concessions_ssea.tif', datatype='INT1U', overwrite=TRUE)



# +Country ####
##### + Create raster with country ID as raster values
ssea_countries_wgs <- st_crop(gadm_0, cropExtent_wgs)

ssea_countries <- st_transform(ssea_countries_wgs, crs=referenceProj)
plot(st_geometry(ssea_countries)) #is tilted.
extent(ssea_countries)

ssea_countries$GID_0 #58 countries
ssea_countries$countryID <- 1:nrow(ssea_countries) #58
plot(ssea_countries["countryID"])

ssea_countries_rst <- fasterize(st_collection_extract(ssea_countries, "POLYGON"), output, field="countryID", background=NA)
plot(ssea_countries_rst)
extent(ssea_countries_rst)
ssea_countries_rst

writeRaster(ssea_countries_rst, "output/ssea/various_rasters/ssea_countryID.tif", datatype='INT1U', overwrite=TRUE)  #create raster with countryID

# Save countryID lookup table
ssea_countries_df <- st_drop_geometry(ssea_countries)
write.csv(ssea_countries_df, "output/ssea/various_rasters/ssea_countryID.csv", row.names=FALSE)




####### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
####### £££££££  MASKING OUT LU ETC FROM RASTERS  £££££££££££ ####
####### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))


####### +Load LU mask ########
mat_land_use_cci <- raster('output/ssea/various_rasters/land_use_ssea.tif')

reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')

mat_land_use <- subs(mat_land_use_cci, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)
plot(mat_land_use)


rm(reclassify.df, mat_land_use_cci)


### +Load PAs ####
mat_pa <- raster('output/ssea/various_rasters/protected_areas_ssea.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use, maskvalue=0, updatevalue=NA)

# Transform to wgs for plotting
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
mat_pa2_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_pa)[1], ncols=dim(mat_pa)[2])
mat_pa2_latlon <- projectRaster(mat_pa2, mat_pa2_latlon, crs=crs_wgs, method='ngb')
plot(mat_pa2_latlon)

pa_spdf <- rasterToPoints(mat_pa2_latlon)
pa_df <- as.data.frame(pa_spdf)
head(pa_df)

write.csv(pa_df, 'output/dfs_for_figs/ssea/pa_df_LU.csv', row.names=FALSE)

rm(mat_pa, mat_pa2_latlon, pa_spdf, pa_df)


### +Load rubber concessions ####
mat_conc <- raster('output/ssea/various_rasters/concessions_ssea.tif') 
mat_conc2 <- mask(mat_conc, mat_conc, maskvalue=0, updatevalue=NA)
plot(mat_conc2)

mat_conc2_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_conc)[1], ncols=dim(mat_conc)[2])
mat_conc2_latlon <- projectRaster(mat_conc2, mat_conc2_latlon, crs=crs_wgs, method='ngb')
plot(mat_conc2_latlon)

conc_spdf <- rasterToPoints(mat_conc2_latlon)
conc_df <- as.data.frame(conc_spdf)
head(conc_df)

write.csv(conc_df, 'output/dfs_for_figs/ssea/conc_df_raster.csv', row.names=FALSE)

rm(mat_conc, mat_conc2_latlon, conc_spdf, conc_df)






# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (1) MAKE BIRD RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), "GISfolder"))
gc()

system.time(layers_2 <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_wgs2.gpkg")) ) #109s to load

reference <- raster('output/ssea/various_rasters/ssea.tif')
referenceProj = as.character(crs(reference)) #aea

system.time(layers_aea <- st_transform(layers_2, crs=referenceProj)) #177s

#system.time(st_write(layers_aea, dsn = paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_aea_asia.gpkg"), layer_options="OVERWRITE=YES") ) #
rm(layers_2, reference, referenceProj)


####### * ONCE YOU HAVE SAVED THE BIRD LAYER IN AEA, CAN SKIP ABOVE SECTION AND START HERE ~~~~ ##########
#system.time(layers_aea <- st_read(paste0(GISfolder, "Biodiversity Maps/BOTW_iucn_aea_asia.gpkg")) ) #
st_crs(layers_aea)

layers_aea <- layers_aea %>% dplyr::select(SISID:DATE_, PRESENCE:SEASONAL, VERSION:presence_richness) %>% rename(binomial = SCINAME, category = redlistCategory)
str(layers_aea)

reference <- raster('output/ssea/various_rasters/ssea.tif') #Path to reference file
referenceProj = as.character(crs(reference)) #aea
referenceExt = extent(reference)
referenceRes = res(reference)

rm(reference)
gc()



#### (a) Bird richness tif ####
# Get list of bird species
spp <- as.character(sort(unique(layers_aea$binomial))) #for the first time round

# spp_df <- read.csv('output/ssea/birds_spp_in_ssea.csv') #after i've run the loop once, i'll have a shortened list of all birds that occur in ssea so i can just use this to speed things up. 
# spp2 <- as.character(spp_df$x)

layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)


### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
sppnotinssea <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

# Species Richness Loop
system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )  
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#2038s

plot(m0)
writeRaster(m0, filename='output/ssea/biodiversity_rasters/m0_ssea_bird_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


#check species whose ranges are in ssea
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] 

length(spp)-length(sppnotinssea2) #3980

sppinssea <- unique(setdiff(spp, sppnotinssea2)) #3980

write.csv(sppinssea, 'output/ssea/birds_spp_in_ssea.csv', row.names=FALSE) #3980




#### (b) Bird vulnerability, tif ####
# Load predicted DD status
dd_pred_bird <- read.csv('data/DD_bird_predictedstatus_clean.csv') %>% dplyr::select(binomial=SCINAME, iucn_sc_pred)
table(dd_pred_bird$iucn_sc_pred)

layers_aea2 <- left_join(layers_aea, dd_pred_bird)
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 4, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, 'CR (PE)'=32, 'CR (PEW)'=32, .default = 0) ) 
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$iucn_sc_pred)

# Checking if DD pred were scored correctly
layers_df <- layers_aea2
st_geometry(layers_df) <- NULL
check <- layers_df %>% distinct(binomial, .keep_all=TRUE)
table(check$iucn_sc, check$category) 


# Get list of bird species
spp_df <- read.csv('output/ssea/birds_spp_in_ssea.csv') #made in (a)
spp <- as.character(spp_df$x)

layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not in SEA.')    )  
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#1979s

plot(m1)
writeRaster(m1, filename='output/ssea/biodiversity_rasters/m0_ssea_bird_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)




#### (c) Bird vulnerability, threatened spp only, tif ####
# Important to have run the line to update iucn_sc to iucn_sc_pred!
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_threat, layers_aea2$category)

# Get list of bird species
spp_df <- read.csv('output/ssea/birds_spp_in_ssea.csv') 
# spp_df <- read.csv('output/ssea/birds_spp_in_ssea_threat.csv') 
spp <- as.character(spp_df$x)

layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not threatened in ssea.')    )  
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)

plot(m2)
writeRaster(m2, filename='output/ssea/biodiversity_rasters/m0_ssea_bird_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


#check species 
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] 

length(spp)-length(sppnotinssea2) #491

sppinssea_threat <- unique(setdiff(spp, sppnotinssea)) #491

write.csv(sppinssea_threat, 'output/ssea/birds_spp_in_ssea_threat.csv', row.names=FALSE)


#### +Save as png (quick plots for checking) #####
mat_rich <- raster('output/ssea/biodiversity_rasters/m0_ssea_bird_richness_DDpred.tif')
png('output/results/results_ssea/m0_ssea_bird_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_rich)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_bird_vuln_all_DDpred.tif')
png('output/results/results_ssea/m0_ssea_bird_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_bird_vuln_threat_DDpred.tif')
png('output/results/results_ssea/m0_ssea_bird_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()




# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (2) MAKE AMPHIBIAN RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))

#### * Load corrected amph shpfiles (made in 01-afr_rasters.R) ####
layers2 <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_amphibians_correctedto2019.gpkg"))

### Process st layers
layers2 <- layers2 %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX")

layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

# # Project shapefile
system.time(layers_aea <- st_transform(layers2, crs=as.character(referenceProj)) ) #14s
str(layers_aea)
#plot(st_geometry(layers_aea))


#### (a) Amph richness  ####
# List of spp
spp <- sort(unique(layers_aea$binomial)) #6631, do this the first time you run the loop
# spp_df <- read.csv('output/ssea/amph_spp_in_ssea.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in ssea so i can just use this to speed things up. 
# spp <- as.character(spp_df$x)

#layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)

### IMPORTANT - need to return m0 to all zeroes before running the loop!!!
sppnotinssea <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )  
      sppnotinssea[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)

plot(m0)
writeRaster(m0, filename='output/ssea/biodiversity_rasters/m0_ssea_amph_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


#check species whose ranges are in ssea
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #5069

length(spp)-length(sppnotinssea2) #1529
sppinssea <- unique(setdiff(spp, sppnotinssea2)) #1529

write.csv(sppinssea, 'output/ssea/amph_spp_in_ssea.csv', row.names=FALSE)



#### (b) Amph vulnerability, tif #####
# in our data the average threat score for non-DD species was 8.51 (between VU and EN) for amphibians 
# use DD=9

dd_pred_amph <- read.csv('data/DD_amph_predictedstatus_clean.csv') %>% dplyr::select(binomial=SCINAME, iucn_sc_pred)
str(dd_pred_amph)

layers_aea2 <- left_join(layers_aea, dd_pred_amph)
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 9, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) 
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)

# Get list of spp
spp_df <- read.csv('output/ssea/amph_spp_in_ssea.csv') #made in (a)
spp <- as.character(spp_df$x)

layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )  
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#197.2s
plot(m1)

writeRaster(m1, filename='output/ssea/biodiversity_rasters/m0_ssea_amph_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)



# #### (c) Amph vulnerability, threatened spp only, tif ####
#layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

layers_aea2 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea2$iucn_sc_pred, layers_aea2$category)
table(layers_aea2$iucn_sc_threat, layers_aea2$category) #check that there are no threatened categories scored 0
table(layers_aea2$iucn_sc_threat, layers_aea2$iucn_sc)

spp_df <- read.csv('output/ssea/amph_spp_in_ssea.csv') 
# spp_df <- read.csv('output/ssea/amph_spp_in_ssea_threat.csv')
spp <- as.character(spp_df$x)

layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea2)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc_threat')] 
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0)
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' is not threatened.')    )  
      
      sppnotinssea[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)
#24s 
plot(m2) #26s

writeRaster(m2, filename='output/ssea/biodiversity_rasters/m0_ssea_amph_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


# check species
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #1060

sppinssea_threat <- setdiff(spp, sppnotinssea2) #469
length(spp)-length(sppnotinssea2) #469

write.csv(sppinssea_threat, 'output/ssea/amph_spp_in_ssea_threat.csv', row.names=FALSE) 



#### +Save as png (quick plots for checking) #####
mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_amph_richness_DDpred.tif')
png('output/results/results_ssea/m0_ssea_amph_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_amph_vuln_all_DDpred.tif')
png('output/results/results_ssea/m0_ssea_amph_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_amph_vuln_threat_DDpred.tif')
png('output/results/results_ssea/m0_ssea_amph_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()





# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (3) MAKE MAMMAL RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))

#### * Load corrected mammals shpfiles (made in 01-areasofcompromise_SEA.R) ####
layers2 <- st_read(paste0(GISfolder, "Biodiversity Maps/IUCN_mammals_correctedto2019.gpkg")) #13.28s 

layers2 <- layers2 %>% dplyr::filter(category != "EW") %>% dplyr::filter(category != "EX") %>% dplyr::filter(terrestial == 't')
#12769 layers

layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

# # Project shapefile
layers_aea <- st_transform(layers2, crs=as.character(referenceProj))  #25s


### (a) Mammal richness ####
### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
str(layers_aea)

#List of spp
spp <- sort(unique(layers_aea$binomial)) 
# spp_df <- read.csv('output/ssea/mammal_spp_in_ssea_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in ssea so i can just use this to speed things up. 
# spp <- as.character(spp_df$x)

#layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)

sppnotinssea <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0) 
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )  
      sppnotinssea[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)
#155s
plot(m0)

writeRaster(m0, filename='output/ssea/biodiversity_rasters/m0_ssea_mammal_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


#check species whose ranges are in ssea
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #3666

length(spp)-length(sppnotinssea2) #1831
sppinssea <- setdiff(spp, sppnotinssea2) #1831

write.csv(sppinssea, 'output/ssea/mammal_spp_in_ssea_terrestrial.csv', row.names=FALSE)



#### (b) Mammal vulnerability, tif #####
#in our data the average threat score for non-DD species was 5.47 (between NT and VU) for mammals. Use DD=6
dd_pred_mammal <- read.csv('data/DD_mammal_predictedstatus_clean.csv') %>% dplyr::select(binomial=SCINAME, iucn_sc_pred)
str(dd_pred_mammal)

layers_aea2 <- left_join(layers_aea, dd_pred_mammal)
layers_aea2 <- layers_aea2 %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 6, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) 
layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)

# Get list of spp
spp_df <- read.csv('output/ssea/mammal_spp_in_ssea_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in ssea so i can just use this to speed things up.
spp <- as.character(spp_df$x)

#layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)

plot(m1)

writeRaster(m1, filename='output/ssea/biodiversity_rasters/m0_ssea_mammal_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# checking - sppnotinssea should be empty
sppnotinssea2 <- sort(unique(sppnotinssea)) #correct




#### (c) Mammal vulnerability, threatened spp only, tif ####
# Important to have run this line!
#layers_aea2 <- layers_aea2 %>% mutate(iucn_sc = ifelse(is.na(.$iucn_sc_pred)==FALSE, iucn_sc_pred, iucn_sc))

table(layers_aea2$iucn_sc, layers_aea2$category)

#If LC, NT, or 'non threatened=3'
layers_aea3 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea3$iucn_sc_threat, layers_aea3$category)
table(layers_aea3$iucn_sc_threat, layers_aea3$iucn_sc)


# Get list of spp
spp_df <- read.csv('output/ssea/mammal_spp_in_ssea_terrestrial.csv') 
# spp_df <- read.csv('output/ssea/mammal_spp_in_ssea_threat.csv') 
spp <- as.character(spp_df$x)

layers_aea3$binomial <- as.character(layers_aea3$binomial)

sppnotinssea <- character(nrow(layers_aea3)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea3[layers_aea3$binomial == spp[i], c('binomial', 'iucn_sc_threat')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], 'is not threatened.')    )  
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#run time: 35s
plot(m2)

writeRaster(m2, filename='output/ssea/biodiversity_rasters/m0_ssea_mammal_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)


# checking species
sppnotinssea2 <- sort(unique(sppnotinssea)) 
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #1202

sppinssea_threat <- setdiff(spp, sppnotinssea2) #629

length(spp)-length(sppnotinssea2) #629

write.csv(sppinssea_threat, 'output/ssea/mammal_spp_in_ssea_threat.csv', row.names=FALSE)


#### +Save as png (quick plots for checking) #####
mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_mammal_richness_DDpred.tif')
png('output/results/results_ssea/m0_ssea_mammal_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_mammal_vuln_all_DDpred.tif')
png('output/results/results_ssea/m0_ssea_mammal_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_mammal_vuln_threat_DDpred.tif')
png('output/results/results_ssea/m0_ssea_mammal_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()





# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# £££££ (4) MAKE REPTILE RASTERS £££££££----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
rm(list=setdiff(ls(), c("GISfolder", "referenceProj", "referenceExt", "referenceRes")))

#### * Load reptiles shpfile 
layers <- st_read(paste0(GISfolder, "Biodiversity Maps/REPTILES/REPTILES.shp")) #takes __s

# Change to LR/cd to NT, LR/lc to LC
str(layers)
layers[layers$category == "LR/cd", ]$category <- "NT"
layers[layers$category == "LR/lc", ]$category <- "LC"


layers2 <- layers %>% dplyr::filter(category != "EW", category != "EX") %>% dplyr::filter(terrestial == 'true')
#12769 layers

layers2 <- layers2 %>% dplyr::mutate(presence_richness = 1) 

# # Project shapefile
layers_aea <- st_transform(layers2, crs=as.character(referenceProj))  #25s

#plot(st_geometry(layers_aea))
#plot(layers_aea, max.plot=1)


### (a) Reptile richness ####
### IMPORTANT - need to return m0 to all zeroes before running the loop!!! 
str(layers_aea)

#List of spp
spp <- sort(unique(layers_aea$binomial)) #7028
#spp_df <- read.csv('output/ssea/reptile_spp_in_ssea_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in ssea so i can just use this to speed things up. 
#spp <- as.character(spp_df$x)

#layers_aea <- layers_aea %>% filter(binomial %in% spp)
layers_aea$binomial <- as.character(layers_aea$binomial)

sppnotinssea <- character(nrow(layers_aea)) #empty vector with nrows
m0 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0


system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea[layers_aea$binomial == spp[i], c('binomial', 'presence_richness')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='presence_richness', background=0) 
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m0 = sum(m0, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )  
      sppnotinssea[i] <- as.character(spp[i])  }
    
    cat("\n")
    #rm(geom, out)
  }
)
#552s
plot(m0)

writeRaster(m0, filename='output/ssea/biodiversity_rasters/m0_ssea_reptile_richness_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

#check species whose ranges are in ssea
sppnotinssea2 <- sort(unique(sppnotinssea))
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #4993

length(spp)-length(sppnotinssea2) #2035
sppinssea <- setdiff(spp, sppnotinssea2) #2035

write.csv(sppinssea, 'output/ssea/reptile_spp_in_ssea_terrestrial.csv', row.names=FALSE)


#### (b) Reptile vulnerability, tif #####
#in our data the average threat score for non-DD species was 5.06 (between NT and VU) for reptiles. Use DD=5

layers_aea2 <- layers_aea %>% dplyr::mutate(iucn_sc = recode(category, 'DD' = 5, 'LC'=2, 'NT'=4, 'VU'=8, 'EN'=16, 'CR'=32, .default = 0) ) 
table(layers_aea2$iucn_sc, layers_aea2$category)

# Get list of spp
spp_df <- read.csv('output/ssea/reptile_spp_in_ssea_terrestrial.csv') #after i've run the loop once, i'll have a shortened list of all amph that occur in ssea so i can just use this to speed things up.
spp <- as.character(spp_df$x)

#layers_aea2 <- layers_aea2 %>% filter(binomial %in% spp)
layers_aea2$binomial <- as.character(layers_aea2$binomial)

sppnotinssea <- character(nrow(layers_aea2)) #empty vector with nrows
m1 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea2[layers_aea2$binomial == spp[i], c('binomial', 'iucn_sc')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m1 = sum(m1, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], ' does not occur in SSEA.')    )
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#487s
plot(m1)
writeRaster(m1, filename='output/ssea/biodiversity_rasters/m0_ssea_reptile_vuln_all_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/ssea/biodiversity_rasters_before27apr2020/m0_ssea_reptile_vuln_all_DDpred.tif')
# all.equal(m1, m0b, tolerance=0) #TRUE
# rm(m0b)

# checking - sppnotinssea should be empty
sppnotinssea2 <- sort(unique(sppnotinssea)) #correct



#### (c) Reptile vulnerability, threatened spp only, tif ####
table(layers_aea2$iucn_sc, layers_aea2$category)

#If LC, NT, or 'non threatened=3'
layers_aea3 <- layers_aea2 %>% mutate(iucn_sc_threat = ifelse(iucn_sc<=4, 0, iucn_sc))

table(layers_aea3$iucn_sc_threat, layers_aea3$category)
table(layers_aea3$iucn_sc_threat, layers_aea3$iucn_sc)


# Get list of spp
spp_df <- read.csv('output/ssea/reptile_spp_in_ssea_terrestrial.csv') #first run
#spp_df <- read.csv('output/ssea/reptile_spp_in_ssea_threat.csv')  #all other runs
spp <- as.character(spp_df$x)

layers_aea3$binomial <- as.character(layers_aea3$binomial)

sppnotinssea <- character(nrow(layers_aea3)) #empty vector with nrows
m2 <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes, vals=0) #raster with values of 0

system.time(
  for(i in 1:length(spp)){
    
    output <- raster(crs=referenceProj, ext=referenceExt , resolution=referenceRes)
    
    geom <- layers_aea3[layers_aea3$binomial == spp[i], c('binomial', 'iucn_sc_threat')] #spp richness
    
    out <- fasterize(st_collection_extract(geom, "POLYGON"), output, field='iucn_sc_threat', background=0) #spp richness
    
    if(!all(values(out)==0)) { #if not all values in out == 0, then write the file
      
      print(geom)
      
      m2 = sum(m2, out) #sum the cumulative vulnerability score
      
    } else {  print(paste0(spp[i], 'is not threatened.')    )  
      
      sppnotinssea[i] <- as.character(spp[i])   }
    
    cat("\n")
    #rm(geom, out)
  }
)
#71s
plot(m2)

writeRaster(m2, filename='output/ssea/biodiversity_rasters/m0_ssea_reptile_vuln_threat_DDpred.tif', datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# m0b <- raster('output/ssea/biodiversity_rasters_before27apr2020/m0_ssea_reptile_vuln_threat_DDpred.tif')
# all.equal(m2, m0b, tolerance=0) #TRUE
# rm(m0b)


# Checking species
sppnotinssea2 <- sort(unique(sppnotinssea)) 
sppnotinssea2 <- sppnotinssea2[sppnotinssea2 != ""] #1489

length(spp)-length(sppnotinssea2) #546
sppinssea <- setdiff(spp, sppnotinssea2) #546

write.csv(sppinssea, 'output/ssea/reptile_spp_in_ssea_threat.csv', row.names=FALSE)


#### +Save as png (quick plots for checking) #####
mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_reptile_richness_DDpred.tif')
png('output/results/results_ssea/m0_ssea_reptile_richness_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_reptile_vuln_all_DDpred.tif')
png('output/results/results_ssea/m0_ssea_reptile_vuln_all_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()

mat_vuln <- raster('output/ssea/biodiversity_rasters/m0_ssea_reptile_vuln_threat_DDpred.tif')
png('output/results/results_ssea/m0_ssea_reptile_vuln_threat_DDpred.png', width=16, height=16, units='cm', res=300)
plot(mat_vuln)
dev.off()


