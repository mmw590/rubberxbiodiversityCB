#### rubberxbiodiversityCB 
#### 06-suit-expanded-rasters-figS2.R #### 

#### Rubber extended suitability niche 
rm(list=ls())

# Libraries
library(dplyr)
library(raster)
library(R.utils)
library(ncdf4)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot) 

# Set folder containing GIS input files
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'


#### Rubber extended suitability requirements --- 
# Dry stress. Minimum rainfall requirements:
# <= 5 months <60 mm rainfall month #BIOCLIM calculate from monthly prec
# >= 1200 mm rainfall y-1 #BIOCLIM 12
# >= 20 mm rainfall during driest quarter #BIOCLIM 17

# Cold stress / Min cold stress requirements:
# <=10 days frost y-1   # CRU frs
# Avg temp >=25C during wet season # BIOCLIM 8
# Temp seasonality <=50% higher than in humid tropics #BIOCLIM 4


### (1) <= 5 months <60 mm rainfall month  #####
prec.files <- list.files(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_prec/"), ".tif", full.names=TRUE) #119s
prec.mth <- stack(prec.files)

# Calculate no. of dry mths
system.time(dry.mth <- calc(prec.mth, fun=function(x){sum(x < 60)})) #40mins! #12.9kb
plot(dry.mth)
dry.mth
writeRaster(dry.mth, "output/dry.mth.60mm.tif" )

# Convert to binary (<= 5 dry months is suitable)
dry.mth <- raster("output/dry.mth.60mm.tif" )
system.time(prec.mth.bin <- calc(dry.mth, fun=function(r) {ifelse( r<=5, 1, 0) } )) #97s
plot(prec.mth.bin)
prec.mth.bin
system.time(writeRaster(prec.mth.bin, "output/extendedsuit_1prec.mth.bin.tif", dataype="INT1U" )) #96s

rm(prec.files, prec.mth, dry.mth)



### (2) >= 1200 mm rainfall y-1 ####
# BIO12 = Annual Precipitation
prec <-  raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif"))
plot(prec)

system.time(prec.bin <- calc(prec, fun=function(r) {ifelse( r>=1200, 1, 0) } )) #85s
plot(prec.bin)
system.time(writeRaster(prec.bin, "output/extendedsuit_2prec.min.bin.tif", datatype='INT1U', overwrite=TRUE ))

rm(prec)



### (3) >= 20 mm rainfall during driest quarter ####
# BIO17 = Precipitation of Driest Quarter
prec.dq <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_17.tif"))
plot(prec.dq)

system.time(prec.dq.bin <- calc(prec.dq, fun=function(r) {ifelse( r>=20, 1, 0) } )) #85s
plot(prec.dq.bin)
prec.dq.bin
system.time(writeRaster(prec.dq.bin, "output/extendedsuit_3prec.dq.bin.tif", datatype='INT1U', overwrite=TRUE )) #107s

rm(prec.dq)




### (4) <=10 days frost y-1 (CRU) ####
cru.files <- list.files(paste0(GISfolder, "ClimateData/CRU/"), ".gz", full.names=TRUE)

library(R.utils)
gunzip(cru.files[1], remove=FALSE)
gunzip(cru.files[2], remove=FALSE)
gunzip(cru.files[3], remove=FALSE)
gunzip(cru.files[4], remove=FALSE)

library(ncdf4)
cru.files <- list.files(paste0(GISfolder, "ClimateData/CRU/"), ".nc", full.names=TRUE)
frs <- lapply(cru.files, brick, varname="frs")
frs2 <- brick(frs)
frs2 <- subset(frs2, 109:nlayers(frs2)) #for year 1970 only
head(names(frs2), 30) #from 1970 (inclusive) to 2000

nyears = nlayers(frs2)/12 #31 years 1970-2000 inclusive 1970

frs.yr.sum.list <- list()

for (i in 1:nyears){ 
  frs.yr <- raster::subset(frs2,  ((i*12)+1-12):((i*12)) ) 
  frs.yr.sum <- sum(frs.yr)
  frs.yr.sum.list[[i]] <- frs.yr.sum
  names(frs.yr.sum.list)[i] <- paste0('yr', i+1969)
}
# 12s
# each pixel = sum of frost days for each year from 1970-2000.
# for each pixel average the sum of frost days over 31 years 
# pixels with <=10 frost days per year are suitable (#no. of frost days), with >10 frost days are 0 (not suitable)

frs.avg <- stack(frs.yr.sum.list)
frs.avg <- mean(frs.avg)

plot(frs.avg)
frs.avg
writeRaster(frs.avg, "output/frs.days.per.year.1970-2000.tif")

rm(cru.files, frs, frs2, nyears, frs.yr.sum.list, frs.yr, frs.yr.sum)

frs.bin <- calc(frs.avg, fun=function(r) {ifelse( r<=10, 1, 0) } ) #0s
plot(frs.bin)
frs.bin
system.time(writeRaster(frs.bin, "output/extendedsuit_4frs.days.tif", datatype='INT1U', overwrite=TRUE )) 

res(frs.bin)
rm(frs.avg)




### (5) Avg temp >=25C during wet season ####
# BIO8 = Mean Temperature of Wettest Quarter 
tavg.wq <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_8.tif"))
plot(tavg.wq)

system.time(tavg.wq.bin <- calc(tavg.wq, fun=function(r) {ifelse( r>=25, 1, 0) } )) #85s
plot(tavg.wq.bin)
tavg.wq.bin
system.time(writeRaster(tavg.wq.bin, "output/extendedsuit_5tavg.wq.bin.tif", datatype='INT1U', overwrite=TRUE )) #105s

rm(tavg.wq)




### (6) Temp seasonality <=50% higher temp seasonality in humid tropics = suitable ####
# BIO4 = Temperature Seasonality (standard deviation *100)
# Crop the Wordclim map to the tropics. latitudinal range = 23.5N and 23.5S
# Get the average of those cells. Multiply that by 1.5. 
# Then use that value to select the areas in the world that are below that value.

tsea <- raster(paste0(GISfolder, "ClimateData/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_4.tif"))
tsea.trop <- crop(tsea, extent(c(xmin=xmin(tsea), xmax=xmax(tsea), ymin=-23.5, ymax=23.5)))
plot(tsea.trop)
tsea.trop.mean <- cellStats(tsea.trop, stat='mean', na.rm=TRUE)
tsea.limit <- tsea.trop.mean*1.5 
#tsea.limit <- 348.393814053621

plot(tsea)

system.time(tsea.bin <- calc(tsea, fun=function(r) {ifelse( r<=tsea.limit, 1, 0) } )) #85s
plot(tsea.bin)
tsea.bin
system.time(writeRaster(tsea.bin, "output/extendedsuit_6tsea.bin.tif", datatype='INT1U', overwrite=TRUE )) #107s

rm(tsea.trop, tsea.trop.mean, tsea.limit)
rm(tsea)




##### Combine dry stress layers ####
suit.files <- list.files("output/", ".tif", full.names=TRUE)
suit.files <- grep("extendedsuit_", suit.files, value=TRUE)

drystack <- stack(suit.files[1:3])

system.time(drystress <- sum(drystack) ) #359s add rasters, so it's CONDITION OR CONDITION OR CONDITION
plot(drystress)

# Crop to Afr and SSEA

#extents in WGS (Afr)
xmin = -25.5 #W
xmax =  63.5 #E
ymin = -35 #S 
ymax =  19 #N

cropExtent_wgs_afr <- extent(c(xmin-5,xmax+5,ymin-5,ymax+5))

#extents in WGS (SSEA)
xmin =  69.25
xmax =  158.4 
ymin = -12.5 
ymax =  30.5

cropExtent_wgs_ssea <- extent(c(xmin-5,xmax+5,ymin-5,ymax+5))

rm(xmin, xmax, ymin, ymax)

drystress.afr <- crop(drystress, cropExtent_wgs_afr)
drystress.ssea <- crop(drystress, cropExtent_wgs_ssea)

# Project to ref grid
#afr
reference <- raster('output/afr/various_rasters/africa.tif') 
output <- raster(crs=as.character(crs(reference)), ext=extent(reference) , resolution=res(reference))
drystress.afr.aea <- projectRaster(drystress.afr, output, method='ngb', filename='output/afr/various_rasters/extendedsuit_drystress_afr.tif', overwrite=TRUE) #6s
plot(drystress.afr.aea)

png('output/results/results_afr/suitability_drystress_bin_afr.png', width=16, height=16, units='cm', res=300)
plot(drystress.afr.aea)
dev.off()


#ssea
reference <- raster('output/ssea/various_rasters/ssea.tif')
output <- raster(crs=as.character(crs(reference)), ext=extent(reference) , resolution=res(reference))
drystress.ssea.aea <- projectRaster(drystress.ssea, output, method='ngb', filename='output/ssea/various_rasters/extendedsuit_drystress_ssea.tif', overwrite=TRUE) #6s
plot(drystress.ssea.aea)

png('output/results/results_ssea/suitability_drystress_bin_ssea.png', width=16, height=16, units='cm', res=300)
plot(drystress.ssea.aea)
dev.off()


##### Combine cold stress layers ####
# CRU (frs days) is different res than WorldClim layers, cannot stack.
# Crop and project to aea separately, then sum. (to minimise transforming)
frs.bin <- raster(suit.files[4])
coldstack <- stack(suit.files[5:6])

### Afr
# Crop
frs.bin.afr <- crop(frs.bin, cropExtent_wgs_afr)
coldstack.afr <- crop(coldstack, cropExtent_wgs_afr)

# Project to ref grid
reference <- raster('output/afr/various_rasters/africa.tif') 
output <- raster(crs=as.character(crs(reference)), ext=extent(reference) , resolution=res(reference))

frs.bin.afr.aea <- projectRaster(frs.bin.afr, output, method='ngb') #6s
coldstack.afr.aea <- projectRaster(coldstack.afr, output, method='ngb') #6s

# Sum rasters
coldstress.afr.aea <- sum(frs.bin.afr.aea, coldstack.afr.aea) #add rasters, so it's CONDITION OR CONDITION OR CONDITION
plot(coldstress.afr.aea)
writeRaster(coldstress.afr.aea, 'output/afr/various_rasters/extendedsuit_coldstress_afr.tif')

png('output/results/results_afr/suitability_coldstress_afr.png', width=16, height=16, units='cm', res=300)
plot(coldstress.afr.aea)
dev.off()



### SSEA
# Crop 
frs.bin.ssea <- crop(frs.bin, cropExtent_wgs_ssea)
coldstack.ssea <- crop(coldstack, cropExtent_wgs_ssea)

# Project to ref grid
reference <- raster('output/ssea/various_rasters/ssea.tif')
output <- raster(crs=as.character(crs(reference)), ext=extent(reference) , resolution=res(reference))

frs.bin.ssea.aea <- projectRaster(frs.bin.ssea, output, method='ngb') #6s
coldstack.ssea.aea <- projectRaster(coldstack.ssea, output, method='ngb') #6s

# Sum rasters
coldstress.ssea.aea <- sum(frs.bin.ssea.aea, coldstack.ssea.aea) #add rasters, so it's CONDITION OR CONDITION OR CONDITION
plot(coldstress.ssea.aea)
writeRaster(coldstress.ssea.aea, 'output/ssea/various_rasters/extendedsuit_coldstress_ssea.tif')

png('output/results/results_ssea/suitability_coldstress_ssea.png', width=16, height=16, units='cm', res=300)
plot(coldstress.ssea.aea)
dev.off()



##### Compare with existing suitability #####
rm(list=setdiff(ls(), c('GISfolder', 'drystress.afr.aea', 'coldstress.afr.aea', 'drystress.ssea.aea', 'coldstress.ssea.aea')))


#### Africa ####
drystress.afr <- raster('output/afr/various_rasters/extendedsuit_drystress_afr.tif')
coldstress.afr <- raster('output/afr/various_rasters/extendedsuit_coldstress_afr.tif')

####### +Load LU mask (CCI) 
mat_land_use <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use <- subs(mat_land_use, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

rm(reclassify.df)


### +Load Suitability, mask out LU 
mat_suit_afr <- raster('output/afr/various_rasters/suitability_afr.tif')
mat_suit_afr_mask <- mask(mat_suit_afr, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit_afr_mask <- mask(mat_suit_afr_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(mat_suit_afr_mask)

drystress.afr_mask <- mask(drystress.afr, mat_land_use, maskvalue=NA, updatevalue=NA)
drystress.afr_mask <- mask(drystress.afr_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(drystress.afr)
plot(drystress.afr_mask)

coldstress.afr_mask <- mask(coldstress.afr, mat_land_use, maskvalue=NA, updatevalue=NA)
coldstress.afr_mask <- mask(coldstress.afr_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(coldstress.afr)
plot(coldstress.afr_mask)


##### Combine dry and cold stress (WET AND WARM) - Afr ####

# Reclassify into 0-6
# Addition, then mask out 0s in either
dry.cold.stress.afr <- drystress.afr + coldstress.afr
freq(dry.cold.stress.afr)

dry.cold.stress.afr <- mask(dry.cold.stress.afr, drystress.afr, maskvalue=0, updatevalue=0)
freq(dry.cold.stress.afr)

dry.cold.stress.afr <- mask(dry.cold.stress.afr, coldstress.afr, maskvalue=0, updatevalue=0)
freq(dry.cold.stress.afr)

plot(dry.cold.stress.afr)
writeRaster(dry.cold.stress.afr, 'output/afr/various_rasters/extendedsuit_afr_0-6.tif',  overwrite=TRUE)

#### Africa ####
dry.cold.stress.afr <- raster('output/afr/various_rasters/extendedsuit_afr_0-6.tif')

### Mask landuse from suit ext ####
dry.cold.stress.afr_mask <- mask(dry.cold.stress.afr, mat_land_use, maskvalue=NA, updatevalue=NA)
dry.cold.stress.afr_mask <- mask(dry.cold.stress.afr_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(dry.cold.stress.afr_mask)



#Transform to binary
dry.cold.stress.afr.bin <- calc(dry.cold.stress.afr, fun=function(r){ifelse(r>0, 1, 0)}) 

dry.cold.stress.afr.bin_mask <- mask(dry.cold.stress.afr.bin, mat_land_use, maskvalue=NA, updatevalue=NA)
dry.cold.stress.afr.bin_mask <- mask(dry.cold.stress.afr.bin_mask, mat_land_use, maskvalue=0, updatevalue=0)

writeRaster(dry.cold.stress.afr.bin_mask, 'output/afr/various_rasters/extendedsuit_afr_bin_mask.tif')


# Compare layers (use masked map)
suit.compare <- as.data.frame( stack(mat_suit_afr_mask, dry.cold.stress.afr_mask) )  %>% 
  rename(suit = suitability_afr, suit.expand = layer) %>% 
  dplyr::filter(is.na(.$suit)==FALSE & is.na(.$suit.expand)==FALSE)

plot(suit.compare$suit.expand ~ suit.compare$suit, ylim=c(0,0.001))
plot(suit.compare$suit.expand ~ suit.compare$suit)

check <- suit.compare %>% filter(suit <= 0.4 & suit.expand > 0) #77016 cells or 770.16 Mha that is poor suitability 0< suit ≤0.4 that falls under suitable in the expanded niche
table(check$suit.expand)
plot(check$suit.expand ~ check$suit)
hist(check$suit, breaks=100)

# areas that are minimally suitable (>0) in Ahrends not in extended map 
check0 <- suit.compare %>% filter(suit > 0 & suit.expand==0)  #78237 minimally suitable 
plot( jitter(check0$suit.expand) ~ check0$suit ) 
range(check0$suit) #0-0.125 



#### SSEA ####
drystress.ssea <- raster('output/ssea/various_rasters/extendedsuit_drystress_ssea.tif')
coldstress.ssea <- raster('output/ssea/various_rasters/extendedsuit_coldstress_ssea.tif')

####### +Load LU mask (CCI) 
mat_land_use <- raster('output/ssea/various_rasters/land_use_ssea.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use <- subs(mat_land_use, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

rm(reclassify.df)

### +Load Suitability, mask out LU 
mat_suit_ssea <- raster('output/ssea/various_rasters/suitability_ssea.tif')
mat_suit_ssea_mask <- mask(mat_suit_ssea, mat_land_use, maskvalue=NA, updatevalue=NA)
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(mat_suit_ssea_mask)

drystress.ssea_mask <- mask(drystress.ssea, mat_land_use, maskvalue=NA, updatevalue=NA)
drystress.ssea_mask <- mask(drystress.ssea_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(drystress.ssea_mask)

coldstress.ssea_mask <- mask(coldstress.ssea, mat_land_use, maskvalue=NA, updatevalue=NA)
coldstress.ssea_mask <- mask(coldstress.ssea_mask, mat_land_use, maskvalue=0, updatevalue=0)
plot(coldstress.ssea_mask)


##### Combine dry and cold stress (WET AND WARM) - SSEA ####

# Reclassify into 0-6
# Addition, then mask out 0s in either
dry.cold.stress.ssea <- drystress.ssea + coldstress.ssea
freq(dry.cold.stress.ssea)

dry.cold.stress.ssea <- mask(dry.cold.stress.ssea, drystress.ssea, maskvalue=0, updatevalue=0)
freq(dry.cold.stress.ssea)

dry.cold.stress.ssea <- mask(dry.cold.stress.ssea, coldstress.ssea, maskvalue=0, updatevalue=0)
freq(dry.cold.stress.ssea)

plot(dry.cold.stress.ssea)
writeRaster(dry.cold.stress.ssea, 'output/ssea/various_rasters/extendedsuit_ssea_0-6.tif',  overwrite=TRUE)

### Mask landuse from suit ext ####
dry.cold.stress.ssea_mask <- mask(dry.cold.stress.ssea, mat_land_use, maskvalue=NA, updatevalue=NA)
dry.cold.stress.ssea_mask <- mask(dry.cold.stress.ssea_mask, mat_land_use, maskvalue=0, updatevalue=0)


#Transform to binary
dry.cold.stress.ssea.bin <- calc(dry.cold.stress.ssea, fun=function(r){ifelse(r>0, 1, 0)}) 

dry.cold.stress.ssea.bin_mask <- mask(dry.cold.stress.ssea.bin, mat_land_use, maskvalue=NA, updatevalue=NA)
dry.cold.stress.ssea.bin_mask <- mask(dry.cold.stress.ssea.bin_mask, mat_land_use, maskvalue=0, updatevalue=0)


# Compare layers (use masked map)
suit.compare.ssea <- as.data.frame( stack(mat_suit_ssea_mask, dry.cold.stress.ssea_mask) )  %>% 
  rename(suit = suitability_ssea, suit.expand = layer) %>% 
  dplyr::filter(is.na(.$suit)==FALSE & is.na(.$suit.expand)==FALSE)


plot(suit.compare.ssea$suit.expand ~ suit.compare.ssea$suit, ylim=c(0,0.001))
plot(suit.compare.ssea$suit.expand ~ suit.compare.ssea$suit)

check.ssea <- suit.compare.ssea %>% filter(suit <= 0.4 & suit.expand > 0) #45695 cells or 456.95 Mha that is poor suitability 0< suit ≤0.4 that falls under suitable in the expanded niche
table(check.ssea$suit.expand)
plot(check.ssea$suit.expand ~ check.ssea$suit)
hist(check.ssea$suit, breaks=100)

# areas that are minimally suitable (>0) in Ahrends not in extended map 
check0.ssea <- suit.compare.ssea %>% filter(suit > 0 & suit.expand==0)  #38200 minimally suitable 
plot( jitter(check0.ssea$suit.expand) ~ check0.ssea$suit ) 
range(check0.ssea$suit) #0-0.04





#### FIGURE FOR PAPER #####
dry.cold.stress.afr <- raster('output/afr/various_rasters/extendedsuit_afr_0-6.tif')
dry.cold.stress.ssea <- raster('output/ssea/various_rasters/extendedsuit_ssea_0-6.tif')

library(sf)

#Transform to latlon for plotting & load afr/ssea  country polygons
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
afr_countries_wgs <- st_crop(gadm_0, refExtent_wgs) #to include Middle East polygons
output_latlon_afr <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_afr_mask)[1], ncols=dim(mat_suit_afr_mask)[2])

refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
ssea_countries_wgs <- st_crop(gadm_0, refExtent_wgs)
output_latlon_ssea <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_ssea_mask)[1], ncols=dim(mat_suit_ssea_mask)[2])



## Plot A) Dry cold stress Afr ####
map_latlon <- projectRaster(dry.cold.stress.afr_mask, output_latlon_afr, method='ngb')
res_spdf <- rasterToPoints(map_latlon)
res_df <- as.data.frame(res_spdf)
colnames(res_df)[3] <- 'zone'
res_df$zone <- as.character(res_df$zone)

gg_suit_ext_afr <- ggplot() +
  geom_tile(data=res_df, aes(y=y, x=x, fill=zone)) +
  #scale_fill_manual(values=c("gray90", "lightgoldenrod", "yellow",  "darkolivegreen1", "palegreen4", "darkgreen" )) +
  scale_fill_viridis(discrete=TRUE, option="rocket", direction=-1) +
  geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
  coord_sf(datum = NA)  +
  theme(
    plot.margin = unit(c(0,0,0,0),"cm"),
    rect = element_blank(),
    text = element_text(size=8),
    legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm")) +
  labs(x = NULL, y=NULL, fill=element_text("no. of\ndry/cold\nstress\nconditions"))



## Plot C) Suitability Ahrends Afr ####
map_latlon <- projectRaster(mat_suit_afr_mask, output_latlon_afr, method='bilinear')
res_spdf <- rasterToPoints(map_latlon)
res_df2 <- as.data.frame(res_spdf)
colnames(res_df2)[3] <- 'suitability'

?scale_fill_viridis

gg_suit_afr <- ggplot() +
  geom_tile(data=res_df2, aes(y=y, x=x, fill=suitability)) +
  #scale_fill_gradientn(colors=rev(terrain.colors(255)), breaks=seq(0,1,by=0.5), labels=c(0,0.5,1), limits=c(0,1)) +
  #scale_fill_gradientn(colors=rev(terrain.colors(255)), breaks=seq(0,1,by=0.2), limits=c(0,1)) +
  scale_fill_viridis(breaks=seq(0,1,by=0.2), limits=c(0,1), direction=-1) +
  geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
  coord_sf(datum = NA)  +
  theme(
    plot.margin = unit(c(t=0,b=0,l=0,r=0),"cm"),
    rect = element_blank(),
    text = element_text(size=8),
    legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm") ) +#,
  #legend.position="none" )
  labs(x = NULL, y=NULL, fill=element_text("suitability\nindex")) 
    


## Plot B) Dry cold stress SSEA ####
map_latlon <- projectRaster(dry.cold.stress.ssea_mask, output_latlon_ssea, method='ngb')
res_spdf <- rasterToPoints(map_latlon)
res_df3 <- as.data.frame(res_spdf)
colnames(res_df3)[3] <- 'zone'
res_df3$zone <- as.character(res_df3$zone)

gg_suit_ext_ssea <- ggplot() +
  geom_tile(data=res_df3, aes(y=y, x=x, fill=zone)) +
  #scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","green")) +
  scale_fill_viridis(discrete=TRUE, option="rocket", direction=-1) +
  geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
  coord_sf(datum = NA)  +
  theme(
    plot.margin = unit(c(0,0,0,0),"cm"),
    rect = element_blank(),
    legend.position="none" ) +
  labs(x = NULL, y=NULL, fill=NULL) 


## Plot D) Suitability Ahrends SSEA ####
map_latlon <- projectRaster(mat_suit_ssea_mask, output_latlon_ssea, method='bilinear')
res_spdf <- rasterToPoints(map_latlon)
res_df4 <- as.data.frame(res_spdf)
colnames(res_df4)[3] <- 'suitability'

gg_suit_ssea <- ggplot() +
  geom_tile(data=res_df4, aes(y=y, x=x, fill=suitability)) +
  scale_fill_viridis(breaks=seq(0,1,by=0.2), limits=c(0,1), direction=-1) +
  geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
  coord_sf(datum = NA)  +
  theme(
    plot.margin = unit(c(t=0,b=0,l=0,r=0),"cm"),
    rect = element_blank(),
    legend.position="none" ) +
  labs(x = NULL, y=NULL, fill=NULL) 





#### Fig S2 Combined 2x2 plot ####


fig_suit_ext_compare <- plot_grid(gg_suit_ext_afr, gg_suit_ext_ssea, gg_suit_afr, gg_suit_ssea, 
                           labels = c('A', 'B', 'C', 'D'), ncol=2, label_size=8) 

cowplot::save_plot("output/results/CBcolorblind/figS2.png", fig_suit_ext_compare, base_height=4, base_width=6.85) #
??plot_grid
