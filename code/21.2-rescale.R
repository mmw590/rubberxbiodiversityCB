####### >>>> 21.2-rescale-bivmaps.R <<<<< ##########
rm(list=ls())

# Run this after both 01-areasofcompromise_AFR.R and 02-areasofcompromise_SSEA.R

library(sf)
library(raster)
library(dplyr)


#####################################################################################
###### Standardize scaling for Asia and Africa  #####

#### (A) Africa - Load rasters #####
rlist <- list.files('output/afr/biodiversity_rasters_cci/', full.names = TRUE)
rlist <- grep(rlist, pattern='mask', inv=TRUE, value=TRUE) #excld 'combspp'
rlist <- grep(rlist, pattern='tif$', value=TRUE) 

rlistread <- sapply(rlist, stack)
rstack <- stack(rlistread)

rstacknames <- sub("output/afr/biodiversity_rasters_cci/", "" , rlist)
rstacknames <- sub("DDpred", "std" , rstacknames)
names(rstack) <- rstacknames

#CCI LU
mat_land_use_afr <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use_afr <- subs(mat_land_use_afr, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)
rm(reclassify.df)

#Suitability
mat_suit_afr <- raster('output/afr/various_rasters/suitability_afr.tif')
mat_suit_afr <- mask(mat_suit_afr, mat_land_use_afr, maskvalue=NA, updatevalue=NA)
#mat_suit_afr <- mask(mat_suit_afr, mat_land_use_afr, maskvalue=0, updatevalue=0) #later, for other figs

rstack_maskNA_afr <- mask(rstack, mat_suit_afr, maskvalue=NA, updatevalue=NA)

plot(rstack_maskNA_afr)


#### (B) SSEA - Load rasters #####
rlist <- list.files('output/ssea/biodiversity_rasters/', full.names = TRUE)
rlist <- grep(rlist, pattern='mask', inv=TRUE, value=TRUE) #excld 'combspp'
rlist <- grep(rlist, pattern='tif$', value=TRUE)  

rlistread <- sapply(rlist, stack)
rstack <- stack(rlistread)

rstacknames <- sub("output/ssea/biodiversity_rasters/", "" , rlist)
rstacknames <- sub("DDpred", "std" , rstacknames)
names(rstack) <- rstacknames

mat_land_use_ssea <- raster('output/ssea/various_rasters/land_use_ssea.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use_ssea <- subs(mat_land_use_ssea, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)
rm(reclassify.df)

mat_suit_ssea <- raster('output/ssea/various_rasters/suitability_ssea.tif')
mat_suit_ssea <- mask(mat_suit_ssea, mat_land_use_ssea, maskvalue=NA, updatevalue=NA)
#mat_suit_ssea <- mask(mat_suit_ssea, mat_land_use_ssea, maskvalue=0, updatevalue=0) #later, for other figs

rstack_maskNA_ssea <- mask(rstack, mat_suit_ssea, maskvalue=NA, updatevalue=NA)

plot(rstack_maskNA_ssea)


#### Standardizing across Africa and Asia #### 
# note: have validated, doesn't change whether or not i mask out unsuitable landuses from suitability map
# note: set minimum to 0. Only bird richness and vulnA has a minValue >0 (19 and 40). But we don't really want to say areas with 19 birds should be 0 in biodiversity value...

rmaxValue_forscale <- pmax(maxValue(rstack_maskNA_afr), maxValue(rstack_maskNA_ssea))
rmaxValue_forscale <- c(rmaxValue_forscale, rmaxValue_forscale) #do this so 12 vals for afr and 12 vals for ssea (but same values)
rminValue_forscale <- 0

rasterRescaleStd <- function(r, i){
  ((r-rminValue_forscale)/(rmaxValue_forscale[i]-rminValue_forscale))
}

rlist_maskNA <- c(as.list(rstack_maskNA_afr), as.list(rstack_maskNA_ssea))
rescaledStdlist <- list()

for(i in 1:length(rlist_maskNA)){
  rescaledStdlist[[i]] <- rasterRescaleStd(rlist_maskNA[[i]], i)
}


rescaledStdlist #this is the list that has been rescaled-standard across Asia and Africa, masked out water NAs. 

rescaledstack_afr <- stack(rescaledStdlist[1:12])
rescaledstack_ssea <- stack(rescaledStdlist[13:24])



###### + Comb spp (richness) #####

### afr
richness_stack_afr <- raster::subset(rescaledstack_afr, grep('richness', names(rescaledstack_afr), value=TRUE))
plot(richness_stack_afr)

richness_comb_afr <- mean(richness_stack_afr)
plot(richness_comb_afr)
names(richness_comb_afr) <- "m0_afr_combspp_richness"

range_richness_rescaled <- rbind(
  range(values(richness_comb_afr), na.rm=TRUE), 
  range(values(richness_stack_afr[[1]]), na.rm=TRUE),
  range(values(richness_stack_afr[[2]]), na.rm=TRUE),
  range(values(richness_stack_afr[[3]]), na.rm=TRUE), 
  range(values(richness_stack_afr[[4]]), na.rm=TRUE) )

print(range_richness_rescaled)
# [1,] 0.01310863 0.6371439
# [2,] 0.00000000 1.0000000
# [3,] 0.02694611 1.0000000
# [4,] 0.00000000 1.0000000
# [5,] 0.00000000 0.6428571


### ssea
richness_stack_ssea <- raster::subset(rescaledstack_ssea, grep('richness', names(rescaledstack_ssea), value=TRUE))
plot(richness_stack_ssea)

richness_comb_ssea <- mean(richness_stack_ssea)
plot(richness_comb_ssea)
names(richness_comb_ssea) <- "m0_ssea_combspp_richness"

range_richness_rescaled <- rbind(
  range(values(richness_comb_ssea), na.rm=TRUE), 
  range(values(richness_stack_ssea[[1]]), na.rm=TRUE),  
  range(values(richness_stack_ssea[[2]]), na.rm=TRUE),  
  range(values(richness_stack_ssea[[3]]), na.rm=TRUE),  
  range(values(richness_stack_ssea[[4]]), na.rm=TRUE) ) 

print(range_richness_rescaled)
# [1,] 0.01905118 0.7711681
# [2,] 0.00000000 0.8131868
# [3,] 0.05089820 0.8607784
# [4,] 0.00000000 0.8750000
# [5,] 0.01587302 1.0000000

# 27apr2020
# [1,] 0.01407304 0.7711681
# [2,] 0.00000000 0.8131868
# [3,] 0.04041916 0.8607784
# [4,] 0.00000000 0.8750000
# [5,] 0.01587302 1.0000000

### Getting the maxValue of Afr and Asia for standardized second rescale of combspp to 0-1
rmaxValue_forscale <- pmax(max(values(richness_comb_afr), na.rm=TRUE), max(values(richness_comb_ssea), na.rm=TRUE))
rminValue_forscale <- 0 #scale to min biodivesity Value to 0 as per before

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

richness_comb_rescale_afr <- rasterRescaleStd_combspp(richness_comb_afr)
plot(richness_comb_rescale_afr)
names(richness_comb_rescale_afr) <- "rescaled_twice_afr_combspp_richness"
range(values(richness_comb_rescale_afr), na.rm=TRUE) #[1]  0.01699841 0.82620625

richness_comb_rescale_ssea <- rasterRescaleStd_combspp(richness_comb_ssea)
plot(richness_comb_rescale_ssea)
names(richness_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_richness"
range(values(richness_comb_rescale_ssea), na.rm=TRUE) #[1] 0.02470432 1.00000000 / 27apr2020 [1] 0.018249 1.000000




#### + Comb spp (vuln all) #####
vuln_stack_afr <- raster::subset(rescaledstack_afr, grep('vuln_all', names(rescaledstack_afr), value=TRUE))
plot(vuln_stack_afr)

vuln_comb_afr <- mean(vuln_stack_afr)
plot(vuln_comb_afr)
names(vuln_comb_afr) <- "m0_afr_combspp_vuln_all"

range_vuln_rescaled <- rbind(
  range(values(vuln_comb_afr), na.rm=TRUE), 
  range(values(vuln_stack_afr[[1]]), na.rm=TRUE),  
  range(values(vuln_stack_afr[[2]]), na.rm=TRUE),  
  range(values(vuln_stack_afr[[3]]), na.rm=TRUE), 
  range(values(vuln_stack_afr[[4]]), na.rm=TRUE) ) 

print(range_vuln_rescaled)
# [1,] 0.02114809 0.6367656
# [2,] 0.00000000 1.0000000
# [3,] 0.02294686 1.0000000
# [4,] 0.00000000 0.7664835
# [5,] 0.00000000 1.0000000


### ssea
vuln_stack_ssea <- raster::subset(rescaledstack_ssea, grep('vuln_all', names(rescaledstack_ssea), value=TRUE))
plot(vuln_stack_ssea)

vuln_comb_ssea <- mean(vuln_stack_ssea)
plot(vuln_comb_ssea)
names(vuln_comb_ssea) <- "m0_ssea_combspp_vuln_all"

range_vuln_rescaled <- rbind(
  range(values(vuln_comb_ssea), na.rm=TRUE), 
  range(values(vuln_stack_ssea[[1]]), na.rm=TRUE),  
  range(values(vuln_stack_ssea[[2]]), na.rm=TRUE),  
  range(values(vuln_stack_ssea[[3]]), na.rm=TRUE), 
  range(values(vuln_stack_ssea[[4]]), na.rm=TRUE) )

print(range_vuln_rescaled)
# [1,] 0.03086301 0.7326687
# [2,] 0.00000000 0.6517413
# [3,] 0.09299517 0.9685990
# [4,] 0.00000000 1.0000000
# [5,] 0.03045685 0.9619289

# 27apr2020 
# [1,] 0.02935334 0.7339125
# [2,] 0.00000000 0.6567164
# [3,] 0.08333333 0.9685990
# [4,] 0.00000000 1.0000000
# [5,] 0.03045685 0.9619289


# Getting the min and maxValue of Afr and Asia for standardized second rescale of combspp
(rmaxValue_forscale <- pmax(maxValue(vuln_comb_afr), maxValue(vuln_comb_ssea)) ) #0.7326687
rminValue_forscale <- 0

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

vuln_comb_rescale_afr <- rasterRescaleStd_combspp(vuln_comb_afr)
plot(vuln_comb_rescale_afr)
names(vuln_comb_rescale_afr) <- "rescaled_twice_afr_combspp_vuln_all"
range(values(vuln_comb_rescale_afr), na.rm=TRUE) #0.02886447 0.86910440 / 27apr2020 [1] 0.02881555 0.86763150

vuln_comb_rescale_ssea <- rasterRescaleStd_combspp(vuln_comb_ssea)
plot(vuln_comb_rescale_ssea)
names(vuln_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_vuln_all"
range(values(vuln_comb_rescale_ssea), na.rm=TRUE) #0.04212409 1.00000000 / 27apr2020 [1] 0.0399957 1.0000000




###### + Comb spp (vuln threat) #####
vulnT_stack_afr <- raster::subset(rescaledstack_afr, grep('vuln_threat', names(rescaledstack_afr), value=TRUE))
plot(vulnT_stack_afr)

vulnT_comb_afr <- mean(vulnT_stack_afr)
plot(vulnT_comb_afr)
names(vulnT_comb_afr) <- "m0_afr_combspp_vuln_threat"

range_vulnT_rescaled <- rbind(
  range(values(vulnT_comb_afr), na.rm=TRUE), 
  range(values(vulnT_stack_afr[[1]]), na.rm=TRUE), 
  range(values(vulnT_stack_afr[[2]]), na.rm=TRUE), 
  range(values(vulnT_stack_afr[[3]]), na.rm=TRUE), 
  range(values(vulnT_stack_afr[[4]]), na.rm=TRUE) ) 

print(range_vulnT_rescaled)
# [1,] 0.01209232 0.6217346
# [2,] 0.00000000 1.0000000
# [3,] 0.00000000 0.8358209
# [4,] 0.00000000 0.6666667
# [5,] 0.00000000 1.0000000

#27apr2020
# [1,] 0.01187601 0.6179492
# [2,] 0.00000000 1.0000000
# [3,] 0.00000000 0.8115942
# [4,] 0.00000000 0.6666667
# [5,] 0.00000000 1.0000000

#ssea
#vulnT_stack_ssea <- raster::subset(rescaledstack_ssea, grep('vuln_threat', names(rescaledstack_ssea), value=TRUE))
vulnT_stack_ssea <- raster::subset(rescaledstack_ssea, seq(3,12,by=3))
plot(vulnT_stack_ssea)

vulnT_comb_ssea <- mean(vulnT_stack_ssea)
plot(vulnT_comb_ssea)
names(vulnT_comb_ssea) <- "m0_ssea_combspp_vuln_threat"

range_vulnT_rescaled <- rbind(
  range(values(vulnT_comb_ssea), na.rm=TRUE), 
  range(values(vulnT_stack_ssea[[1]]), na.rm=TRUE),  
  range(values(vulnT_stack_ssea[[2]]), na.rm=TRUE),
  range(values(vulnT_stack_ssea[[3]]), na.rm=TRUE), 
  range(values(vulnT_stack_ssea[[4]]), na.rm=TRUE) )

print(range_vulnT_rescaled)
# [1,] 0.05191898 0.5622601
# [2,] 0.00000000 0.6944444
# [3,] 0.13432836 1.0000000
# [4,] 0.00000000 1.0000000
# [5,] 0.02857143 0.8928571

#27apr2020
# [1,] 0.05062112 0.5656473
# [2,] 0.00000000 0.6944444
# [3,] 0.14492754 1.0000000
# [4,] 0.00000000 1.0000000
# [5,] 0.02857143 0.8928571

# Getting the maxValue of combspp Afr and Asia for standardized second rescale of combspp
(rmaxValue_forscale <- pmax(maxValue(vulnT_comb_afr), maxValue(vulnT_comb_ssea))) #0.6217346
rminValue_forscale <- 0

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

vulnT_comb_rescale_afr <- rasterRescaleStd_combspp(vulnT_comb_afr)
plot(vulnT_comb_rescale_afr)
names(vulnT_comb_rescale_afr) <- "rescaled_twice_afr_combspp_vuln_threat"
range(values(vulnT_comb_rescale_afr), na.rm=TRUE) #0.01944932 1.00000000 / 27apr2020 [1] 0.01921842 1.00000000

vulnT_comb_rescale_ssea <- rasterRescaleStd_combspp(vulnT_comb_ssea)
plot(vulnT_comb_rescale_ssea)
names(vulnT_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_vuln_threat"
range(values(vulnT_comb_rescale_ssea), na.rm=TRUE) #0.08350666 0.90434109 / 27apr2020  [1] 0.08191793 0.91536217




##### Mask out unsuitable LUs, PAs #####
rescaledstack_afr_std <- stack(rescaledstack_afr, richness_comb_rescale_afr, vuln_comb_rescale_afr, vulnT_comb_rescale_afr)
rescaledstack_ssea_std <- stack(rescaledstack_ssea, richness_comb_rescale_ssea, vuln_comb_rescale_ssea, vulnT_comb_rescale_ssea)


# + masking LUs, PAs for Afr ####
mat_suit_afr <- mask(mat_suit_afr, mat_land_use_afr, maskvalue=0, updatevalue=0) #

mat_pa <- raster('output/afr/various_rasters/protected_areas_afr.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_afr, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_afr, maskvalue=0, updatevalue=NA)

rescaledstack_afr_std_mask <- raster::mask(rescaledstack_afr_std, mat_suit_afr, maskvalue=0, updatevalue=NA)
rescaledstack_afr_std_mask <- raster::mask(rescaledstack_afr_std_mask, mat_pa2, maskvalue=1, updatevalue=NA)

plot(rescaledstack_afr_std_mask[[1]])


# + masking LUs, PAs for SSEA ####
mat_suit_ssea <- mask(mat_suit_ssea, mat_land_use_ssea, maskvalue=0, updatevalue=0) #

mat_pa <- raster('output/ssea/various_rasters/protected_areas_ssea.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_ssea, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_ssea, maskvalue=0, updatevalue=NA)

mat_conc <- raster('output/ssea/various_rasters/concessions_ssea.tif') 
mat_conc2 <- mask(mat_conc, mat_conc, maskvalue=0, updatevalue=NA)

rescaledstack_ssea_std_mask <- mask(rescaledstack_ssea_std, mat_suit_ssea, maskvalue=0, updatevalue=NA)
rescaledstack_ssea_std_mask <- mask(rescaledstack_ssea_std_mask, mat_pa2, maskvalue=1, updatevalue=NA)
rescaledstack_ssea_std_mask <- mask(rescaledstack_ssea_std_mask, mat_conc2, maskvalue=1, updatevalue=NA)

plot(rescaledstack_ssea_std_mask[[1]])


##### + Write the std_masked rescaled rasters ####
rescaled_raster_filenames <- names(rescaledstack_afr_std_mask)
rescaled_raster_filenames<- sub("_std.tif", "", rescaled_raster_filenames)
rescaled_raster_filenames <- paste0(rescaled_raster_filenames, '_std_mask')

writeRaster(rescaledstack_afr_std_mask, filename= paste0('./output/afr/biodiversity_rasters_cci/', rescaled_raster_filenames, '.tif'), bylayer=TRUE, format='GTiff', overwrite=TRUE)


rescaled_raster_filenames <- names(rescaledstack_ssea_std_mask)
rescaled_raster_filenames<- sub("_std.tif", "", rescaled_raster_filenames)
rescaled_raster_filenames <- paste0(rescaled_raster_filenames, '_std_mask')

writeRaster(rescaledstack_ssea_std_mask, filename= paste0('./output/ssea/biodiversity_rasters/', rescaled_raster_filenames, '.tif'), bylayer=TRUE, format='GTiff', overwrite=TRUE)



##### Number of threatened spp in each taxonomic class ####
rlist <- list.files('./output/ssea/biodiversity_rasters/')

mat_vuln_list <- list.files('output/afr/biodiversity_rasters_cci/', full.names=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='threat_DDpred', value=TRUE)

mat_vuln_rescale <- raster(paste0('output/afr/biodiversity_rasters_cci/', mat_vuln_list[1]))



