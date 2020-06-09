#### rubberxbiodiversityCB 
#### 07-suit-expanded-rescale.R #### 
rm(list=ls())

# Run this after both 01-areasofcompromise_AFR.R and 02-areasofcompromise_SSEA.R

library(sf)
library(raster)
library(dplyr)


#####################################################################################
###### Standardize scaling for Asia and Africa  #####

# [1,] 0.01348288 0.6371439
# [2,] 0.00000000 1.0000000
# [3,] 0.02844311 1.0000000
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
# [1,] 0.009843408 0.7711681
# [2,] 0.000000000 0.8131868
# [3,] 0.031437126 0.8607784
# [4,] 0.000000000 0.8750000
# [5,] 0.007936508 1.0000000

### Getting the maxValue of Afr and Asia for standardized second rescale of combspp to 0-1
rmaxValue_forscale <- pmax(max(values(richness_comb_afr), na.rm=TRUE), max(values(richness_comb_ssea), na.rm=TRUE))
rminValue_forscale <- 0 #scale to min biodivesity Value to 0 as per before

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

richness_comb_rescale_afr <- rasterRescaleStd_combspp(richness_comb_afr)
plot(richness_comb_rescale_afr)
names(richness_comb_rescale_afr) <- "rescaled_twice_afr_combspp_richness"
range(values(richness_comb_rescale_afr), na.rm=TRUE) #[1] 0.01748371 0.82620625

richness_comb_rescale_ssea <- rasterRescaleStd_combspp(richness_comb_ssea)
plot(richness_comb_rescale_ssea)
names(richness_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_richness"
range(values(richness_comb_rescale_ssea), na.rm=TRUE) #[1] 0.01276428 1.00000000




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
# [3,] 0.02415459 1.0000000
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
# [1,] 0.02228629 0.7326687
# [2,] 0.00000000 0.6567164
# [3,] 0.06884058 0.9685990
# [4,] 0.00000000 1.0000000
# [5,] 0.02030457 0.9619289


# Getting the min and maxValue of Afr and Asia for standardized second rescale of combspp
(rmaxValue_forscale <- pmax(maxValue(vuln_comb_afr), maxValue(vuln_comb_ssea)) )
rminValue_forscale <- 0

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

vuln_comb_rescale_afr <- rasterRescaleStd_combspp(vuln_comb_afr)
plot(vuln_comb_rescale_afr)
names(vuln_comb_rescale_afr) <- "rescaled_twice_afr_combspp_vuln_all"
range(values(vuln_comb_rescale_afr), na.rm=TRUE) # [1] 0.02886447 0.86910440

vuln_comb_rescale_ssea <- rasterRescaleStd_combspp(vuln_comb_ssea)
plot(vuln_comb_rescale_ssea)
names(vuln_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_vuln_all"
range(values(vuln_comb_rescale_ssea), na.rm=TRUE) # 0.03041796 1.00000000




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
# [1,] 0.04337474 0.5656473
# [2,] 0.00000000 0.6944444
# [3,] 0.14492754 1.0000000
# [4,] 0.00000000 1.0000000
# [5,] 0.02857143 0.8928571

# Getting the maxValue of combspp Afr and Asia for standardized second rescale of combspp
(rmaxValue_forscale <- pmax(maxValue(vulnT_comb_afr), maxValue(vulnT_comb_ssea)))
rminValue_forscale <- 0

rasterRescaleStd_combspp <- function(r){
  ((r-rminValue_forscale)/(rmaxValue_forscale-rminValue_forscale))
}

vulnT_comb_rescale_afr <- rasterRescaleStd_combspp(vulnT_comb_afr)
plot(vulnT_comb_rescale_afr)
names(vulnT_comb_rescale_afr) <- "rescaled_twice_afr_combspp_vuln_threat"
range(values(vulnT_comb_rescale_afr), na.rm=TRUE) # 0.01921842 1.00000000

vulnT_comb_rescale_ssea <- rasterRescaleStd_combspp(vulnT_comb_ssea)
plot(vulnT_comb_rescale_ssea)
names(vulnT_comb_rescale_ssea) <- "rescaled_twice_ssea_combspp_vuln_threat"
range(values(vulnT_comb_rescale_ssea), na.rm=TRUE) #0.07019144 0.91536217




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
rescaled_raster_filenames <- paste0(rescaled_raster_filenames, '_std_mask_ext')

writeRaster(rescaledstack_afr_std_mask, filename= paste0('./output/afr/biodiversity_rasters/', rescaled_raster_filenames, '.tif'), bylayer=TRUE, format='GTiff', overwrite=TRUE)


rescaled_raster_filenames <- names(rescaledstack_ssea_std_mask)
rescaled_raster_filenames<- sub("_std.tif", "", rescaled_raster_filenames)
rescaled_raster_filenames <- paste0(rescaled_raster_filenames, '_std_mask_ext')

writeRaster(rescaledstack_ssea_std_mask, filename= paste0('./output/ssea/biodiversity_rasters/', rescaled_raster_filenames, '.tif'), bylayer=TRUE, format='GTiff', overwrite=TRUE)

