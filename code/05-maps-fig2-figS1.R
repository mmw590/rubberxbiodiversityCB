#### rubberxbiodiversityCB 
#### 05-maps-fig2-figS1.R #### 


#### For generalised version of bivariate chloropleth map (limited to equal number of interval classes along y and axis), follow link below:  
#http://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html

rm(list=ls())
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'

library(dplyr)
library(sf)
library(raster)
library(ggplot2)
library(cowplot)
library(magick)
library(scales) #for hue 


############### 4 x 5 Color Matrix Bivariate Chloropleth Map ######################

#### Col.matrix fxn  (MANUAL)
colmat.manual <- function(nclass.y=5, nclass.x=5, upperleft="blue3", upperright="red", bottomleft="grey", bottomright="#FFE60F"){
  for (i in 1:nclass.y){
    rowcolorlist[[i]] <- colorRampPalette( c(colorRampPalette(c(upperleft, bottomleft))(nclass.y)[i], 
                                             colorRampPalette(c(upperright,bottomright))(nclass.y)[i]) )(nclass.x) 
  }
  
  col.matrix <- unlist(rowcolorlist)
  return(col.matrix)
  
  #still working on automated quick legend plot
  #col.matrix.plot <- plot(x=rep(1:nclass.x, nclass.y), y=rep(nclass.y:1, each=nclass.x), bg=as.character(t(col.matrix)), pch=22, cex=5, xlim=c(0,nclass.x+1), ylim=c(0,nclass.y+1)) 
  #print(col.matrix.plot)
}

rowcolorlist <- list()
col.matrix2 <- colmat.manual(nclass.y=4)

zz <- expand.grid( vuln=seq(0.2,1,0.2), suit=c(1,0.8,0.6,0.4)) #upperlimits of the interval classes
zz$bivcol <- seq_len(nrow(zz))
zz$color <- as.vector(t(col.matrix2))

# quick plot to check colours
#plot(x=rep(1:5, 4), y=rep(4:1, each=5), bg=as.character(zz$color), pch=22, cex=5, xlim=c(0,6), ylim=c(0,5))
plot(x=zz$vuln, y=zz$suit, bg=as.character(zz$color), pch=22, cex=6, xlim=c(min(zz$vuln)-0.1, max(zz$vuln)+0.1), ylim=c(min(zz$suit)-0.1, max(zz$suit)+0.1))
text(x=zz$vuln, y=zz$suit, labels=zz$bivcol)

#### + named vector for full color combos
myColors_manual <- structure(as.character(zz$color), names = as.character(zz$bivcol))

zz

rm(rowcolorlist, col.matrix2, colmat.manual )
#

#### Fxn returns a raster layer where the values point to the col.matrix 
# verbose/not generalisable, but easy to understand 
colmap_fxn <- function(rvuln, rsuit){
  ifelse( (rsuit>0.8 & rsuit<=1) & (rvuln>0.0 & rvuln<=0.2), 1,
  ifelse( (rsuit>0.8 & rsuit<=1) & (rvuln>0.2 & rvuln<=0.4), 2, 
  ifelse( (rsuit>0.8 & rsuit<=1) & (rvuln>0.4 & rvuln<=0.6), 3,
  ifelse( (rsuit>0.8 & rsuit<=1) & (rvuln>0.6 & rvuln<=0.8), 4,
  ifelse( (rsuit>0.8 & rsuit<=1) & (rvuln>0.8 & rvuln<=1), 5, 
                                          
  ifelse( (rsuit>0.6 & rsuit<=0.8) & (rvuln>0.0 & rvuln<=0.2), 6,
  ifelse( (rsuit>0.6 & rsuit<=0.8) & (rvuln>0.2 & rvuln<=0.4), 7, 
  ifelse( (rsuit>0.6 & rsuit<=0.8) & (rvuln>0.4 & rvuln<=0.6), 8,
  ifelse( (rsuit>0.6 & rsuit<=0.8) & (rvuln>0.6 & rvuln<=0.8), 9,
  ifelse( (rsuit>0.6 & rsuit<=0.8) & (rvuln>0.8 & rvuln<=1), 10, 
          
  ifelse( (rsuit>0.4 & rsuit<=0.6) & (rvuln>0.0 & rvuln<=0.2), 11,
  ifelse( (rsuit>0.4 & rsuit<=0.6) & (rvuln>0.2 & rvuln<=0.4), 12, 
  ifelse( (rsuit>0.4 & rsuit<=0.6) & (rvuln>0.4 & rvuln<=0.6), 13,
  ifelse( (rsuit>0.4 & rsuit<=0.6) & (rvuln>0.6 & rvuln<=0.8), 14,
  ifelse( (rsuit>0.4 & rsuit<=0.6) & (rvuln>0.8 & rvuln<=1), 15, 
                                                                                  
  ifelse( (rsuit>0 & rsuit<=0.4) & (rvuln>0.0 & rvuln<=0.2), 16,
  ifelse( (rsuit>0 & rsuit<=0.4) & (rvuln>0.2 & rvuln<=0.4), 17, 
  ifelse( (rsuit>0 & rsuit<=0.4) & (rvuln>0.4 & rvuln<=0.6), 18,
  ifelse( (rsuit>0 & rsuit<=0.4) & (rvuln>0.6 & rvuln<=0.8), 19,
  ifelse( (rsuit>0 & rsuit<=0.4) & (rvuln>0.8 & rvuln<=1), 20,   0))))) ))))) ))))) )))))
}





######## MAKE DFS FOR BIVMAP PLOT (ONLY NEED TO DO ONCE) #################

##### +Loop for saving bivmap res_dfs (Afr) #############
# Masking LUs, PAs from suitability 
mat_land_use_afr <- raster('output/afr/various_rasters/land_use_africa_CCI.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use_afr <- subs(mat_land_use_afr, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_pa <- raster('output/afr/various_rasters/protected_areas_afr.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_afr, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_afr, maskvalue=0, updatevalue=NA)

# Suitability 
mat_suit_afr <- raster('output/afr/various_rasters/suitability_afr.tif')
mat_suit_afr_mask <- mask(mat_suit_afr, mat_land_use_afr, maskvalue=NA, updatevalue=NA)
mat_suit_afr_mask <- mask(mat_suit_afr_mask, mat_land_use_afr, maskvalue=0, updatevalue=0) #
mat_suit_afr_mask <- mask(mat_suit_afr_mask, mat_pa2, maskvalue=1, updatevalue=NA)

#plot(mat_suit_afr)
#plot(mat_suit_afr_mask)

rm(reclassify.df, mat_land_use_afr, mat_pa, mat_pa2, mat_suit_afr)


# Biodiversity rasters (should already be masked)
mat_vuln_list <- list.files('output/afr/biodiversity_rasters/', full.names=FALSE)
mat_vuln_list <- grep(mat_vuln_list, pattern='.tif', value=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='std_mask', value=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='_ext', value=TRUE, invert=TRUE)


#Transform to latlon for plotting
refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

bivmap_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_afr_mask)[1], ncols=dim(mat_suit_afr_mask)[2])

res_df_list2 <- list()

theme_set(theme_cowplot())
i=9
system.time(
  for (i in 1:length(mat_vuln_list)){
    mat_vuln_rescale <- raster(paste0('output/afr/biodiversity_rasters/', mat_vuln_list[i]))
    
    bivmap <- overlay(mat_vuln_rescale, mat_suit_afr_mask, fun=colmap_fxn) #for unequal classes 3x5
    bivmap_latlon <- projectRaster(bivmap, bivmap_latlon, method='ngb')
    res_spdf <- rasterToPoints(bivmap_latlon)
    res_df <- as.data.frame(res_spdf)
    
    res_df_list2[[i]] <- res_df
  }
)
# 17s

plot(bivmap)
plot(bivmap_latlon)

names(res_df_list2) <- gsub('m0_', 'biv20_df_latlon_', mat_vuln_list)
names(res_df_list2) <- gsub('rescaled_twice_afr', 'biv20_df_latlon_afr_rescaled_twice', names(res_df_list2))
names(res_df_list2) <- gsub('tif', 'csv', names(res_df_list2))

#save dfs in list to csv ... 
mapply(function(dfslistname, dfslist) 
  write.csv(dfslist, file = paste0("output/dfs_for_figs/afr/", dfslistname), row.names = FALSE), 
  names(res_df_list2), res_df_list2)



######## +Loop for saving bivmap res_dfs (SSEA) #################
# Masking LUs, PAs from suitability 
mat_land_use_ssea <- raster('output/ssea/various_rasters/land_use_ssea.tif')
reclassify.df <- read.csv('data/mat_land_use_reclassifyMW.csv')
mat_land_use_ssea <- subs(mat_land_use_ssea, reclassify.df, by='NB_LAB', which='hassparse', subsWithNA=FALSE)

mat_pa <- raster('output/ssea/various_rasters/protected_areas_ssea.tif')
mat_pa2 <- mask(mat_pa, mat_pa, maskvalue=0, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_ssea, maskvalue=NA, updatevalue=NA)
mat_pa2 <- mask(mat_pa2, mat_land_use_ssea, maskvalue=0, updatevalue=NA)

mat_conc <- raster('output/ssea/various_rasters/concessions_ssea.tif') 
mat_conc2 <- mask(mat_conc, mat_conc, maskvalue=0, updatevalue=NA)
plot(mat_conc)

mat_suit_ssea <- raster('output/ssea/various_rasters/suitability_ssea.tif')
mat_suit_ssea_mask <- mask(mat_suit_ssea, mat_land_use_ssea, maskvalue=NA, updatevalue=NA)
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_land_use_ssea, maskvalue=0, updatevalue=0) #
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_pa2, maskvalue=1, updatevalue=NA)
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_conc2, maskvalue=1, updatevalue=NA)

plot(mat_suit_ssea)
plot(mat_suit_ssea_mask)

rm(reclassify.df, mat_land_use_ssea, mat_pa, mat_pa2, mat_conc, mat_conc2, mat_suit_ssea)

# Biodiversity rasters
mat_vuln_list <- list.files('output/ssea/biodiversity_rasters/', full.names=FALSE)
mat_vuln_list <- grep(mat_vuln_list, pattern='std_mask', value=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='.tif', value=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='_ext', value=TRUE, invert=TRUE)

#Transform to latlon for plotting
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
bivmap_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_ssea_mask)[1], ncols=dim(mat_suit_ssea_mask)[2])

res_df_list2 <- list()

system.time(
  for (i in 1:length(mat_vuln_list)){
    mat_vuln_rescale <- raster(paste0('output/ssea/biodiversity_rasters/', mat_vuln_list[i]))
    
    bivmap <- overlay(mat_vuln_rescale, mat_suit_ssea_mask, fun=colmap_fxn) #for unequal classes 3x5
    bivmap_latlon <- projectRaster(bivmap, bivmap_latlon, method='ngb')
    res_spdf <- rasterToPoints(bivmap_latlon)
    res_df <- as.data.frame(res_spdf)
    
    res_df_list2[[i]] <- res_df
  }
)
#16s for manual overlay fxn

plot(bivmap_latlon)

names(res_df_list2) <- gsub('m0_', 'biv20_df_latlon_', mat_vuln_list)
names(res_df_list2) <- gsub('rescaled_twice_ssea', 'biv20_df_latlon_ssea_rescaled_twice', names(res_df_list2))
names(res_df_list2) <- gsub('tif', 'csv', names(res_df_list2))


#save dfs in list to csv ... 
mapply(function(dfslistname, dfslist) 
  write.csv(dfslist, file = paste0("output/dfs_for_figs/ssea/", dfslistname), row.names = FALSE), 
  names(res_df_list2), res_df_list2)





############## MAKE INDIVIDUAL PLOTS (AFRICA) ###################
rm(list=setdiff(ls(), c("GISfolder", "zz", "myColors_manual")))
gc()

#### load afr_countries_wgs 
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')

refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
afr_countries_wgs <- st_crop(gadm_0, refExtent_wgs) #include Middle East polygons



#### + loop for comb biv plots (AFRICA) ####
res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv20_df", res_df_list)]
res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list_comb <- res_df_list[grep("comb", res_df_list)]
ggfiglist_afr_comb <- list()

system.time(
  for (i in 1:length(res_df_list_comb)){
    res_df0 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list_comb[i]))
    colnames(res_df0)[3] <- 'bivcol'
    res_df0$bivcol <- as.character(res_df0$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list_comb[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_afr_comb[[i]] <- ggplot() +
      geom_tile(data=res_df0, aes(y=round(y, 10), x=round(x, 10), fill=bivcol)) +
      scale_fill_manual(values = myColors_manual) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0.5,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none") #, 
    #plot.background = element_rect(color = "black")) #,
    #panel.border = element_rect(colour = "black", fill=NA))
    
    print(ggname)
    #print(ggfig)
    names(ggfiglist_afr_comb)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr_comb[[i]], width=169, units='mm', dpi=600)
    
  }
)
#ggfiglist_afr_comb[[3]]


#### ++ correcting res_df files ####
#res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
#res_df_list <- res_df_list[grep("biv20_df", res_df_list)]
#res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_afr <- list()

# Note that geom_tile will not produce the error but will instead produce a blank map
# troubleshooting error in geom_raster, column infinity ###
#Error in matrix(NA_character_, nrow = nrow, ncol = ncol) : invalid 'ncol' value (too large or NA) geom_raster
#https://github.com/tidyverse/ggplot2/blob/9eae13b3d17bde26cf9df649887b4a6bb2ac92ce/R/geom-raster.r#L72-L79
# likely very small jitterings in the data can cause this problem

res_df9 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[9]))
res_df3 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[3]))
res_df6 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[6]))
res_df12 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[12]))
all.equal(res_df9$x, res_df3$x, tolerance=0) #"Mean relative difference: 9.537867e-08" 
all.equal(res_df9$y, res_df3$y, tolerance=0) #"Mean relative difference: 1.205997e-07" 

# datasets with no issues have the same x and y
all.equal(res_df3$x, res_df6$x, tolerance=0)
all.equal(res_df3$x, res_df12$x, tolerance=0)

res_df2 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[2])) #data with issues
res_df5 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[5])) #data no issues
res_df8 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[8])) #data no issues
res_df11 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[11])) #data no issues
all.equal(res_df2$x, res_df5$x, tolerance=0) #"Mean relative difference: 1.069883e-07"
all.equal(res_df2$y, res_df5$y, tolerance=0) #"Mean relative difference: 9.707528e-08"

#compare two problematic datasets, not the same
all.equal(res_df9$x, res_df2$x, tolerance=0) #"Mean relative difference: 9.219911e-08"
all.equal(res_df9$y, res_df2$y, tolerance=0) # "Mean relative difference: 1.418274e-07"

# datasets with no issues have the same x and y
all.equal(res_df11$x, res_df5$x, tolerance=0) #TRUE
all.equal(res_df11$x, res_df8$x, tolerance=0) #TRUE
all.equal(res_df11$y, res_df5$y, tolerance=0) #TRUE
all.equal(res_df11$y, res_df8$y, tolerance=0) #TRUE

# even for vulnA vs vulnT , have same x and y
all.equal(res_df5$x, res_df6$x, tolerance=0) #TRUE
all.equal(res_df11$y, res_df12$y, tolerance=0) #TRUE

#### Should be safe to simply replace the x and y of problematic datasets 
res_df9 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[9]))
res_df9$x <- res_df3$x
res_df9$y <- res_df3$y
head(res_df9)
write.csv(res_df9, paste0('output/dfs_for_figs/afr/', res_df_list[9]), row.names=FALSE)

res_df2 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[2]))
res_df2$x <- res_df5$x
res_df2$y <- res_df5$y
write.csv(res_df2, paste0('output/dfs_for_figs/afr/', res_df_list[2]), row.names=FALSE)


#### + loop for taxa biv subplots (AFRICA) - with corrected files ####
#res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
#res_df_list <- res_df_list[grep("biv20_df", res_df_list)]
#res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_afr <- list()

system.time(
  for (i in 1:length(res_df_list)){
    res_df0 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[i]))
    colnames(res_df0)[3] <- 'bivcol'
    res_df0$bivcol <- as.character(res_df0$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_afr[[i]] <- ggplot() +
      #geom_raster(data=res_df0, aes(y=y, x=x, fill=bivcol)) +
      geom_tile(data=res_df0, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_manual) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none") #, 
    #plot.background = element_rect(color = "black")) #,
    #panel.border = element_rect(colour = "black", fill=NA))
    
    print(ggname)
    #print(ggfig)
    names(ggfiglist_afr)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr[[i]], width=169, units='mm', dpi=300)
    
  }
)


# filter out richness figs
ggfigrich_afr <- ggfiglist_afr[grep('richness', names(ggfiglist_afr))]
names(ggfigrich_afr)

# filter out vuln all figs
ggfigvuln_afr <- ggfiglist_afr[grep('vuln_all', names(ggfiglist_afr))]
names(ggfigvuln_afr)
#ggfigvuln_afr[[4]]

# filter out vulnT figs
ggfigvulnT_afr <- ggfiglist_afr[grep('vuln_threat', names(ggfiglist_afr))]
names(ggfigvulnT_afr)
#ggfigvulnT_afr[[3]]  #doesn't take too long to plot



############## MAKE INDIVIDUAL PLOTS (SSEA) ###################
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))

#gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0') #gadm countries
ssea_countries_wgs <- st_crop(gadm_0, refExtent_wgs)

#### + loop for comb biv plots (SSEA) ####
res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv20_df", res_df_list)]
res_df_list <- res_df_list[grep("csv", res_df_list)]
res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list_comb <- res_df_list[grep("comb", res_df_list)]

ggfiglist_ssea_comb <- list()

system.time(
  for (i in 1:length(res_df_list_comb)){
    res_df <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list_comb[i]))
    colnames(res_df)[3] <- 'bivcol'
    res_df$bivcol <- as.character(res_df$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list_comb[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_ssea_comb[[i]] <- ggplot() +
      geom_tile(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_manual) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0.5,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none" )
    
    print(ggname)
    names(ggfiglist_ssea_comb)[i] <- paste0(ggname)
    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea_comb[[i]], width=169, units='mm', dpi=600)
  }
)

#ggfiglist_ssea_comb[[3]]


#### ++ correcting res_df files ####
# res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
# res_df_list <- res_df_list[grep("biv_df", res_df_list)]
# res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_ssea <- list()

# troubleshooting error in geom_raster, column infinity ###
res_df6 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[6])) #issue
res_df3 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[3]))
res_df9 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[9]))
res_df5 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[5]))

all.equal(res_df6$x, res_df3$x, tolerance=0) #"Mean relative difference: 1.42913e-07"
all.equal(res_df6$y, res_df3$y, tolerance=0) #"Mean relative difference: 1.090935e-07"

# datasets with no issues have the same x and y
all.equal(res_df3$x, res_df9$x, tolerance=0)
all.equal(res_df3$y, res_df9$y, tolerance=0)
all.equal(res_df5$x, res_df9$x, tolerance=0)
all.equal(res_df5$y, res_df9$y, tolerance=0)

#### Should be safe to simply replace the x and y of problematic datasets 
res_df6 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[6])) #issue
res_df6$x <- res_df9$x
res_df6$y <- res_df9$y
head(res_df6)
write.csv(res_df6, paste0('output/dfs_for_figs/ssea/', res_df_list[6]), row.names=FALSE)


#### + loop for taxa biv subplots (SSEA) - with corrected files ####
# res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
# res_df_list <- res_df_list[grep("biv_df", res_df_list)]
# res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_ssea <- list()

system.time(
  for (i in 1:length(res_df_list)){
    res_df <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[i]))
    colnames(res_df)[3] <- 'bivcol'
    res_df$bivcol <- as.character(res_df$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_ssea[[i]] <- ggplot() +
      #geom_raster(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      geom_tile(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_manual) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none" )
    
    print(ggname)
    #print(ggfig)
    names(ggfiglist_ssea)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea[[i]], width=169, units='mm', dpi=300)
  }
)


# filter out richness figs
ggfigrich_ssea <- ggfiglist_ssea[grep('richness', names(ggfiglist_ssea))]
names(ggfigrich_ssea)

# filter out vuln all figs
ggfigvuln_ssea <- ggfiglist_ssea[grep('vuln_all', names(ggfiglist_ssea))]
names(ggfigvuln_ssea)

# filter out vulnT figs
ggfigvulnT_ssea <- ggfiglist_ssea[grep('vuln_threat', names(ggfiglist_ssea))]
names(ggfigvulnT_ssea)

#ggfigvulnT_ssea[[2]]




############## MAKE COMBINED PLOTS FOR PAPER ###################

# + Manually calculating the dimensions of the figure ####
mat_vuln_ssea <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_threat_std_mask.tif')
mat_vuln_afr <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_threat_std_mask.tif')

dim(mat_vuln_ssea) # 582 (ht) x 1026 (wd)
dim(mat_vuln_afr)  # 612 (ht) x 918 (wd)

rel_w_afr = dim(mat_vuln_afr)[2]/dim(mat_vuln_ssea)[2] # afr width
rel_h_afr = dim(mat_vuln_afr)[1]/dim(mat_vuln_ssea)[2] # afr height
rel_w_ssea = dim(mat_vuln_ssea)[2]/dim(mat_vuln_ssea)[2] # ssea height
rel_h_ssea = dim(mat_vuln_ssea)[1]/dim(mat_vuln_ssea)[2] # ssea height

# max width = 6.85 inches
subplot_width = (6.85/3)
subplot_height_afr = (6.85/3)*rel_h_afr
subplot_height_ssea = (6.85/3)*rel_h_ssea
overall_height = (subplot_height_afr*(2+1)) + (subplot_height_ssea*(2+1))  #base_height, max length is a full page, about 9inches. 7.731462 is ok, ratio of comb plot to subplots is 2:1

rm(mat_vuln_afr, mat_vuln_ssea)



#### + Legends ####
# df for legend plot (as 5x5 matrix)
zz_bottomrow <- zz[16:20, ]
zz_bottomrow$suit <- 0.2
zz3 <- rbind(zz, zz_bottomrow )
zz3 <- zz3 %>% mutate(vulnplot=vuln-0.1, suitplot=suit-0.1)

# # df for legend plot (as 4x5 matrix)
# zz2 <- zz %>% mutate(vulnplot=vuln-0.1, suitplot=suit-0.1)

gg_bivcol_legend_rich <- ggplot(data=zz3, aes(x=vulnplot, y=suitplot)) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_manual)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  #scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2), labels=c(0,0.4,0.6,0.8,1), lim=c(0.2,1)) + #for 4x5
  theme_cowplot(font_size=12) +
  ylab('Rubber bioclimatic suitability') + xlab('Species richness') +
  theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm"))

gg_bivcol_legend_vuln <- ggplot(data=zz3, aes(x=vulnplot, y=suitplot)) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_manual)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2)) +
  #scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2), labels=c(0,0.4,0.6,0.8,1), lim=c(0.2,1)) + #for 4x5
  theme_cowplot(font_size=12) +
  ylab('Rubber bioclimatic suitability') + xlab('Extinction vulnerability') +
  theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm"))




########### + Concessions and PAs Overlay ###############

### + Load Concessions Point Data 
afr_pa_df <- read.csv('output/dfs_for_figs/afr/pa_df.csv')
ssea_pa_df <- read.csv('output/dfs_for_figs/ssea/pa_df_LU.csv')

ssea_conc_df <- read.csv('output/dfs_for_figs/ssea/conc_df_raster.csv')

conc_sf <- st_read('data/conc_sf_landmatrix.gpkg') #this gpkg was created from data downloaded from landmatrix.org, which is freely available under the Creative Commons Attribution 4.0 International License. Hence I am able to share my processed data. Code and input data used to make this gpkg are in 00-preprocess-landmatrix.R 

ssea_conc_sf <- st_crop(conc_sf, extent(ssea_countries_wgs)) 
afr_conc_sf <- st_crop(conc_sf, extent(afr_countries_wgs)) 

# #Remove duplicate coordinates (to reduce number of points for better visuals) -
afr_conc_sf_nodups <- afr_conc_sf %>% distinct(geom, status) #drop 3 pts
ssea_conc_sf_nodups <- ssea_conc_sf %>% distinct(geom, status) #drop 65 pts


## + No. of Concessions - FOR RESULTS Section 2 DISCUSSION #####
check <- dplyr::filter(ssea_conc_sf, is.na(status)==FALSE)
check$loc.country <- droplevels(check$loc.country)
table(check$loc.country)

check <- dplyr::filter(afr_conc_sf, is.na(status)==FALSE)
check$loc.country <- droplevels(check$loc.country)
table(check$loc.country)




#######  * Simple combined spp plots, no PAs, (Richness, VulnA, VulnT) #####
gg_sea_afr_rich <- plot_grid(ggfiglist_afr_comb[[1]], ggfiglist_ssea_comb[[1]], labels = c('A', 'B'), nrow=2, ncol = 1, label_size=8, align="v", axis="r", rel_heights = c(rel_h_afr, rel_h_ssea), rel_widths = c(rel_w_afr, rel_w_ssea)) + #
  draw_plot(gg_bivcol_legend_rich, x=0, y=0.5, width=0.35, height=0.3) 

gg_sea_afr_vuln <- plot_grid(ggfiglist_afr_comb[[2]], ggfiglist_ssea_comb[[2]], labels = c('A', 'B'), nrow=2, ncol = 1, label_size=8, align="v", axis="r", rel_heights = c(rel_h_afr, rel_h_ssea), rel_widths = c(rel_w_afr, rel_w_ssea)) + #
  draw_plot(gg_bivcol_legend_vuln, x=0, y=0.5, width=0.35, height=0.3) 

gg_sea_afr_vulnT <- plot_grid(ggfiglist_afr_comb[[3]], ggfiglist_ssea_comb[[3]], labels = c('A', 'B'), nrow=2, ncol = 1, label_size=8, align="v", axis="r", rel_heights = c(rel_h_afr, rel_h_ssea), rel_widths = c(rel_w_afr, rel_w_ssea)) + #
  draw_plot(gg_bivcol_legend_vuln, x=0, y=0.5, width=0.35, height=0.3) 

# cowplot::save_plot("output/results/ssea_afr_comb_richness_std.png", gg_sea_afr_rich, base_width=6.85, base_height = 6.85*(rel_h_afr+rel_h_ssea))
# cowplot::save_plot("output/results/ssea_afr_comb_vuln_all_std.png", gg_sea_afr_vuln, base_width=6.85, base_height = 6.85*(rel_h_afr+rel_h_ssea)) # ????
# cowplot::save_plot("output/results/ssea_afr_comb_vuln_threat_std.png", gg_sea_afr_vulnT, base_width=6.85, base_height = 6.85*(rel_h_afr+rel_h_ssea))





####### * Multipanel Comb spp + Taxa subplots + PA (Richness, VulnA, VulnT) #####

#####   + Multipanel VulnT (Fig 2 for paper) #####
ggfigvulnT_afr_comb_pa <- ggfiglist_afr_comb[[3]] + geom_raster(data=afr_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)
ggfigvulnT_ssea_comb_pa <- ggfiglist_ssea_comb[[3]] + geom_raster(data=ssea_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)

top_sub_afr <- plot_grid(ggfigvulnT_afr[[1]], ggfigvulnT_afr[[2]], ggfigvulnT_afr[[3]], ggfigvulnT_afr[[4]],
                         labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

top_afr <- plot_grid(ggfigvulnT_afr_comb_pa, top_sub_afr, labels = c('A', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

bottom_sub_ssea <- plot_grid(ggfigvulnT_ssea[[1]], ggfigvulnT_ssea[[2]], ggfigvulnT_ssea[[3]], ggfigvulnT_ssea[[4]],
                             labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

bottom_ssea <- plot_grid(ggfigvulnT_ssea_comb_pa, bottom_sub_ssea, labels = c('B', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

gg_bivcol_legend_vuln_taxa <- gg_bivcol_legend_vuln +  theme_cowplot(font_size=10) + theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm")) 

gg_topbottom2 <- plot_grid(top_afr, bottom_ssea, ncol = 1, label_size=8, align="v") +
  draw_plot(gg_bivcol_legend_vuln_taxa, x=0.0, y=0.68, width=0.29, height=0.24) 

cowplot::save_plot("output/results/fig2.tiff", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85, dpi=300) 

cowplot::save_plot("output/results/fig2_highres.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85, dpi=1000) #super high res version





##### + Multipanel VulnA (Data S1 Fig II) #####
ggfigvuln_afr_comb_pa <- ggfiglist_afr_comb[[2]] + geom_raster(data=afr_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)
ggfigvuln_ssea_comb_pa <- ggfiglist_ssea_comb[[2]] + geom_raster(data=ssea_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)

top_sub_afr <- plot_grid(ggfigvuln_afr[[1]], ggfigvuln_afr[[2]], ggfigvuln_afr[[3]], ggfigvuln_afr[[4]],
                         labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

top_afr <- plot_grid(ggfigvuln_afr_comb_pa, top_sub_afr, labels = c('A', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

bottom_sub_ssea <- plot_grid(ggfigvuln_ssea[[1]], ggfigvuln_ssea[[2]], ggfigvuln_ssea[[3]],  ggfigvuln_ssea[[4]],
                             labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

bottom_ssea <- plot_grid(ggfigvuln_ssea_comb_pa, bottom_sub_ssea, labels = c('B', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

gg_bivcol_legend_vuln_taxa <- gg_bivcol_legend_vuln +  theme_cowplot(font_size=10) + theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm")) 

gg_topbottom2 <- plot_grid(top_afr, bottom_ssea, ncol = 1, label_size=8, align="v") +
  draw_plot(gg_bivcol_legend_vuln_taxa, x=0.0, y=0.68, width=0.29, height=0.24) 

cowplot::save_plot("output/results/dataS1_fig2.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85) 





# # + Multipanel Richness ####
# ggfigrich_afr_comb_pa <- ggfiglist_afr_comb[[1]] + geom_raster(data=afr_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)
# ggfigrich_ssea_comb_pa <- ggfiglist_ssea_comb[[1]] + geom_raster(data=ssea_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)
# 
# top_sub_afr <- plot_grid(ggfigrich_afr[[1]], ggfigrich_afr[[2]], ggfigrich_afr[[3]],  ggfigrich_afr[[4]],
#                          labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
#   draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
#   draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
#   draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
#   draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)
# 
# top_afr <- plot_grid(ggfigrich_afr_comb_pa,  top_sub_afr, labels = c('A', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")
# 
# bottom_sub_ssea <- plot_grid(ggfigrich_ssea[[1]], ggfigrich_ssea[[2]], ggfigrich_ssea[[3]], ggfigrich_ssea[[4]],
#                              labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
#   draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
#   draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
#   draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
#   draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)
# 
# bottom_ssea <- plot_grid(ggfigrich_ssea_comb_pa,  bottom_sub_ssea, labels = c('B', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")
# 
# gg_bivcol_legend_rich_taxa <- gg_bivcol_legend_rich + theme_cowplot(font_size=10) + theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm")) 
# 
# gg_topbottom2 <- plot_grid(top_afr, bottom_ssea, ncol = 1, label_size=8, align="v") +
#   draw_plot(gg_bivcol_legend_rich_taxa, x=0.0, y=0.68, width=0.29, height=0.24) 
# 
# cowplot::save_plot("output/results/xtra_fig2_richness.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85)








######## PLOT AOC/AOR with Concessions (Fig S1) #######

#### +Levels of Compromise ####
# Assigning 'levels of compromise', from most ideal to convert first (1) to (6). 

zz3 <- zz3 %>% 
  mutate(comprom =  ifelse((vuln<=0.2 & suit >0.8), 1,  #production driven compromise (compromise vuln, preserve suit)
                           ifelse((vuln<=0.2 & suit>0.6 & suit<=0.8), 3,
                                  ifelse((vuln<=0.2 & suit>0.4 & suit<=0.6), 5,
                                         ifelse((vuln<=0.4 & suit >0.8), 2,
                                                ifelse((vuln<=0.4 & suit>0.6 & suit<=0.8), 4, 
                                                       ifelse((vuln<=0.4 & suit>0.4 & suit<=0.6), 6, 
                                                              99)))))) )

# Assigning levels of risk
zz3 <- zz3 %>% 
  mutate(risk = ifelse((vuln>0.61 & suit<=0.4), 1, 0)) #Lose-Lose only

# + Customise Legend ####

zz4 <- zz3
zz4$color <- ifelse(zz4$comprom <99 | zz4$risk >0, zz3$color, NA)

#### + named vector for full color combos
myColors_aoc <- structure(as.character(zz4$color), names = as.character(zz4$bivcol))

gg_aoc_legend_vuln <- ggplot(data=zz4, aes(x=vulnplot, y=suitplot)) +
  geom_hline(yintercept=seq(0,0.8,0.2), lty="dotted", lwd=0.5) +
  geom_vline(xintercept=seq(0,0.8,0.2), lty="dotted", lwd=0.5) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_aoc)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  #scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2), labels=c(0,0.4,0.6,0.8,1), lim=c(0.2,1)) + #4x5
  theme_cowplot(font_size=12) +
  ylab('Rubber bioclimatic suitability') + xlab('Extinction vulnerability') +
  theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm"))

aor.rect <- data.frame(xmin=0.61, xmax=1, ymin=0.01, ymax=0.39)
aor.text <- data.frame(x=c(0.8, 0.8), y=c(0.25,0.15), text=c("Lose-", "Lose" ))

aoc.rect <- data.frame(xmin=0, xmax=0.39, ymin=0.41, ymax=1)
aoc.text <- data.frame(x=c(0.2, 0.2, 0.2), y=c(0.7,0.6,0.5), text=c("Area of", "Comp-", "romise" ))

ww.rect <- data.frame(xmin=0.01, xmax=0.19, ymin=0.81, ymax=0.99)
ww.text <- data.frame(x=0.1, y=0.9, text="WW")

gg_aoc_legend_vuln <- gg_aoc_legend_vuln + 
  geom_rect(data=ww.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.5), lty=1, lwd=0.7, inherit.aes = FALSE) +
  geom_text(data=aoc.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.8), size=4.7) +
  geom_text(data=aor.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.5), size=4.7) +
  geom_text(data=ww.text, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.5), size=4.5) 

gg_aoc_legend_vuln




#### +Africa #
res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv20_df_", res_df_list)]
res_df_list <- res_df_list[grep("combspp", res_df_list)]
res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]

ggfiglist_afr_aoc <- list()


#### loop for AOC+AOR conc plots (AFR) #
system.time(
  for (i in 1:length(res_df_list)){
    res_df <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[i]))
    colnames(res_df)[3] <- 'bivcol'
    res_df <- left_join(res_df, zz3, by=c('bivcol'))
    res_df$bivcol <- as.character(res_df$bivcol)
    
    #res_df1 <- dplyr::filter(res_df, comprom < 99 | risk >0 )
    
    ggname <- gsub('biv20_df', 'aoc_aor_conc', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    print(ggname)
    
    ggfiglist_afr_aoc[[i]] <- ggplot() +
      geom_tile(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_aoc) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.35) +
      geom_sf(data=afr_conc_sf, aes(shape = status), color='red', size=1.0, alpha=0.7,  stroke=0.55) +
      #scale_shape_manual(values=c(21,25,24)) +
      scale_shape_manual(values=c(0,6,2)) + #square, down triangle, up triangle
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none")   
    
    names(ggfiglist_afr_aoc)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr_aoc[[i]], width=174*rel_w_afr/rel_w_ssea, units='mm', dpi=600)
  }
)



#### +SSEA #
res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv20_df", res_df_list)]
res_df_list <- res_df_list[grep("combspp", res_df_list)]
res_df_list <- res_df_list[grep("ext", res_df_list, invert=TRUE)]

ggfiglist_ssea_aoc <- list()

#### loop for AOC+AOR conc plots (SSEA) #
system.time(
  for (i in 1:length(res_df_list)){
    res_df <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[i]))
    colnames(res_df)[3] <- 'bivcol'
    res_df <- left_join(res_df, zz3, by=c('bivcol'))
    res_df$bivcol <- as.character(res_df$bivcol)
    
    #res_df1 <- dplyr::filter(res_df, comprom < 99 | risk >0 )
    
    #AOC
    ggname <- gsub('biv20_df', 'aoc_aor_conc', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    print(ggname)
    
    ggfiglist_ssea_aoc[[i]] <- ggplot() +
      #geom_raster(data=res_df1, aes(y=round(y,6), x=round(x,6), fill=bivcol)) +
      geom_tile(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_aoc) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.35) +
      geom_sf(data=ssea_conc_sf, aes(shape = status), color='red', size=1.0, alpha=0.7,  stroke=0.55)  +
      #scale_shape_manual(values=c(22,25,24)) +     
      scale_shape_manual(values=c(0,6,2)) + #square, down triangle, up triangle
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none")  
    
    names(ggfiglist_ssea_aoc)[i] <- paste0(ggname)
    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea_aoc[[i]], width=174, units='mm', dpi=600)
  }
)




### FIG S1: AOC/AOR ONLY + CONCESSIONS (AFR+SSEA) ####

gg_aoc_aor_conc <- plot_grid(ggfiglist_afr_aoc[[3]] , ggfiglist_ssea_aoc[[3]] , labels = c('A', 'B'), nrow=2, ncol = 1, label_size=8, align="v", axis="r", rel_heights = c(rel_h_afr, rel_h_ssea), rel_widths = c(rel_w_afr, rel_w_ssea)) + #
  draw_plot(gg_aoc_legend_vuln, x=0, y=0.5, width=0.38, height=0.33) 

system.time(cowplot::save_plot("output/results/figS1.png", gg_aoc_aor_conc, base_width=6.85, base_height = 6.85*(rel_h_afr+rel_h_ssea), dpi=300)) # 19s


gg_aoc_aor_conc_vulnA <- plot_grid(ggfiglist_afr_aoc[[2]] , ggfiglist_ssea_aoc[[2]] , labels = c('A', 'B'), nrow=2, ncol = 1, label_size=8, align="v", axis="r", rel_heights = c(rel_h_afr, rel_h_ssea), rel_widths = c(rel_w_afr, rel_w_ssea)) + #
  draw_plot(gg_aoc_legend_vuln, x=0, y=0.5, width=0.38, height=0.33) 

system.time(cowplot::save_plot("output/results/dataS1_results_reference_aoc_aor_conc.png", gg_aoc_aor_conc_vulnA, base_width=6.85, base_height = 6.85*(rel_h_afr+rel_h_ssea), dpi=300)) # 19s



#### FIG S4 (graphical explainer for compromise scenarios 3a-c) #### 
gg_bivcol_legend_vuln <- ggplot(data=zz3, aes(x=vulnplot, y=suitplot)) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_manual)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2)) +
  #scale_y_continuous(expand=c(0,0), breaks=seq(0.2,1,0.2), labels=c(0,0.4,0.6,0.8,1), lim=c(0.2,1)) + #for 4x5
  theme_cowplot(font_size=8) +
  ylab('Rubber bioclimatic suitability') + xlab('Extinction vulnerability') +
  theme(legend.position="none", plot.margin = unit(c(0.5,0.5,0,0),"cm"))


aoc.rect.s4 <- data.frame(xmin=0.01, xmax=0.39, ymin=0.41, ymax=0.99)
aoc.text.s4 <- data.frame(x=rep(c(0.1,0.3),each=3), y=rep(c(0.5,0.7,0.9),2), text=c("5", "3", "1", "6", "4", "2" ))

gg_scen1 <- gg_bivcol_legend_vuln + 
  geom_rect(data=aoc.rect.s4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.8), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_text(data=aoc.text.s4, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.8), size=5, alpha=0.5) 


aoc.text.s4 <- data.frame(x=rep(c(0.1,0.3),each=3), y=rep(c(0.5,0.7,0.9),2), text=c("3", "2", "1", "6", "5", "4" ))  
gg_scen2 <- gg_bivcol_legend_vuln + 
  geom_rect(data=aoc.rect.s4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.8), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_text(data=aoc.text.s4, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.8), size=5, alpha=0.5) 


aoc.text.s4 <- data.frame(x=rep(c(0.1,0.3),each=3), y=rep(c(0.5,0.7,0.9),2), text=c("3", "2", "1", "4", "3", "2" ))
gg_scen3 <- gg_bivcol_legend_vuln + 
  geom_rect(data=aoc.rect.s4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color=gray(0.8), lty=1, lwd=0.7, inherit.aes = FALSE, alpha=0.5) +
  geom_text(data=aoc.text.s4, aes(x=x, y=y,label=text), inherit.aes=FALSE, color=gray(0.8), size=5, alpha=0.5) 


gg_scen_supp <- plot_grid(gg_scen1, gg_scen2, gg_scen3, labels=c('A', ' B', 'C'), label_size=8,  
                          align = 'h', nrow=1, ncol=3, rel_widths = c(1, 1, 1))  

ggsave('./output/results/figS4.png', plot=gg_scen_supp, height=174/3, width=174, units='mm', dpi=300) 
#cowplot::save_plot("output/results/figs4.png", gg_scen_supp, base_width=3.35, base_height=3.35/3, dpi=300) #using cowplot



