#### rubberxbiodiversityCB 
#### 09-suit-expanded-maps-figS3.R #### 

#### Bivariate chloropleth map 
#http://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html

#rm(list=ls())
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'

library(classInt)
library(dplyr)
library(sf)
library(raster)
library(ggplot2)
library(cowplot)
#library(magick)


#### Col.matrix fxn 
colmat.fixed <- function(nclasses=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label") {
  my.data <- seq(0, 1, .01)
  my.class <- classIntervals(my.data, n=nclasses, style="equal", intervalClosure="right")
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i])) #i=1, is upper left and upper right. 
    col.matrix[102-i, ] <- findColours(my.class, my.col) } #create horizontal color scale, between left and right, the upper most line goes into the bottom row of the matrix (because upper is higher value)
  
  
  plot(c(1,1), pch=19, col=my.pal.1, cex=0.5, xlim=c(0,1), ylim=c(0,1), frame.plot=F, xlab=xlab, ylab=ylab, cex.lab=1.3) # base plot
  
  for(i in 1:101){
    col.temp <- col.matrix[i-1, ]  #row in the 101 by 101 matrix. the plot plots from the bottom, 
    points(my.data, rep((i-1)/100, 101), pch=15, col=col.temp, cex=1)}  #my.data=x, y=0 to 1, by 0.01.
  #every i is each horizontal line, going up y-axis 
  
  seqs <- seq(0, 100, (100/nclasses)) #make a smaller color matrix based on the intervals??
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)] } 

col.matrix <- colmat.fixed(nclasses=5, upperleft="blue3", upperright="red", bottomleft="lightblue", xlab="Cumulative Vulnerability", ylab="Rubber Suitability") #bottom left default grey, bottom right default yellow




############ +Color codes for bivariate map ###################
# Create dataframe of bivcol codes corresponding to hex codes
col.matrix2 <- col.matrix

for(i in 1:length(col.matrix2)){
  ifelse(is.na(col.matrix2[i]), col.matrix2[i] <- 1, col.matrix2[i] <- which(col.matrix2[i]==col.matrix2)[1]) } #turn col.matrix2 into matrix of indices pointing to location in the col.matrix

zz <- expand.grid(b_row = seq(1,6,1), a_col = seq(1,6,1))
zz$vuln = (zz$a_col-1)*0.2
zz$suit = zz$b_row

for (i in 1:nrow(zz) )  {
  #z$color[i] <-  col.matrix[as.numeric(as.character(z$bivcol[i]))]
  zz$bivcol[i]<- col.matrix2[zz$b_row[i], zz$a_col[i]] %>% as.character %>% as.numeric
  zz$color[i] <-  col.matrix[zz$bivcol[i]]
}

zz2 <- zz %>% dplyr::filter(vuln >0 & suit >1) %>% dplyr::select(vuln:color)

#### + named vector for full color combos ####
myColors_biv_full <- structure(as.character(zz2$color), names = as.character(zz2$bivcol))

#zz2 <- zz2 %>% dplyr::mutate(vuln = vuln/10, suit = suit/10)

# Axis for plotting
zz2 <- zz2 %>% mutate(vulnplot=vuln-0.1, suitplot=suit-1)


#### Fxn returns a raster layer where the values point to the col.matrix 
# verbose but easy to understand 
colmap_fxn <- function(rvuln, rsuit){
  ifelse( (rsuit==2) & (rvuln>0.0 & rvuln<=0.2), 8,
  ifelse( (rsuit==3) & (rvuln>0.0 & rvuln<=0.2), 9, 
  ifelse( (rsuit==4) & (rvuln>0.0 & rvuln<=0.2), 10,
  ifelse( (rsuit==5) & (rvuln>0.0 & rvuln<=0.2), 11,
  ifelse( (rsuit==6) & (rvuln>0.0 & rvuln<=0.2), 12, 
     
  ifelse( (rsuit==2) & (rvuln>0.2 & rvuln<=0.4), 8+6,
  ifelse( (rsuit==3) & (rvuln>0.2 & rvuln<=0.4), 9+6, 
  ifelse( (rsuit==4) & (rvuln>0.2 & rvuln<=0.4), 10+6,
  ifelse( (rsuit==5) & (rvuln>0.2 & rvuln<=0.4), 11+6,
  ifelse( (rsuit==6) & (rvuln>0.2 & rvuln<=0.4), 12+6, 
          
  ifelse( (rsuit==2) & (rvuln>0.4 & rvuln<=0.6), 8+(6*2),
  ifelse( (rsuit==3) & (rvuln>0.4 & rvuln<=0.6), 9+(6*2), 
  ifelse( (rsuit==4) & (rvuln>0.4 & rvuln<=0.6), 10+(6*2),
  ifelse( (rsuit==5) & (rvuln>0.4 & rvuln<=0.6), 11+(6*2),
  ifelse( (rsuit==6) & (rvuln>0.4 & rvuln<=0.6), 12+(6*2),
          
  ifelse( (rsuit==2) & (rvuln>0.6 & rvuln<=0.8), 8+(6*3),
  ifelse( (rsuit==3) & (rvuln>0.6 & rvuln<=0.8), 9+(6*3), 
  ifelse( (rsuit==4) & (rvuln>0.6 & rvuln<=0.8), 10+(6*3),
  ifelse( (rsuit==5) & (rvuln>0.6 & rvuln<=0.8), 11+(6*3),
  ifelse( (rsuit==6) & (rvuln>0.6 & rvuln<=0.8), 12+(6*3),
          
  ifelse( (rsuit==2) & (rvuln>0.8 & rvuln<=1), 8+(6*4),
  ifelse( (rsuit==3) & (rvuln>0.8 & rvuln<=1), 9+(6*4), 
  ifelse( (rsuit==4) & (rvuln>0.8 & rvuln<=1), 10+(6*4),
  ifelse( (rsuit==5) & (rvuln>0.8 & rvuln<=1), 11+(6*4),
  ifelse( (rsuit==6) & (rvuln>0.8 & rvuln<=1), 12+(6*4),  0))))) ))))) ))))) ))))) )))))
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
mat_suit_afr <- raster('output/afr/various_rasters/extendedsuit_afr_0-6.tif')
mat_suit_afr_mask <- mask(mat_suit_afr, mat_land_use_afr, maskvalue=NA, updatevalue=NA)
mat_suit_afr_mask <- mask(mat_suit_afr_mask, mat_land_use_afr, maskvalue=0, updatevalue=0) #
mat_suit_afr_mask <- mask(mat_suit_afr_mask, mat_pa2, maskvalue=1, updatevalue=NA)

rm(reclassify.df, mat_land_use_afr, mat_pa, mat_pa2, mat_suit_afr)

# Biodiversity rasters (should already be masked)
mat_vuln_list <- list.files('output/afr/biodiversity_rasters/', full.names=FALSE)
mat_vuln_list <- grep(mat_vuln_list, pattern='.tif', value=TRUE)
mat_vuln_list <- grep(mat_vuln_list, pattern='std_mask_ext', value=TRUE)


#Transform to latlon for plotting
refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

bivmap_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_afr_mask)[1], ncols=dim(mat_suit_afr_mask)[2])

res_df_list2 <- list()

system.time(
  for (i in 1:length(mat_vuln_list)){
    mat_vuln_rescale <- raster(paste0('output/afr/biodiversity_rasters/', mat_vuln_list[i]))
    
    bivmap <- overlay(mat_vuln_rescale, mat_suit_afr_mask, fun=colmap_fxn) #very quick to run
    bivmap_latlon <- projectRaster(bivmap, bivmap_latlon, method='ngb')
    
    res_spdf <- rasterToPoints(bivmap_latlon)
    res_df <- as.data.frame(res_spdf)
    
    res_df_list2[[i]] <- res_df
  }
)
# 19s for 15 files 

plot(bivmap_latlon)

names(res_df_list2) <- gsub('m0_', 'biv_df_latlon_', mat_vuln_list)
names(res_df_list2) <- gsub('rescaled_twice_afr', 'biv_df_latlon_afr_rescaled_twice', names(res_df_list2))
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
#plot(mat_conc)

mat_suit_ssea <- raster('output/ssea/various_rasters/extendedsuit_ssea_0-6.tif')
mat_suit_ssea_mask <- mask(mat_suit_ssea, mat_land_use_ssea, maskvalue=NA, updatevalue=NA)
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_land_use_ssea, maskvalue=0, updatevalue=0) #
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_pa2, maskvalue=1, updatevalue=NA)
mat_suit_ssea_mask <- mask(mat_suit_ssea_mask, mat_conc2, maskvalue=1, updatevalue=NA)

#plot(mat_suit_ssea)
#plot(mat_suit_ssea_mask)

rm(reclassify.df, mat_land_use_ssea, mat_pa, mat_pa2, mat_conc, mat_conc2, mat_suit_ssea)

# Biodiversity rasters
mat_vuln_list <- list.files('output/ssea/biodiversity_rasters/', full.names=FALSE)
mat_vuln_list <- grep(mat_vuln_list, pattern='std_mask_ext', value=TRUE)


#Transform to latlon for plotting
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
bivmap_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_ssea_mask)[1], ncols=dim(mat_suit_ssea_mask)[2])

res_df_list2 <- list()

system.time(
  for (i in 1:length(mat_vuln_list)){
    mat_vuln_rescale <- raster(paste0('output/ssea/biodiversity_rasters/', mat_vuln_list[i]))

    bivmap <- overlay(mat_vuln_rescale, mat_suit_ssea_mask, fun=colmap_fxn) #very quick to run    
    bivmap_latlon <- projectRaster(bivmap, bivmap_latlon, method='ngb')
    
    res_spdf <- rasterToPoints(bivmap_latlon)
    res_df <- as.data.frame(res_spdf)

    res_df_list2[[i]] <- res_df
  }
)

plot(bivmap_latlon)

names(res_df_list2) <- gsub('m0_', 'biv_df_latlon_', mat_vuln_list)
names(res_df_list2) <- gsub('rescaled_twice_ssea', 'biv_df_latlon_ssea_rescaled_twice', names(res_df_list2))
names(res_df_list2) <- gsub('tif', 'csv', names(res_df_list2))


#save dfs in list to csv ... 
mapply(function(dfslistname, dfslist) 
  write.csv(dfslist, file = paste0("output/dfs_for_figs/ssea/", dfslistname), row.names = FALSE), 
  names(res_df_list2), res_df_list2)









############## MAKE INDIVIDUAL PLOTS (AFRICA) ###################
#### load afr_countries_wgs 
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')

refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
afr_countries_wgs <- st_crop(gadm_0, refExtent_wgs)

#### + loop for comb biv plots (AFRICA) ####
res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv_df", res_df_list)]
res_df_list <- res_df_list[grep("_ext", res_df_list)]
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
      geom_tile(data=res_df0, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_biv_full) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0.5,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none") 

    print(ggname)
    names(ggfiglist_afr_comb)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr_comb[[i]], width=169, units='mm', dpi=600)
    
  }
)

#names(ggfiglist_afr_comb)
#ggfiglist_afr_comb[[1]] 



#### + loop for taxa biv subplots (AFRICA) ####
#res_df_list <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
#res_df_list <- res_df_list[grep("biv_df", res_df_list)]
#res_df_list <- res_df_list[grep("_ext", res_df_list)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_afr <- list()

# Note that geom_tile will not produce the error but will instead produce a blank map
# troubleshooting error in geom_raster, column infinity ###
#Error in matrix(NA_character_, nrow = nrow, ncol = ncol) : invalid 'ncol' value (too large or NA) geom_raster
#https://github.com/tidyverse/ggplot2/blob/9eae13b3d17bde26cf9df649887b4a6bb2ac92ce/R/geom-raster.r#L72-L79
# likely very small jitterings in the data can cause this problem

#vulnA
res_df2 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[2])) #data with issues
res_df5 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[5])) #data no issues
res_df8 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[8])) #data no issues
res_df11 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[11])) #data no issues
all.equal(res_df2$x, res_df5$x, tolerance=0) #"Mean relative difference: 1.060193e-07"
all.equal(res_df2$y, res_df5$y, tolerance=0) #"Mean relative difference: 9.846104e-08"

# datasets with no issues have the same x and y
all.equal(res_df11$x, res_df5$x, tolerance=0) #TRUE
all.equal(res_df11$x, res_df8$x, tolerance=0) #TRUE
all.equal(res_df11$y, res_df5$y, tolerance=0) #TRUE
all.equal(res_df11$y, res_df8$y, tolerance=0) #TRUE

#vulnT
res_df12 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[12])) #with issues
res_df3 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[3]))
res_df6 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[6]))
res_df9 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[9]))

all.equal(res_df12$x, res_df3$x, tolerance=0) #"Mean relative difference: 9.618281e-08"
all.equal(res_df12$y, res_df3$y, tolerance=0) #"Mean relative difference: 1.204576e-07" 

# datasets with no issues have the same x and y
all.equal(res_df3$x, res_df6$x, tolerance=0)
all.equal(res_df3$y, res_df9$y, tolerance=0)


# even for vulnA vs vulnT , have same x and y
all.equal(res_df5$x, res_df6$x, tolerance=0) #TRUE
all.equal(res_df8$y, res_df9$y, tolerance=0) #TRUE


#### Should be safe to simply replace the x and y of problematic datasets ####
res_df2 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[2]))
res_df2$x <- res_df5$x
res_df2$y <- res_df5$y
write.csv(res_df2, paste0('output/dfs_for_figs/afr/', res_df_list[2]), row.names=FALSE)

res_df12 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[12]))
res_df12$x <- res_df3$x
res_df12$y <- res_df3$y
write.csv(res_df12, paste0('output/dfs_for_figs/afr/', res_df_list[12]), row.names=FALSE)


system.time(
  for (i in 1:length(res_df_list)){
    res_df0 <- read.csv(paste0('output/dfs_for_figs/afr/', res_df_list[i]))
    colnames(res_df0)[3] <- 'bivcol'
    res_df0$bivcol <- as.character(res_df0$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_afr[[i]] <- ggplot() +
      geom_tile(data=res_df0, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_biv_full) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none") 
    
    print(ggname)
    names(ggfiglist_afr)[i] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr[[i]], width=169, units='mm', dpi=100)
  }
)


# filter out richness figs
ggfigrich_afr <- ggfiglist_afr[grep('richness', names(ggfiglist_afr))]
names(ggfigrich_afr)

# filter out vuln all figs
ggfigvuln_afr <- ggfiglist_afr[grep('vuln_all', names(ggfiglist_afr))]
names(ggfigvuln_afr)

# filter out vulnT figs
ggfigvulnT_afr <- ggfiglist_afr[grep('vuln_threat', names(ggfiglist_afr))]
names(ggfigvulnT_afr)

#ggfigvuln_afr[[1]] #doesn't take too long to plot




############## MAKE INDIVIDUAL PLOTS (SSEA) ###################
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))

#gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0') #gadm countries
ssea_countries_wgs <- st_crop(gadm_0, refExtent_wgs)

#### + loop for comb biv plots (SSEA) ####
res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
res_df_list <- res_df_list[grep("biv_df", res_df_list)]
res_df_list <- res_df_list[grep("_ext", res_df_list)]
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
      scale_fill_manual(values = myColors_biv_full) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0.5,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none" )
    
    print(ggname)
    names(ggfiglist_ssea_comb)[i] <- paste0(ggname)
    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea_comb[[i]], width=169, units='mm', dpi=300)
  }
) #0.19s


#### + loop for taxa biv subplots (SSEA) ####
#res_df_list <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
#res_df_list <- res_df_list[grep("biv_df", res_df_list)]
res_df_list <- res_df_list[grep("comb", res_df_list, invert=TRUE)]
ggfiglist_ssea <- list()

# troubleshooting error in geom_raster, column infinity ###
# vulnT
res_df3 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[3])) #issue
res_df6 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[6])) 
res_df9 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[9]))
res_df12 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[12]))
all.equal(res_df6$x, res_df3$x, tolerance=0) #"Mean relative difference: 1.747706e-07"
all.equal(res_df6$y, res_df3$y, tolerance=0) #"Mean relative difference: 1.316584e-07"

# datasets with no issues have the same x and y
all.equal(res_df6$x, res_df9$x, tolerance=0)
all.equal(res_df6$y, res_df9$y, tolerance=0)
all.equal(res_df6$x, res_df12$x, tolerance=0)
all.equal(res_df12$y, res_df12$y, tolerance=0)

#vulnA
res_df8 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[8])) #data with issues
res_df2 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[2])) #data no issues
res_df5 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[5])) #data no issues
res_df11 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[11])) #data no issues
all.equal(res_df8$x, res_df5$x, tolerance=0) #"Mean relative difference: 1.879519e-07"
all.equal(res_df8$y, res_df5$y, tolerance=0) #"Mean relative difference: 8.762884e-08"

# datasets with no issues have the same x and y
all.equal(res_df2$x, res_df5$x, tolerance=0) #TRUE
all.equal(res_df2$y, res_df5$y, tolerance=0) #TRUE
all.equal(res_df11$x, res_df5$x, tolerance=0) #TRUE
all.equal(res_df11$y, res_df8$y, tolerance=0) #TRUE

# even for vulnA vs vulnT , have same x and y
all.equal(res_df5$x, res_df6$x, tolerance=0) #TRUE
all.equal(res_df8$y, res_df9$y, tolerance=0) #TRUE


#### Should be safe to simply replace the x and y of problematic datasets ####
res_df3 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[3]))
res_df3$x <- res_df6$x
res_df3$y <- res_df6$y
write.csv(res_df3, paste0('output/dfs_for_figs/ssea/', res_df_list[3]), row.names=FALSE)

res_df8 <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[8]))
res_df8$x <- res_df2$x
res_df8$y <- res_df2$y
write.csv(res_df8, paste0('output/dfs_for_figs/ssea/', res_df_list[8]), row.names=FALSE)

system.time(
  for (i in 1:length(res_df_list)){
    res_df <- read.csv(paste0('output/dfs_for_figs/ssea/', res_df_list[i]))
    colnames(res_df)[3] <- 'bivcol'
    res_df$bivcol <- as.character(res_df$bivcol)
    
    #make ggplot
    ggname <- gsub('biv_df', 'bivmap', res_df_list[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_ssea[[i]] <- ggplot() +
      geom_tile(data=res_df, aes(y=y, x=x, fill=bivcol)) +
      scale_fill_manual(values = myColors_biv_full) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA, expand=FALSE)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(), 
        text = element_blank(), 
        legend.position="none" )
    
    print(ggname)
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





############## MAKE COMBINED PLOTS FOR PAPER ###################

# + Manually calculating the dimensions of the figure ####
mat_vuln_ssea <- raster('output/ssea/biodiversity_rasters/rescaled_twice_ssea_combspp_vuln_threat_std_mask_ext.tif')
mat_vuln_afr <- raster('output/afr/biodiversity_rasters/rescaled_twice_afr_combspp_vuln_threat_std_mask_ext.tif')

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
gg_bivcol_legend_rich <- ggplot(data=zz2, aes(x=vulnplot, y=suitplot)) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_biv_full)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), labels=c(NA, 2,3,4,5,6)) +
    theme_cowplot(font_size=12) +
  ylab('No. of dry/cold stress conditions met') + xlab('Species richness') +
  theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm"))

gg_bivcol_legend_vuln <- ggplot(data=zz2, aes(x=vulnplot, y=suitplot)) +
  geom_tile(aes(fill = factor(bivcol))) +
  scale_fill_manual(values = (myColors_biv_full)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0,0), labels=c(0, 2,3,4,5,6)) +
  theme_cowplot(font_size=12) +
  ylab('No. of dry/cold stress conditions met') + xlab('Extinction vulnerability') +
  theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm"))


########### + Concessions and PAs Overlay ###############

### + Load Concessions Point Data 
afr_pa_df <- read.csv('output/dfs_for_figs/afr/pa_df.csv')
ssea_pa_df <- read.csv('output/dfs_for_figs/ssea/pa_df_LU.csv')

ssea_conc_df <- read.csv('output/dfs_for_figs/ssea/conc_df_raster.csv')

conc_sf <- st_read('data/conc_sf_landmatrix.gpkg')
ssea_conc_sf <- st_crop(conc_sf, extent(ssea_countries_wgs)) 
afr_conc_sf <- st_crop(conc_sf, extent(afr_countries_wgs)) 

names(ggfigrich_ssea)
names(ggfigrich_afr)



#####   + Multipanel VulnT (Fig S3 for paper) #####
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

cowplot::save_plot("output/results/figS3.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85) 





##### + Multipanel VulnA #####
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

#cowplot::save_plot("output/results/figS3_vulnA.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85) 




# + Multipanel Richness ####
ggfigrich_afr_comb_pa <- ggfiglist_afr_comb[[1]] + geom_raster(data=afr_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)
ggfigrich_ssea_comb_pa <- ggfiglist_ssea_comb[[1]] + geom_raster(data=ssea_pa_df, aes(y=y, x=x), fill="black", alpha=0.6)

top_sub_afr <- plot_grid(ggfigrich_afr[[1]], ggfigrich_afr[[2]], ggfigrich_afr[[3]],  ggfigrich_afr[[4]],
                         labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

top_afr <- plot_grid(ggfigrich_afr_comb_pa,  top_sub_afr, labels = c('A', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

bottom_sub_ssea <- plot_grid(ggfigrich_ssea[[1]], ggfigrich_ssea[[2]], ggfigrich_ssea[[3]], ggfigrich_ssea[[4]],
                             labels = c(NA, NA, NA, NA), align = 'h', ncol=4, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.0, y=0.05, width = 0.15, height = 0.15) +
  draw_image("images/170px_bird_icon.jpg",     x=0.22, y=0.01, width = 0.18, height = 0.18) +
  draw_image("images/170px_elephant_icon.png", x=0.47, y=0.03, width = 0.17, height = 0.17) +
  draw_image("images/170px_snake_icon.png",     x=0.72, y=0.05, width = 0.17, height = 0.17)

bottom_ssea <- plot_grid(ggfigrich_ssea_comb_pa,  bottom_sub_ssea, labels = c('B', ''), ncol = 1, rel_heights = c(2, 1), label_size=8, align="v")

gg_bivcol_legend_rich_taxa <- gg_bivcol_legend_rich + theme_cowplot(font_size=10) + theme(legend.position="none", plot.margin = unit(c(0,0,0,0),"cm")) 

gg_topbottom2 <- plot_grid(top_afr, bottom_ssea, ncol = 1, label_size=8, align="v") +
  draw_plot(gg_bivcol_legend_rich_taxa, x=0.0, y=0.68, width=0.29, height=0.24) 

#cowplot::save_plot("output/results/figS3_richness.png", gg_topbottom2, base_height = overall_height+0.05, base_width=6.85)
