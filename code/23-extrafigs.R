# Additional figures not in paper


#### load afr_countries_wgs ####
gadm_0 <- st_read(paste0(GISfolder, 'GADM/gadm36_levels_gpkg/gadm36_levels.gpkg'), layer='level0')

listofafricancountries <- read.csv('data/listofafricancountries.csv')
listofafricancountries <- listofafricancountries$x

gadm_afr <- gadm_0 %>% dplyr::filter(GID_0 %in% listofafricancountries) #0.11s
refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
afr_countries_wgs <- st_crop(gadm_afr, refExtent_wgs)


#### load ssea_countries_wgs ####
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
ssea_countries_wgs <- st_crop(gadm_0, refExtent_wgs)



###### Supp Figs - mask out water NAs, but don't rescale! keep # of spp #####

#### (A) Africa --------------------------------------------------------#####

#### +Load biodiversity rasters 
# Load list of biodiversity rasters, apply raster rescale fxn
rlist <- list.files('output/afr/biodiversity_rasters_cci/', full.names = TRUE)
#rlist <- grep(rlist, pattern='combspp', inv=TRUE, value=TRUE) #excld 'combspp'
#rlist <- grep(rlist, pattern='rescaled', inv=TRUE, value=TRUE) #excld 'combspp'
rlist <- grep(rlist, pattern='mask', inv=TRUE, value=TRUE) #excld 'combspp'
rlist <- grep(rlist, pattern='tif$', value=TRUE) 
rlistread <- sapply(rlist, stack)


#### + Mask out the NAs (water) ######
plot(mat_suit)
freq(mat_suit)

rstack <- stack(rlistread)
rstack_maskNA <- mask(rstack, mat_suit, maskvalue=NA, updatevalue=NA)

plot(rstack_maskNA)


rstack_figs3_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit)[1], ncols=dim(mat_suit)[2])
plot(rstack_figs3_latlon)

rstack_figs3_latlon <- projectRaster(rstack_maskNA, rstack_figs3_latlon, crs=crs_wgs, method='bilinear')
plot(rstack_figs3_latlon[[1]])
#plot(rstack_figs3_latlon)


res_df_list2 <- list()
for (i in 1:length(names(rstack_figs3_latlon))){
  res_spdf <- rasterToPoints(rstack_figs3_latlon[[i]])
  res_df <- as.data.frame(res_spdf)
  res_df_list2[[i]] <- res_df
  
  res_df_name <- gsub('m0', 'suppfig_df', names(rstack_figs3_latlon[[i]]))
  #res_df_name <- gsub('', 'DD_pred', res_df_name)
  res_df_name <- paste0(res_df_name, '.csv')
  names(res_df_list2)[i] <- paste0(res_df_name)
}

ggfiglist_afr <- list()

names(res_df_list2)

#### loop for  plots
system.time(
  for (i in 1:length(res_df_list2)){
    res_df <- res_df_list2[[i]]
    colnames(res_df)[3] <- 'vuln'
    
    #make ggplot
    ggname <- gsub('suppfig_df', 'plot', names(res_df_list2[i]))
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_afr[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red")) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank()) + #,
      #text = element_blank() ) #,
      #legend.position="none" )
      labs(x = NULL, y=NULL, fill=NULL)
    
    print(ggname)
    names(ggfiglist_afr)[[i]] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr[[i]], width=169, units='mm', dpi=600)
    
  }
)

names(ggfiglist_afr)
#ggfiglist_afr[[1]]
names(rstack_maskNA)
maxValue(rstack_maskNA)

#subplot_height_afr = (4)*(dim(mat_suit)[1]/dim(mat_suit)[2])*3

# This was before adding reptile

# Richness Supp Fig (mask out NAs, not rescaled)
taxa_afr_ncol2 <- plot_grid(ggfiglist_afr[[1]], ggfiglist_afr[[4]], ggfiglist_afr[[7]],
                            labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.81, y=0.9, width = 0.09, height = 0.09) +
  draw_image("images/170px_bird_icon.jpg",     x=0.8, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.8, y=0.23, width = 0.1, height = 0.1)

cowplot::save_plot("output/results/figS7_afr_richness_taxa.png", taxa_afr_ncol2, base_height=8.48, base_width=6.65)


# Vuln All Supp Fig (mask out NAs, not rescaled)
taxa_afr_ncol2 <- plot_grid(ggfiglist_afr[[2]], ggfiglist_afr[[5]], ggfiglist_afr[[8]],
                            labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.81, y=0.9, width = 0.09, height = 0.09) +
  draw_image("images/170px_bird_icon.jpg",     x=0.8, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.8, y=0.23, width = 0.1, height = 0.1)

cowplot::save_plot("output/results/figS5_afr_vuln_all_taxa.png", taxa_afr_ncol2, base_height=8.48, base_width=6.65)


# Vuln Threat Supp Fig (mask out NAs, not rescaled)
taxa_afr_ncol1 <- plot_grid(ggfiglist_afr[[3]], ggfiglist_afr[[6]], ggfiglist_afr[[9]],
                            labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.81, y=0.9, width = 0.09, height = 0.09) +
  draw_image("images/170px_bird_icon.jpg",     x=0.8, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.8, y=0.23, width = 0.1, height = 0.1)

cowplot::save_plot("output/results/figS5B_afr_vuln_threat_taxa.png", taxa_afr_ncol1, base_height=8.48, base_width=6.65) #169mm = 6.65 inches, 100mm=4 inches



#### (B) SSEA --------------------------------------------------------#####

#### +Load biodiversity rasters 
# Load list of biodiversity rasters
rlist <- list.files('output/ssea/biodiversity_rasters/', full.names = TRUE)
rlist <- grep(rlist, pattern='tif$', value=TRUE) #only 'tif'
rlist <- grep(rlist, pattern='mask', inv=TRUE, value=TRUE) 

#rlist <- grep(rlist, pattern='vuln_all', value=TRUE) 
rlist

#### + Mask out the NAs (water) ######
plot(mat_suit)
freq(mat_suit)

rstack <- stack(rlist)
#rstack <- crop(rstack, extent(ssea_countries))
extent(rstack)
extent(mat_suit)
extent(ssea_countries)
rstack_maskNA <- mask(rstack, mat_suit, maskvalue=NA, updatevalue=NA)

plot(rstack_maskNA[[1]])
plot(st_geometry(ssea_countries), add=TRUE)

names(rstack_maskNA)
maxValue(rstack_maskNA)

# USE rstack_maskNA to make Supp Figs 1-4 for individual taxa, before rescale !!! see below code


rstack_figs3_latlon <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit)[1], ncols=dim(mat_suit)[2])
plot(rstack_figs3_latlon)

rstack_figs3_latlon <- projectRaster(rstack_maskNA, rstack_figs3_latlon, crs=crs_wgs, method='bilinear')
plot(rstack_figs3_latlon[[1]])

i=1
res_df_list2 <- list()
str(rstack_figs3_latlon)

for (i in 1:length(names(rstack_figs3_latlon))){
  res_spdf <- rasterToPoints(rstack_figs3_latlon[[i]])
  res_df <- as.data.frame(res_spdf)
  res_df_list2[[i]] <- res_df
  
  res_df_name <- gsub('m0', 'suppfig_df', names(rstack_figs3_latlon[[i]]))
  #res_df_name <- gsub('', 'DD_pred', res_df_name)
  res_df_name <- paste0(res_df_name, '.csv')
  names(res_df_list2)[i] <- paste0(res_df_name)
}

#0.23s for 12 files

names(res_df_list2)
ggfiglist_ssea <- list()

i=1
#### loop for plots
system.time(
  for (i in 1:length(res_df_list2)){
    res_df <- res_df_list2[[i]]
    colnames(res_df)[3] <- 'vuln'
    
    #make ggplot
    ggname <- gsub('suppfig_df', 'plot', names(res_df_list2[i]))
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_ssea[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red")) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank()) + #,
      #text = element_blank() ) #,
      #legend.position="none" )
      labs(x = NULL, y=NULL, fill=NULL)
    
    print(ggname)
    names(ggfiglist_ssea)[[i]] <- paste0(ggname)
    
    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea[[i]], width=169, units='mm', dpi=600)
    
  }
)

names(ggfiglist_ssea)
ggfiglist_ssea[[1]]

5.1/4*6.65

subplot_height_ssea = (4)*(dim(mat_suit)[1]/dim(mat_suit)[2])*3
subplot_height_ssea = (4)*(477/950)*3


# Richness Supp Fig (mask out NAs, not rescaled)
taxa_ssea_ncol3 <- plot_grid(ggfiglist_ssea[[1]], ggfiglist_ssea[[4]], ggfiglist_ssea[[6]],
                             labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.9, y=0.9, width = 0.1, height = 0.1) +
  draw_image("images/170px_bird_icon.jpg",     x=0.9, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.9, y=0.23, width = 0.09, height = 0.09)

cowplot::save_plot("output/results/figS8_ssea_richness_taxa.png", taxa_ssea_ncol3, base_height=8.48, base_width=6.65)


# Vuln All Supp Fig (mask out NAs, not rescaled)
taxa_ssea_ncol2 <- plot_grid(ggfiglist_ssea[[2]], ggfiglist_ssea[[5]], ggfiglist_ssea[[8]],
                             labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.9, y=0.9, width = 0.1, height = 0.1) +
  draw_image("images/170px_bird_icon.jpg",     x=0.9, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.9, y=0.23, width = 0.09, height = 0.09)

cowplot::save_plot("output/results/figS6_ssea_vuln_all_taxa.png", taxa_ssea_ncol2, base_height=8.48, base_width=6.65) #169mm = 6.65 inches, 100mm=4 inche


# Vuln Threat Supp Fig (mask out NAs, not rescaled)
taxa_ssea_ncol1 <- plot_grid(ggfiglist_ssea[[3]], ggfiglist_ssea[[6]], ggfiglist_ssea[[9]],
                             labels = c('(a)', '(b)', '(c)'), ncol=1, label_size=8) +
  draw_image("images/170px_frog_icon.png",     x=0.9, y=0.9, width = 0.1, height = 0.1) +
  draw_image("images/170px_bird_icon.jpg",     x=0.9, y=0.55, width = 0.1, height = 0.1) +
  draw_image("images/170px_elephant_icon.png", x=0.9, y=0.23, width = 0.09, height = 0.09)

cowplot::save_plot("output/results/figS6B_ssea_vuln_threat_taxa.png", taxa_ssea_ncol1, base_height=8.48, base_width=6.65) #169mm = 6.65 inches, 100mm=4 inche



######## PLOTS without standardizing across continents ######

#### (A) Africa - Loop for plots afr (unstandardized with Asia) #####
ggfiglist_afr_fig2 <- list()

rlist_fig2_df <- list.files('output/dfs_for_figs/afr/', full.names=FALSE)
rlist_fig2_df <- rlist_fig2_df[grep("fig2_df_latlon", rlist_fig2_df)]

i=1
system.time(
  for (i in 1:length(rlist_fig2_df)){
    res_df <- read.csv(paste0('output/dfs_for_figs/afr/', rlist_fig2_df[i]))
    colnames(res_df)[3] <- 'vuln'

    #make ggplot
    ggname <- gsub('fig2_df_', 'fig2_plot_', rlist_fig2_df[i])
    ggname <- gsub('csv', 'png', ggname)

    ggfiglist_afr_fig2[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red"), breaks=seq(0,1,by=0.5), labels=c(0,0.5,1), limits=c(0,1)) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(t=0,b=0,l=0,r=0),"cm"),
        rect = element_blank(),
        text = element_text(size=8),
        legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm") ) +#,
      #legend.position="none" )
      labs(x = NULL, y=NULL, fill=NULL)

    print(ggname)
    names(ggfiglist_afr_fig2)[[i]] <- paste0(ggname)

    #ggsave(filename=paste0("output/results/results_afr/", ggname), plot=ggfiglist_afr[[i]], width=169, units='mm', dpi=600)

  }
)
#1.14s
#?theme

fig2_ace <- plot_grid(ggfiglist_afr_fig2[[1]], ggfiglist_afr_fig2[[2]], ggfiglist_afr_fig2[[3]],
                      labels = c('(a)', '(c)', '(e)'), ncol=1, label_size=8)
#scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom")

#cowplot::save_plot("output/results/fig2ace_afr_suit_rich_vuln.png", fig2_ace, base_height=5.1, base_width=6.85/2) #169mm = 6.85 inches, 100mm=4 inches




#### (B) SSEA - Loop for plots ssea (unstandardized with Afr) #####

ggfiglist_ssea_fig2 <- list()

rlist_fig2_df <- list.files('output/dfs_for_figs/ssea/', full.names=FALSE)
rlist_fig2_df <- rlist_fig2_df[grep("fig2_df_latlon", rlist_fig2_df)]

system.time(
  for (i in 1:length(rlist_fig2_df)){
    res_df <- read.csv(paste0('output/dfs_for_figs/ssea/', rlist_fig2_df[i]))
    colnames(res_df)[3] <- 'vuln'

    #make ggplot
    ggname <- gsub('fig2_df_', 'fig2_plot_', rlist_fig2_df[i])
    ggname <- gsub('csv', 'png', ggname)

    ggfiglist_ssea_fig2[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red"), breaks=seq(0,1,by=0.5), labels=c(0,0.5,1), limits=c(0,1)) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(),
      #text = element_blank() ) #,
      legend.position="none" ) +
      labs(x = NULL, y=NULL, fill=NULL)

    print(ggname)
    names(ggfiglist_ssea_fig2)[[i]] <- paste0(ggname)

    #ggsave(filename=paste0("output/results/results_ssea/", ggname), plot=ggfiglist_ssea[[i]], width=169, units='mm', dpi=600)

  }
)
#0.76s

fig2_bdf <- plot_grid(ggfiglist_ssea_fig2[[3]], ggfiglist_ssea_fig2[[1]], ggfiglist_ssea_fig2[[2]],
                      labels = c('(b)', '(d)', '(f)'), ncol=1, label_size=8)
#scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom")

#cowplot::save_plot("output/results/fig2bdf_ssea_suit_rich_vuln.png", fig2_bdf, base_height=5.1, base_width=6.85/2) #169mm = 6.85 inches, 100mm=4 inches

fig2 <- plot_grid(ggfiglist_afr_fig2[[3]], ggfiglist_ssea_fig2[[3]], ggfiglist_afr_fig2[[1]], ggfiglist_ssea_fig2[[1]], ggfiglist_afr_fig2[[2]], ggfiglist_ssea_fig2[[2]],
                      labels = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)','(g)'), ncol=2, label_size=8)

cowplot::save_plot("output/results/fig2_suit_rich_vuln_height5.1_0-1.png", fig2, base_height=5.1, base_width=6.85) #169mm = 6.85 inches, 100mm=4 inches # base_height=5.1,


###################################################
###### [Not used in paper] Make dfs for fig w/ only water masked out (rescaled to standardized btw Afr and Asia) #####
# other land uses & PAs not masked in this fig

#Transform to latlon for plotting
rstack_fig2_std_afr <- stack(mat_suit_afr, richness_comb_rescale_afr, vuln_comb_rescale_afr, vulnT_comb_rescale_afr)
refExtent_wgs <- extent(c(xmin=-25.5, xmax=63.5, ymin=-35, ymax=19))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
rstack_fig2_latlon_afr <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_afr)[1], ncols=dim(mat_suit_afr)[2])
rstack_fig2_latlon_afr <- projectRaster(rstack_fig2_std_afr, rstack_fig2_latlon_afr, crs=crs_wgs, method='bilinear')
plot(rstack_fig2_latlon_afr)

rstack_fig2_std_ssea <- stack(mat_suit_ssea, richness_comb_rescale_ssea, vuln_comb_rescale_ssea, vulnT_comb_rescale_ssea)
refExtent_wgs <- extent(c(xmin=69.25, xmax=158.4, ymin=-12.5, ymax=30.5))
crs_wgs <- crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
rstack_fig2_latlon_ssea <- raster(ext=refExtent_wgs, crs=crs_wgs, vals=0, nrows=dim(mat_suit_ssea)[1], ncols=dim(mat_suit_ssea)[2])
rstack_fig2_latlon_ssea <- projectRaster(rstack_fig2_std_ssea, rstack_fig2_latlon_ssea, crs=crs_wgs, method='bilinear')
plot(rstack_fig2_latlon_ssea)

#names(rstack_fig2_latlon_afr)[1] <- 'suitability_afr'

# Loop to make dfs
rlist_fig2_std <- c(as.list(rstack_fig2_latlon_afr), as.list(rstack_fig2_latlon_ssea))

rlist_fig2_df <- list()

for (i in 1:length(rlist_fig2_std)){
  res_spdf <- rasterToPoints(rlist_fig2_std[[i]])
  res_df <- as.data.frame(res_spdf)
  rlist_fig2_df[[i]] <- res_df
  
  res_df_name <- gsub('m0', 'suppfig_df', names(rlist_fig2_std[[i]]))
  res_df_name <- paste0('xtrafig2_df_', names(rlist_fig2_std[[i]]), '_unmasked.csv')
  names(rlist_fig2_df)[i] <- paste0(res_df_name)
}

names(rlist_fig2_df)

#save dfs in list to csv ... 
mapply(function(dfslistname, dfslist) 
  write.csv(dfslist, file = paste0("output/dfs_for_figs/", dfslistname), row.names = FALSE), 
  names(rlist_fig2_df), rlist_fig2_df)


#### (A) Africa - Loop for plots afr (standardized with Asia, unmasked LU) #####
ggfiglist_afr_fig2 <- list()

rlist_fig2_df <- list.files('output/dfs_for_figs/', full.names=FALSE)
rlist_fig2_df <- rlist_fig2_df[grep("xtrafig2_df_", rlist_fig2_df)]
rlist_fig2_df <- rlist_fig2_df[grep("afr", rlist_fig2_df)]


i=1

system.time(
  for (i in 1:length(rlist_fig2_df)){
    res_df <- read.csv(paste0('output/dfs_for_figs/', rlist_fig2_df[i]))
    colnames(res_df)[3] <- 'vuln'
    
    #make ggplot
    ggname <- gsub('fig2_df_', 'fig2_', rlist_fig2_df[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_afr_fig2[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red"), breaks=seq(0,1,by=0.5), labels=c(0,0.5,1), limits=c(0,1)) +
      geom_sf(data = afr_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(t=0,b=0,l=0,r=0),"cm"),
        rect = element_blank(),
        text = element_text(size=8),
        legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm") ) +#,
      #legend.position="none" )
      labs(x = NULL, y=NULL, fill=NULL) 
    
    print(ggname)
    names(ggfiglist_afr_fig2)[[i]] <- paste0(ggname)
  }
)


#### (B) SSEA - Loop for plots ssea (standardized with Afr, unmasked LU) #####
ggfiglist_ssea_fig2 <- list()

rlist_fig2_df <- list.files('output/dfs_for_figs/', full.names=FALSE)
rlist_fig2_df <- rlist_fig2_df[grep("xtrafig2_df_", rlist_fig2_df)]
rlist_fig2_df <- rlist_fig2_df[grep("ssea", rlist_fig2_df)]

system.time(
  for (i in 1:length(rlist_fig2_df)){
    res_df <- read.csv(paste0('output/dfs_for_figs/', rlist_fig2_df[i]))
    colnames(res_df)[3] <- 'vuln'
    
    #make ggplot
    ggname <- gsub('fig2_df_', 'fig2_', rlist_fig2_df[i])
    ggname <- gsub('csv', 'png', ggname)
    
    ggfiglist_ssea_fig2[[i]] <- ggplot() +
      geom_raster(data=res_df, aes(y=y, x=x, fill=vuln)) +
      scale_fill_gradientn(colours=c("gray90","lightblue","yellow","orangered","red"), breaks=seq(0,1,by=0.5), labels=c(0,0.5,1), limits=c(0,1)) +
      geom_sf(data = ssea_countries_wgs, fill=NA, lwd=0.25) +
      coord_sf(datum = NA)  +
      theme(
        plot.margin = unit(c(0,0,0,0),"cm"),
        rect = element_blank(),
        #text = element_blank() ) #,
        legend.position="none" ) +
      labs(x = NULL, y=NULL, fill=NULL) 
    
    print(ggname)
    names(ggfiglist_ssea_fig2)[[i]] <- paste0(ggname)
    
  }
)


# For some reason, there's an issue with the ssea suitability fig.... 
cowplot::save_plot("output/results/xtrafig2_suit_rich_vuln_stdscale2.png", ggfiglist_ssea_fig2[[4]]) 
#Error in matrix(NA_character_, nrow = nrow, ncol = ncol) : 
# invalid 'ncol' value (too large or NA)

# Combined fig 
fig2_std_all3 <- plot_grid(ggfiglist_afr_fig2[[4]], ggfiglist_ssea_fig2[[4]], ggfiglist_afr_fig2[[1]], ggfiglist_ssea_fig2[[1]], ggfiglist_afr_fig2[[2]], ggfiglist_ssea_fig2[[2]], ggfiglist_afr_fig2[[3]], ggfiglist_ssea_fig2[[3]], 
                           labels = c('A', 'B', 'C', 'D', 'E', 'F','G', 'H'), ncol=2, label_size=8) 

cowplot::save_plot("output/results/fig2_suit_rich_vuln_vulnall_stdscale.png", fig2_std_all3, base_height=8, base_width=6.85) #169mm = 6.85 inches, 100mm=4 inches # base_height=5.1,


