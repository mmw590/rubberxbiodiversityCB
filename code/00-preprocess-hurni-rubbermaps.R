#### 00-preprocess-hurni-rubbermaps.R ####

# Preparing Husni MMSEA rubber map for use in other analyses
#### --> create input file "mmsea2014.tif" to be used in 02-ssea_rasters.R

# Libraries
library(raster)
library(dplyr)

# Directories
GISfolder <- "C:/Users/bop17mw/Desktop/GIS_Files/"

mmsea <- raster(paste0(GISfolder, "Hurni/MODIS_MSEA_classification/MMSEA_MODIS_LCC_01-14_rfcEstimation.tif"))
str(mmsea)
plot(mmsea)
crs(mmsea) #utm48
str(values(mmsea)) #"land cover" classes

#### Create map for year 2000 -----####
# Replace raster values as follows:
# if value >200 & ends with 1, it was 2=F in 2000.
# if value >200 & ends with 2, it was 3=DF in 2000.
# if value >200 & ends with 3, it was 5=LVA in 2000.

lcc <- unique(values(mmsea))
reclassify.df <- data.frame(oldval=lcc, newval00=lcc) %>% arrange(oldval)
reclassify.df[reclassify.df$oldval %in% lcc[grep("[0-9]{2,3}1$", lcc)], ]$newval00 <- 2
reclassify.df[reclassify.df$oldval %in% lcc[grep("[0-9]{2,3}2$", lcc)], ]$newval00 <- 3
reclassify.df[reclassify.df$oldval %in% lcc[grep("[0-9]{2,3}3$", lcc)], ]$newval00 <- 5

mmsea2000 <- subs(mmsea, reclassify.df, by='oldval', which='newval00',subsWithNA=FALSE)

#check if values changed correctly
table(values(mmsea), values(mmsea2000))


#### Create map for year 2005 -----####
# Replace raster values as follows:
# Use the same replacement values as 2000, except:
# for values c(231:233), replace with 8=Rubber in 2005

reclassify.df$newval05 <- reclassify.df$newval00
reclassify.df[reclassify.df$oldval %in% 231:233, ]$newval05 <- 8
reclassify.df

mmsea2005 <- subs(mmsea, reclassify.df, by='oldval', which='newval05',subsWithNA=FALSE)

#check if values changed correctly
table(values(mmsea), values(mmsea2005))


#### Create map for year 2008 -----####
# Replace raster values as follows:
# Use the same replacement values as 2005, except:
# for values c(261:263), replace with 8=Rubber in 2008
# for values c(1061), replace with 10=Sugarcane in 2008

reclassify.df$newval08 <- reclassify.df$newval05
reclassify.df[reclassify.df$oldval %in% 261:263, ]$newval08 <- 8
reclassify.df[reclassify.df$oldval %in% 1061, ]$newval08 <- 10
reclassify.df

mmsea2008 <- subs(mmsea, reclassify.df, by='oldval', which='newval08',subsWithNA=FALSE)

#check if values changed correctly
table(values(mmsea), values(mmsea2008))


#### Create map for year 2011 -----####
# Replace raster values as follows:
# Use the same replacement values as 2008, except:
# for values c(291:292), replace with 8=Rubber in 2011
# for values c(1092), replace with 10=Sugarcane in 2011

reclassify.df$newval11 <- reclassify.df$newval08
reclassify.df[reclassify.df$oldval %in% 291:292, ]$newval11 <- 8
reclassify.df[reclassify.df$oldval %in% 1092, ]$newval11 <- 10
reclassify.df

mmsea2011 <- subs(mmsea, reclassify.df, by='oldval', which='newval11',subsWithNA=FALSE)

#check if values changed correctly
table(values(mmsea), values(mmsea2011))


#### Create map for year 2014 -----####
# Replace raster values as follows:
# Use the same replacement values as 2008, except:
# for values c(321:322), replace with 8=Rubber in 2014

reclassify.df$newval14 <- reclassify.df$newval11
reclassify.df[reclassify.df$oldval %in% 321:322, ]$newval14 <- 8
reclassify.df

mmsea2014 <- subs(mmsea, reclassify.df, by='oldval', which='newval14',subsWithNA=FALSE)

#check if values changed correctly
table(values(mmsea), values(mmsea2014))

writeRaster(mmsea2014, filename=paste0(GISfolder,"Hurni/byyear/mmsea2014.tif"), format="GTiff", prj=TRUE)   #### to be used in 02-ssea_rasters.R


# # RasterStack all layers
# mmsea_allyears <- stack(mmsea2000, mmsea2005, mmsea2008, mmsea2011, mmsea2014)
# names(mmsea_allyears) <- paste0("mmsea", c(2000,2005,2008,2011,2014))
# 
# writeRaster(mmsea_allyears, filename=paste0(GISfolder,"Hurni/byyear/mmsea_allyears.tif"), format="GTiff", prj=TRUE)
# writeRaster(mmsea_allyears, filename=paste0(GISfolder,"Hurni/byyear/", names(mmsea_allyears)), bylayer=TRUE, format="GTiff")