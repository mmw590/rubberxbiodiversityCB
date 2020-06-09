#### rubberxbiodiversityCB 
#### 00-preprocess-landmatrix.R #### 

# Code for processing LandMatrix data for plotting
# landmatrix, data downloaded 2019-04-09
# code not tested after package updates

library(dplyr)
library(sf)
library(ggplot2)


landmatrix <- read.csv('data/landmatrix/alldeals1.csv') #all data, unfiltered, downloaded 2019-04-09
rubberID <- read.csv('data/landmatrix/rubber-listofdealIDs.csv') #rubber deals only, and extracted only the Deal.ID column

landmatrix3 <- left_join(rubberID, landmatrix)

write.csv(landmatrix3, 'data/landmatrix/allrubberdeals.csv')


#convert blanks to NAs
landmatrix3 <- landmatrix3 %>% mutate_all(na_if,"")

#landmatrix3 <- landmatrix3 %>% mutate_all(na_if," ")

#remove empty cols
landmatrix4 <- Filter(function(x)!all(is.na(x)), landmatrix3)

names(landmatrix4)

landmatrix5 <- landmatrix4[, 1:72] #until "Comment.on.implementation.status"

library(tidyr)
names(landmatrix5)

landmatrix.loc.accuracy <- landmatrix5 %>% dplyr::select(1:10, 61:72, ends_with('Spatial.accuracy.level')) %>%
  gather(key='loc', value='loc.accuracy', Location.1..Spatial.accuracy.level:Location.8..Spatial.accuracy.level, na.rm=TRUE) #%>% dplyr::select(loc, loc.accuracy)
landmatrix.loc.accuracy$loc <- gsub('..Spatial.accuracy.level', '', landmatrix.loc.accuracy$loc)


landmatrix.loc.loc <- landmatrix5 %>% dplyr::select(1, matches('Location.[1-9]..Location')) %>%
  gather(key='loc', value='loc.loc', Location.1..Location:Location.8..Location, na.rm=TRUE) #%>% 
#dplyr::select(loc.loc)
landmatrix.loc.loc$loc <- gsub('..Location', '', landmatrix.loc.loc$loc)

landmatrix.loc.lat <- landmatrix5 %>% dplyr::select(1, matches('Location.[1-9]..Latitude')) %>%
  gather(key='loc', value='loc.lat', Location.1..Latitude:Location.8..Latitude, na.rm=TRUE) #%>% 
#dplyr::select(loc.lat)
landmatrix.loc.lat$loc <- gsub('..Latitude', '', landmatrix.loc.lat$loc)

landmatrix.loc.lon <- landmatrix5 %>% dplyr::select(1, matches('Location.[1-9]..Longitude')) %>%
  gather(key='loc', value='loc.lon', Location.1..Longitude:Location.8..Longitude, na.rm=TRUE) #%>% 
#dplyr::select(loc.lon)
landmatrix.loc.lon$loc <- gsub('..Longitude', '', landmatrix.loc.lon$loc)

landmatrix.loc.country <- landmatrix5 %>% dplyr::select(1:10, 61:72, matches('Location.[1-9]..Target.country')) %>%
  gather(key='loc', value='loc.country', Location.1..Target.country:Location.8..Target.country, na.rm=TRUE) 
landmatrix.loc.country$loc <- gsub('..Target.country', '', landmatrix.loc.country$loc)

landmatrix.loc.comment <- landmatrix5 %>% dplyr::select(1:10, 61:72, matches('Location.[1-9]..Comment.on.location')) %>% 
  gather(key='loc', value='loc.comment', Location.1..Comment.on.location:Location.7..Comment.on.location, na.rm=FALSE)
landmatrix.loc.comment$loc <- gsub('..Comment.on.location', '', landmatrix.loc.comment$loc)


landmatrix.loc <- list(landmatrix.loc.accuracy, landmatrix.loc.loc, landmatrix.loc.lat, landmatrix.loc.lon ) %>%
  Reduce(function(dtf1,dtf2) cbind(dtf1,dtf2), .)

landmatrix.loc <- list(landmatrix.loc.accuracy, landmatrix.loc.loc, landmatrix.loc.lat, landmatrix.loc.lon, landmatrix.loc.country, landmatrix.loc.comment ) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2), .)

landmatrix.loc %>% dplyr::filter(is.na(.$loc.country) == TRUE) #this entry has weird coords that fall in Himalayas, remove

landmatrix.loc <- landmatrix.loc %>% dplyr::filter(is.na(.$loc.country) == FALSE) #this entry has weird coords that fall in Himalayas, remove

landmatrix.loc <- landmatrix.loc %>% mutate(Deal.ID.Loc = paste(Deal.ID, loc, sep='_') )
landmatrix.loc$Deal.ID.Loc <- gsub('Location.', '', landmatrix.loc$Deal.ID.Loc)


write.csv(landmatrix.loc, 'data/LandMatrix_NR_20190409.csv', row.names=FALSE)


# Load processed csv of concessions ####
conc_sf <- read.csv('data/LandMatrix_NR_20190409.csv')
conc_sf <- dplyr::rename(conc_sf, y=loc.lat, x=loc.lon)
conc_sf <- st_as_sf(conc_sf, coords= c('x', 'y'), crs= 4326)
plot(st_geometry(conc_sf))

table(conc_sf$Current.negotiation.status) #349 concluded, 9 under negotiation
table(conc_sf$Current.implementation.status) #203 in operation. 84 project not started. 91 none or -----

# Create new column called 'status' to combine the implementation and negotiation status
conc_sf$status <- NA
conc_sf[grepl('In operation', conc_sf$Current.implementation.status), ]$status  <- 'Operating'
conc_sf[grepl('Startup', conc_sf$Current.implementation.status), ]$status  <- 'Startup'

conc_sf[grepl('Intended', conc_sf$Current.negotiation.status) & is.na(conc_sf$status)==TRUE & conc_sf$Current.implementation.status !='Project abandoned', ]$status <- 'Intended'

table(conc_sf$Current.negotiation.status)

# Filter out the failed contracts (but include failed contracts that are in Startup Phase - coz the land has been cleared)
check <- conc_sf[grepl('Failed', conc_sf$Current.negotiation.status), ]
conc_sf <- conc_sf %>% dplyr::filter(!(grepl('Failed', .$Current.negotiation.status) & is.na(.$status)==TRUE))

# Filter out the 3 'None' & status NA (lacking in info)
check <- conc_sf %>% dplyr::filter(Current.negotiation.status=='None')
conc_sf <- conc_sf %>% dplyr::filter(!(Current.negotiation.status=='None' & is.na(.$status)==TRUE))

st_crs(conc_sf)

st_write(conc_sf, dsn='data/conc_sf_landmatrix.gpkg', overwrite=TRUE)
