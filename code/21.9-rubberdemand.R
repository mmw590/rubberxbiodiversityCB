######## 21.9-rubberdemand.R #######
library(dplyr)
library(ggplot2)
library(cowplot)

#### FAO Rubber Harvested Area by Year ####
fao <- read.csv('data/FAOSTAT_yield_NR_World_2020-04-21.csv')
head(fao)

nr <- fao %>% dplyr::filter(Element == 'Area harvested') %>% dplyr::select(Area, Year, Value)
head(nr)
nr <- nr %>% dplyr::filter(Year > 1980)
nr <- droplevels(nr)
unique(nr$Area)
range(nr$Year)

countriesinasia <- c('Bangladesh', 'Brunei Darussalam', 'Cambodia', 'China', 'India', 'Indonesia', 'Malaysia', 'Myanmar', 'Philippines', 'Singapore', 'Sri Lanka', 'Thailand', 'Timor-Leste', 'Viet Nam', 'Papua New Guinea') # 'China, mainland',

countriesinafrica <- c('Cameroon', 'Central African Republic', 'Congo', "CÃ´te d'Ivoire", 'Democratic Republic of the Congo', 'Gabon', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Liberia', 'Nigeria')

countriesinSA <- c('Bolivia (Plurinational State of)', 'Brazil', 'Colombia', 'Costa Rica', 'Dominican Republic', 'Ecuador', 'Guatemala', 'Mexico', 'Peru')

#Regional categories: World, Africa, Middle Africa, Western Africa, Americas, Central  america, Caribbean, South America, Asia, Eastern Asia, Southern Asia, South-Eastern Asia, Oceania, Melanesia
#Income: 51] Least Developed Countries               Land Locked Developing Countries    Small Island Developing States          Low Income Food Deficit Countries       Net Food Importing Developing Countries

nr <- nr %>% mutate(Region=ifelse(Area %in% countriesinasia, 'Asia/Oceania',
                                  ifelse(Area %in% countriesinafrica, 'Africa',
                                         ifelse(Area %in% countriesinSA, 'S. America', 'NA') ) ) ) 

nr_reg <- nr %>% filter(Region!="NA") %>% group_by(Region, Year) %>% summarize(rubberArea_milha = sum(Value, na.rm=TRUE)/1000000)
# nr_reg_All <- nr_reg  %>% group_by(Year) %>% summarize(rubberArea_milha = sum(rubberArea_milha)) %>% mutate(Region = "4-All")
# 
# nr_reg_check <- nr %>% filter(Region=="4-All") %>% group_by(Region, Year) %>% summarize(rubberArea_milha = sum(Value, na.rm=TRUE)/1000000)


nr_reg$Region <- factor(nr_reg$Region, levels = c("S. America", "Africa", "Asia/Oceania", "All"))

check <- nr_reg %>% dplyr::filter(Year==2016) %>% dplyr::select(rubberArea_milha)
check$rubberArea_milha/sum(check$rubberArea_milha)
#in 2016, Asia/Oceania had 89.6% of all rubber harv area, africa 7.9% and S. America 2%

check <- nr_reg %>% dplyr::filter(Year==2018) %>% dplyr::select(rubberArea_milha)
check$rubberArea_milha/sum(check$rubberArea_milha)
# Trends pretty much the same in 2018.



#### + Projected increase in rubber area to meet demand ####

# Total rubber area in specific years
rubberArea_milha_2010 = ungroup(nr_reg) %>% filter(Year==2010) %>% dplyr::select(rubberArea_milha) %>% sum()
rubberArea_milha_2016 = ungroup(nr_reg) %>% filter(Year==2016) %>% dplyr::select(rubberArea_milha) %>% sum()
rubberArea_milha_2017 = ungroup(nr_reg) %>% filter(Year==2017) %>% dplyr::select(rubberArea_milha) %>% sum()
rubberArea_milha_2018 = ungroup(nr_reg) %>% filter(Year==2018) %>% dplyr::select(rubberArea_milha) %>% sum()
rubberArea_milha_1981 = ungroup(nr_reg) %>% filter(Year==1981) %>% dplyr::select(rubberArea_milha) %>% sum() 

# Actual expansion from 2010 to 2018
rubberArea_milha_2018-rubberArea_milha_2010 # 2.336213 mil ha

#### Figure showing projections overlayed on FAO rubber area harvested ####
theme_set(theme_cowplot(font_size=14))

# Color blind friendly palette
cbPalette <- c("#999999", "#D55E00",  "#56B4E9", "#000000") #gray, red, blue, black
# The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #i,3,7 - gray, blue, red

rub_harvarea <- ggplot(data=nr_reg, aes(y=rubberArea_milha, x=Year, fill=Region)) +
  geom_area() +
  scale_fill_manual(limits=c("S. America", "Africa", "Asia/Oceania", "All"), values=cbPalette) +
  ylab('Rubber area harvested \n(mil ha)')

#there was a slight dip in 2017 but slight increase in 2018


#### + Converting rubber demand to land demand ####
# using IRSG estimates + conversion factors from Warren-Thomas et al. 2015

# The demand for 2018 used by Warren-Thomas et al. 2015 (EWT) is pretty close to IRSG's more recent estimate
# EWT uses Demand: 13,800,000 t yr-1 by 2018, IRSG 13.68
# baseline 2010 - 10.7 mil tonnes; 9.464277 Mha (harv area)

# The demand for 2024 used by EWT is much higher than IRSG's more recent estimate
# EWT uses Demand: 18.05 mil tonnes yr-1 by 2024, IRSG 15.88

# IRSG pred demand for 2027: 16.79 (range 16.65-16.76)
# IRSG reported demand for 2017: 13.22 mil tonnes
16.79-13.22 # pred increase in demand of 3.57 t from 2017-2027

# Predicted increase in rubber area to meet 2027 demand using IRSG demand (2017 baseline demand)
# Using simple conversion factors from EWT = Minimum and maximum yields of current plantations on mainland Southeast Asia, based on adjusted tapped area (Table S3 in EWT2015), 0.915 t ha-1 yr-1 and 1.452 t ha-1 yr-1
irsg2027_high <- (16.79-13.22)/0.915
irsg2027_low <- (16.79-13.22)/1.452

irsg2024_high <- (15.88-13.22)/0.915
irsg2024_low <- (15.88-13.22)/1.452



# Total rubber area projected in 2027
irsg_proj_low <- data.frame(Region='All', Year=c(2017, 2024, 2027), rubberArea_milha=c(rubberArea_milha_2017, rubberArea_milha_2017+irsg2024_low, rubberArea_milha_2017+irsg2027_low)) #13.29 mil ha in 2024, 10.36 in 2018
irsg_proj_high <- data.frame(Region='All', Year=c(2017, 2024, 2027), rubberArea_milha=c(rubberArea_milha_2017, rubberArea_milha_2017+irsg2024_high, rubberArea_milha_2017+irsg2027_high)) #18.33 mil ha in 2024, 13.68 in 2018

irsg_proj_high$Region <- factor(irsg_proj_high$Region, levels = c("S. America", "Africa", "Asia/Oceania", "All"))
irsg_proj_low$Region <- factor(irsg_proj_low$Region, levels = c("S. America", "Africa", "Asia/Oceania", "All"))

irsg_proj  <- rbind(irsg_proj_low, irsg_proj_high)

theme_set(theme_cowplot(font_size=14))

rub_harvarea <- ggplot(data=nr_reg, aes(y=rubberArea_milha, x=Year, fill=Region)) +
  geom_area() +
  scale_fill_manual(limits=c("S. America", "Africa", "Asia/Oceania", "All"), values=cbPalette) +
  ylab('Rubber area harvested \n(mil ha)')

rub_harvarea_proj <- rub_harvarea +
  geom_point(data=irsg_proj, size=3, shape=20) +
  geom_line(data=irsg_proj_low, lty = 'dotted', lwd=1) +
  geom_line(data=irsg_proj_high, lty='dotted', lwd=1) +
  geom_vline(xintercept=c(2017,2027), lty='dotted', lwd=1) +
  theme(legend.position=c(0, 0.85), legend.title=element_blank())

ggsave('output/results/rubber_harvarea_1981-2027_IRSGproj.png', rub_harvarea_proj, width=18, height = 12.2, units='cm')



#### Warren-Thomas 2015 Projections ####
# numbers from Table S8
# range of projected increase in rubber area from 2010-2024: 3.822345-8.864975 mil ha
# based on Demand: 18,050,000 t yr-1 by 2024 

(18.05-13.22)/0.915
(18.05-13.22)/1.452

# Actual expansion from 2010 to 2018
rubberArea_milha_2018-rubberArea_milha_2010 # 2.336213 mil ha
(0.895347+4.220167)/2 #the midpoint btwn EWT2015's highest and lowest projection for expansion by 2018 is 2.56, pretty close to actual . 

# Additional area needed from 2017 to 2024, adjusted for intensification of existing rubber in Malaysia/Indonesia and displacement by oil palm 
ewt2024_lowest <- rubberArea_milha_2010+3.822345-rubberArea_milha_2017 #1.66-6.70 mil ha
ewt2024_highest <- rubberArea_milha_2010+8.864975-rubberArea_milha_2017 #

# if using simple conversion factors from EWT = Minimum and maximum yields of current plantations on mainland Southeast Asia, based on adjusted tapped area (Table S3 in EWT2015), 0.915 t ha-1 yr-1 and 1.452 t ha-1 yr-1
ewt2024_low <- (18.05-13.22)/1.452
ewt2024_high <- (18.05-13.22)/0.915


# Total rubber area projected in 2024
nr_proj_low <- data.frame(Region='All', Year=c(2010, 2024), rubberArea_milha=c(rubberArea_milha_2010, rubberArea_milha_2010+3.822345)) #13.29 mil ha in 2024
nr_proj_high <- data.frame(Region='All', Year=c(2010, 2024), rubberArea_milha=c(rubberArea_milha_2010, rubberArea_milha_2010+8.864975)) #18.33 mil ha in 2024

nr_proj_low <- data.frame(Region='All', Year=c(2010, 2018, 2024), rubberArea_milha=c(rubberArea_milha_2010, rubberArea_milha_2010+0.895347, rubberArea_milha_2010+3.822345)) #13.29 mil ha in 2024, 10.36 in 2018
nr_proj_high <- data.frame(Region='All', Year=c(2010, 2018, 2024), rubberArea_milha=c(rubberArea_milha_2010, rubberArea_milha_2010+4.220167, rubberArea_milha_2010+8.864975)) #18.33 mil ha in 2024, 13.68 in 2018

nr_proj_high$Region <- factor(nr_proj_high$Region, levels = c("S. America", "Africa", "Asia/Oceania", "All"))
nr_proj_low$Region <- factor(nr_proj_low$Region, levels = c("S. America", "Africa", "Asia/Oceania", "All"))

nr_proj  <- rbind(nr_proj_low, nr_proj_high)

# + Validation figure for EWT 2015 Projections ####
rub_harvarea <- ggplot(data=nr_reg, aes(y=rubberArea_milha, x=Year, fill=Region)) +
  geom_area() +
  scale_fill_manual(limits=c("S. America", "Africa", "Asia/Oceania", "All"), values=cbPalette) +
  ylab('Rubber area harvested \n(mil ha)')

rub_harvarea_proj <- rub_harvarea +
  geom_point(data=nr_proj, size=3, show.guide=FALSE) +
  geom_line(data=nr_proj_low, lty = 'dashed', lwd=1.2, show.guide=FALSE) +
  geom_line(data=nr_proj_high, lty='dashed', lwd=1.2, show.guide=FALSE) +
  theme(legend.position=c(0, 0.85), legend.title=element_blank())

#ggsave('output/figures/rubber_harvarea_1981-2016_iccb.png', rub_harvarea_proj, width=18, height = 12.2, units='cm') #a version was used in a conference presentation
ggsave('output/results/rubber_harvarea_1981-2018-2024_EWTproj.png', rub_harvarea_proj, width=18, height = 12.2, units='cm')


# + Figure with both EWT and IRSG predictions ####
# remove 2010 baseline and 2018 points from EWT
# remove 2024 points from IRSG
nr_proj2 <- nr_proj %>% filter(Year!=2018)
irsg_proj2 <- irsg_proj %>% filter(Year!=2018)
nr_reg2 <- nr_reg %>% filter(Year>=1990)

rub_harvarea <- ggplot(data=nr_reg2, aes(y=rubberArea_milha, x=Year, fill=Region)) +
  geom_area() +
  scale_fill_manual(limits=c("S. America", "Africa", "Asia/Oceania", "All"), values=cbPalette) +
  ylab('Rubber area harvested \n(mil ha)')

rub_harvarea_proj <- rub_harvarea +
  geom_point(data=nr_proj, size=3, color="grey20", shape=4) +
  geom_point(data=irsg_proj, size=3, shape=20) +
  geom_line(data=nr_proj_low, lty = 'dashed', lwd=1, colour="grey40") +
  geom_line(data=nr_proj_high, lty='dashed', lwd=1, colour="grey40") +
  geom_line(data=irsg_proj_low, lty = 'dotted', lwd=1) +
  geom_line(data=irsg_proj_high, lty='dotted', lwd=1) +
  geom_vline(xintercept=c(2024), lty='dashed', lwd=1, colour="grey40") +
  geom_vline(xintercept=c(2027), lty='dotted', lwd=1) +
  #geom_vline(xintercept=c(2010,2024), lty='dashed', lwd=1, colour="grey40") +
  #geom_vline(xintercept=c(2017,2027), lty='dotted', lwd=1) +
  theme(legend.position=c(0, 0.85), legend.title=element_blank())

ggsave('output/results/rubber_harvarea_1981-2024-2027_EWT_IRSGproj.png', rub_harvarea_proj, width=18, height = 12.2, units='cm') 
ggsave('output/results/rubber_harvarea_1990-2024-2027_EWT_IRSGproj.png', rub_harvarea_proj, width=18, height = 12.2, units='cm') 




###### Extras ####
# + Countries with highest rate of expansion FAO ####
fao <- read.csv('data/FAOSTAT_yield_NR_World_2020-04-21.csv')
head(fao)

nr <- fao %>% dplyr::filter(Element == 'Area harvested') %>% dplyr::select(Area, Year, Value)
range(nr$Year)
nr1985 <- nr %>% filter(Year==1985) %>% dplyr::select(country = Area, area1985 = Value)
nr2015 <- nr %>% filter(Year==2015) %>% dplyr::select(country = Area, area2015 = Value)
nr.rate <- left_join(nr1985, nr2015) %>% group_by(country) %>% summarize(exp.rate = (area2015 - area1985)/30, exp.area = (area2015-area1985)/1000000) %>% arrange(exp.area, exp.rate) #Rate of increase in 30 years 1985-2015
#Bangladesh, Dominican Republic, Gabon, Guinea don't have data for 1985-1990
unique(nr$Area)
View(nr.rate)

nrOther <- nr %>% filter(Area %in% c('Bangladesh', 'Dominican Republic', 'Gabon', 'Guinea'), is.na(nr$Value)==FALSE)
nrOther %>% group_by(Area) %>% summarize(firstYear = min(Year))

nr1990o <- nrOther %>% filter(Year==1990) %>% dplyr::select(country = Area, area1990 = Value)
nr2015o <- nrOther %>% filter(Year==2015) %>% dplyr::select(country = Area, area2015 = Value)
nr.rateOther <- left_join(nr1990o, nr2015o) %>% group_by(country) %>% summarize(exp.rate = (area2015 - area1990)/25) %>% arrange(exp.rate)

nr.rate %>% filter(exp.area > 0.30) %>% summarize(sum.area=sum(exp.area)) 


#### + Countries with highest production FAO ####
fao <- read.csv('data/FAOSTAT_yield_NR_World_2020-04-21.csv')
levels(fao$Element)
levels(fao$Area) #Lao is not on here!

prod <- fao %>% dplyr::filter(Element == 'Production') %>% dplyr::select(Area, Year, Value, Unit) #unit = tonnes
prod <- prod %>% dplyr::filter(Year %in% 2010:2018) #production is not too far from demand, NR demand in 2017 is 10.7mil tonnes, in 2017 is 13.22tonnes. NR supply in 2017 is 13.54 tonnes.
head(prod)

filter(prod, Area == "World")

unique(prod$Area)

# 11 major rubber countries in IRSG Report
countriesinasia <- c('Bangladesh', 'Brunei Darussalam', 'Cambodia', 'China', 'India', 'Indonesia', 'Malaysia', 'Myanmar', 'Philippines', 'Singapore', 'Sri Lanka', 'Thailand', 'Timor-Leste', 'Viet Nam', 'Papua New Guinea') 

countries11 <- c( 'Cambodia', 'China', 'India', 'Indonesia', 'Malaysia', 'Myanmar',  'Philippines', 'Sri Lanka', 'Thailand', 'Viet Nam')

info11 <- fao %>% filter(Area %in% countries11, Year == 2018) %>% group_by(Element, Unit) %>% summarize(sumProduction=sum(Value/1000000)) #12.83731 mil tonnes

infoworld <- fao %>% filter(Area=="World", Year == 2018) %>% group_by(Element, Unit) %>% summarize(sum(Value/1000000)) #14.33482

info11/infoworld*100 #90% of world production & 89% of harv area is by these 10(11?) countries in 2018. 


# Countries outside 11 country
info_other <- prod %>% filter(Year == 2018) %>% group_by(Area) %>% summarize(sumProduction=sum(Value/1000000))  %>% mutate(pctProduction = round(sumProduction/14.334823*100, 1)) %>% arrange(desc(pctProduction))

write.csv(info_other, "output/FAO_NRproduction2018_bycountry.csv", row.names=FALSE)




