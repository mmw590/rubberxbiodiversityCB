#### rubberxbiodiversityCB 
#### 11-scenarios_prep_country.R ####

library(tidyverse)
library(data.table)

# Create output folders ####

if (!dir.exists(file.path('output/', 'country_scenario'))) {
  (dir.create(file.path('output/', 'country_scenario'))) }


# make suit_vuln_vals3 dataset for 11 countries 
suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv')
suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]


suit_vuln_11 <- suit_vuln_vals3 %>% filter(country %in% c("Thailand", "Indonesia", "Malaysia", "India", "Vietnam", "China", "Cambodia", "Myanmar", "Laos", "Sri Lanka", "Philippines")) #70811

suit_vuln_11$country <- ifelse(suit_vuln_11$country %in% c("Cambodia", "Myanmar", "Laos"), "CAMAL", suit_vuln_11$country)

suit_vuln_11 <- suit_vuln_11 %>% arrange(cell.id)

cell.id11 <- suit_vuln_11$cell.id


# Convert country names to codes 
suit_vuln_11$country11 <- as.integer(as.factor(suit_vuln_11$country)) #convert into number codes (so it can be converted into matrix to save memory)

# lookup table
country11codes <- suit_vuln_11 %>% distinct(country11, country) %>% arrange(country11)
country11codes$ncellstoconvert <- c(7, 3, 6, 9, 4,3,1,7,1 )
country11codes 
#fwrite(country11codes, "output/country11codes_ncellstoconvert.csv")




#### Checking for duplicates in Scen1-2 datasets ####
#  single criteria to be sorted by (for order of converting cells)

# data.table methods
suit_vuln_11 <- as.data.table(suit_vuln_11)
scen_in <- suit_vuln_11[ , .(cell.id, suit, forest, country11, avg.suit.acc, avg.vulnA.carb, avg.vulnT.carb)] #only extract needed columns for sorting
# scen_in <- dplyr::select(suit_vuln_11, cell.id, suit, forest, country11, avg.suit.acc, avg.vulnA.carb, avg.vulnT.carb, optim4.vulnA, optim4.vulnT)


list_scen_country <- split(scen_in, scen_in$country11)

colnames(scen_in)[5:7] #j=column#, j=5,6,7 for avg.suit.acc, avg.vulnA.carb, avg.vulnT.carb, "optim4.vulnA"   "optim4.vulnT"  

for (j in 5:7){
  for (i in 1:9) {   #i = countrycode
    scen_country <- list_scen_country[[i]] #select the country
    scen_country <- scen_country[order(scen_country[, ..j]), ] #sort dt by criteria 
    scen_country <- scen_country[1:10, ] #9 is the max number of cells to convert per country, so add 1 in case there's multiple options for the last cell
    
    if( sum(duplicated(scen_country[, ..j]))==0 ) { #is the number of duplicates = 0? 
      #print(paste("country", i, "has 0 duplicates in column", j, sep=" "))# TRUE - continue don't need to simulate 1000x
    }  else { print(paste("country", i, "has duplicates in column", j, "- go to next checking step", sep=" ")) }
  }
}

# country #2 is China, we only need to convert 3 cells in China
for (j in 5:7){
  i=2
  scen_country <- list_scen_country[[i]] #select the country
  scen_country <- scen_country[order(scen_country[, ..j]), ] #sort dt by criteria 
  scen_country <- scen_country[1:4, ] #9 is the max number of cells to convert per country, so add 1 in case there's multiple options for the last cell
  
  if( sum(duplicated(scen_country[, ..j]))==0 ) { #is the number of duplicates = 0? 
    print(paste("country", i, "has 0 duplicates in column", j, sep=" "))# TRUE - continue don't need to simulate 1000x
  }  else { print(paste("country", i, "has duplicates in column", j, "- go to next checking step", sep=" ")) }
}
# no duplicates, no need to do simulations




##### + Splitting data by country, sorting by criteria, then selecting only n cells to be converted acc to specified projections ####

list_scen_in_c11 <- list()
scen_in_blank <- data.frame(cell.id=integer(), suit=numeric(), forest=integer(), country11=integer(), avg.suit.acc=numeric(), avg.vulnA.carb=numeric(), avg.vulnT.carb=numeric())
scen_in_c11 <- scen_in_blank

for (j in 5:7){
  for (i in 1:9) {
    scen_country <- list_scen_country[[i]] #select the country
    scen_country <- scen_country[order(scen_country[, ..j]), ] #sort dt by criteria 
    
    if (i == 1 ) { scen_country <- scen_country[1:7, ] }
    if (i == 2 ) { scen_country <- scen_country[1:3, ] }
    if (i == 3 ) { scen_country <- scen_country[1:6, ] }
    if (i == 4 ) { scen_country <- scen_country[1:9, ] }
    if (i == 5 ) { scen_country <- scen_country[1:4, ] }
    if (i == 6 ) { scen_country <- scen_country[1:3, ] }
    if (i == 7 ) { scen_country <- scen_country[1:1, ] }
    if (i == 8 ) { scen_country <- scen_country[1:7, ] }
    if (i == 9 ) { scen_country <- scen_country[1:1, ] }
    
    scen_in_c11 <- rbind(scen_in_c11, scen_country)
  }
  
  # sort by criteria again, after combining all countries after cutting to n cells by country
  scen_in_c11 <- scen_in_c11[order(scen_in_c11[, ..j]), ]
  
  list_scen_in_c11[[j-4]] <- scen_in_c11 #save to list
  
  scen_in_c11 <- scen_in_blank #refresh to empty dt
  
}

names(list_scen_in_c11) <- c("scen_in_c11_1.csv", "scen_in_c11_vulnA_2.csv", "scen_in_c11_2.csv")

str(list_scen_in_c11)


#### Check for duplicates in scen 3 dataset (vulnT) ####
scen_in <- suit_vuln_11[ , .(cell.id, suit, forest, country11, comprom.vulnT.1, comprom.vulnT.2, comprom.vulnT.3, optim4.vulnT )] #only extract needed columns for sorting

list_scen_country2 <- split(scen_in, scen_in$country11)

colnames(scen_in)[5:8] #j=column#, j=5,6,7 for avg.suit.acc, avg.vulnA.carb, avg.vulnT.carb, "optim4.vulnA"   "optim4.vulnT"  

for (j in 5:7){
  for (i in 1:9) {   #i = countrycode
    scen_country <- list_scen_country2[[i]] #select the country
    scen_country$comprom <- scen_country[, ..j]
    scen_country <- scen_country[ order(comprom, optim4.vulnT)] #sort dt by comprom then optim 
    scen_country <- scen_country[1:10, ] #9 is the max number of cells to convert per country, so add 1 in case there's multiple options for the last cell

    if( sum(duplicated(scen_country[, c("comprom", "optim4.vulnT")] ) )==0 ) { #is the number of duplicates = 0? 
      #print(paste("country", i, "has 0 duplicates in column", j, sep=" "))# TRUE - continue don't need to simulate 1000x
    }  else { print(paste("country", i, "has duplicates in column", j, "- go to next checking step", sep=" ")) }
  }
}

# no duplicates!

##### + Splitting data by country, sorting by 2 criteria, then selecting only n cells to be converted acc to specified projections ####

names(list_scen_in_c11)

scen_in_blank <- data.frame(cell.id=integer(), suit=numeric(), forest=integer(), country11=integer(), comprom.vulnT.1 =integer(), comprom.vulnT.2=integer(), comprom.vulnT.3=integer(), optim4.vulnT=numeric(), comprom=integer())
scen_in_c11 <- scen_in_blank

for (j in 5:7){
  for (i in 1:9) {
    scen_country <- list_scen_country2[[i]] #select the country
    scen_country$comprom <- scen_country[, ..j]
    scen_country <- scen_country[order(comprom, optim4.vulnT), ] #sort dt by criteria 
    
    if (i == 1 ) { scen_country <- scen_country[1:7, ] }
    if (i == 2 ) { scen_country <- scen_country[1:3, ] }
    if (i == 3 ) { scen_country <- scen_country[1:6, ] }
    if (i == 4 ) { scen_country <- scen_country[1:9, ] }
    if (i == 5 ) { scen_country <- scen_country[1:4, ] }
    if (i == 6 ) { scen_country <- scen_country[1:3, ] }
    if (i == 7 ) { scen_country <- scen_country[1:1, ] }
    if (i == 8 ) { scen_country <- scen_country[1:7, ] }
    if (i == 9 ) { scen_country <- scen_country[1:1, ] }
    
    scen_in_c11 <- rbind(scen_in_c11, scen_country)
  }
  
  # sort by criteria again, after combining all countries after cutting to n cells by country
  scen_in_c11 <- scen_in_c11[order(comprom, optim4.vulnT), ]
  
  list_scen_in_c11[[j-1]] <- scen_in_c11 #save to list
  
  scen_in_c11 <- scen_in_blank #refresh to empty dt
  
}

names(list_scen_in_c11)[4:6] <- c("scen_in_c11_3a.csv", "scen_in_c11_3b.csv", "scen_in_c11_3c.csv")




#### Check for duplicates in scen 3 dataset (vulnA) ####
scen_in <- suit_vuln_11[ , .(cell.id, suit, forest, country11, comprom.vulnA.1, comprom.vulnA.2, comprom.vulnA.3, optim4.vulnA )] #only extract needed columns for sorting

list_scen_country2 <- split(scen_in, scen_in$country11)

colnames(scen_in)[5:8] #j=column#, 

for (j in 5:7){
  for (i in 1:9) {   #i = countrycode
    scen_country <- list_scen_country2[[i]] #select the country
    scen_country$comprom <- scen_country[, ..j]
    scen_country <- scen_country[ order(comprom, optim4.vulnA)] #sort dt by comprom then optim 
    scen_country <- scen_country[1:10, ] #9 is the max number of cells to convert per country, so add 1 in case there's multiple options for the last cell
    
    if( sum(duplicated(scen_country[, c("comprom", "optim4.vulnA")] ) )==0 ) { #is the number of duplicates = 0? 
      #print(paste("country", i, "has 0 duplicates in column", j, sep=" "))# TRUE - continue don't need to simulate 1000x
    }  else { print(paste("country", i, "has duplicates in column", j, "- go to next checking step", sep=" ")) }
  }
}

# no duplicates!


##### + Splitting data by country, sorting by 2 criteria, then selecting only n cells to be converted acc to specified projections ####

names(list_scen_in_c11)

scen_in_blank <- data.frame(cell.id=integer(), suit=numeric(), forest=integer(), country11=integer(), comprom.vulnA.1 =integer(), comprom.vulnA.2=integer(), comprom.vulnA.3=integer(), optim4.vulnA=numeric(), comprom=integer())
scen_in_c11 <- scen_in_blank

for (j in 5:7){
  for (i in 1:9) {
    scen_country <- list_scen_country2[[i]] #select the country
    scen_country$comprom <- scen_country[, ..j]
    scen_country <- scen_country[order(comprom, optim4.vulnA), ] #sort dt by criteria 
    
    if (i == 1 ) { scen_country <- scen_country[1:7, ] }
    if (i == 2 ) { scen_country <- scen_country[1:3, ] }
    if (i == 3 ) { scen_country <- scen_country[1:6, ] }
    if (i == 4 ) { scen_country <- scen_country[1:9, ] }
    if (i == 5 ) { scen_country <- scen_country[1:4, ] }
    if (i == 6 ) { scen_country <- scen_country[1:3, ] }
    if (i == 7 ) { scen_country <- scen_country[1:1, ] }
    if (i == 8 ) { scen_country <- scen_country[1:7, ] }
    if (i == 9 ) { scen_country <- scen_country[1:1, ] }
    
    scen_in_c11 <- rbind(scen_in_c11, scen_country)
  }
  
  # sort by criteria again, after combining all countries after cutting to n cells by country
  scen_in_c11 <- scen_in_c11[order(comprom, optim4.vulnA), ]
  
  list_scen_in_c11[[j+2]] <- scen_in_c11 #save to list
  
  scen_in_c11 <- scen_in_blank #refresh to empty dt
  
}

names(list_scen_in_c11)[7:9] <- c("scen_in_c11_vulnA_3a.csv", "scen_in_c11_vulnA_3b.csv", "scen_in_c11_vulnA_3c.csv")

names(list_scen_in_c11)


rm(country11codes, list_scen_country, scen_country, scen_in, scen_in_blank, scen_in_c11)
gc()



# Check for duplicates within the 41 cells after splitting And combining countries ####
for (j in 1:3){
  scen_in <-  list_scen_in_c11[[j]] 
  k = j+4
  
  if( sum(duplicated(scen_in[, ..k]))==0 ) { #is the number of duplicates = 0? 
    #print(paste("0 duplicates in criteria", j, sep=" "))# TRUE - continue don't need to simulate 1000x
  }  else { print(paste("duplicates in criteria", j, "- go to next checking step", sep=" ")) }
}


for (j in 4:9){
  scen_in <-  list_scen_in_c11[[j]] 
  
  if( sum(duplicated( scen_in[, 8:9] ) )==0 ) { #is the number of duplicates = 0? 
    #print(paste("0 duplicates in criteria", j, sep=" "))# TRUE - continue don't need to simulate 1000x
  }  else { print(paste("duplicates in criteria", j, "- go to next checking step", sep=" ")) }
}


# No duplicates!


str(list_scen_in_c11)


# + save dfs in list to csv ...  ####
mapply(function(dfslistname, dfslist) 
  write.csv(dfslist, file = paste0("output/country_scenario/", dfslistname), row.names = FALSE), 
  names(list_scen_in_c11), list_scen_in_c11)




######### Scenario simulations (combine 11 countries + not 11 countries) #####
rm(list=ls())
gc()

# source('code/21.8-country_scenarios_fxn.R')


##### 21.8-country_scenarios_fxn.R ####
# Written for use in HPC, adapt as required for your HPC system

## Description of variables:
# vals_mod is a data.matrix/table/frame with 3 columns: suit, forest, and the criteria variable scenario is based on. each row represents a cell in the grid.
# vals_spp is a data.matrix/table/frame with as many columns as there are species, and each row indicates presence/absence of that species in the cell
# spp_ranges is a vector of  the original range of all spp (colsums(vals_spp), or sums of 1s)
# ranges_ = range remaining after each (forested) cell is converted
# nrows is number of cells to convert. I set the default to 700 cells = 7 Mha
# res is lost area (no. of cells converted), sum of forested range lost (1cell=100km2=10,000ha=0.01Mha), sum of % range loss, no. of spp lost any amount of range, no. of spp lost >=10% of range, average suitability of converted cells 


#### Function for 2 criteria to convert on
# inputs same as above but
# vals_mod is a data.matrix/table/frame with 4 columns: suit, forest, and the two criteria variables the scenario is based on. each row represents a cell in the grid.
# In Country Simulations, the first criteria is conv.ord, which signals that these cells are within threshold of restricted expansion in 11 countries OR outside 11 countries 
## In Country Simulations (forced expansion in 11 countries first), the first criteria is conv.ord, which is the order for the first 41 cells to be converted within the 11 countries . 

scen_results_nvar2 <- function(vals_mod, vals_spp, spp_ranges, nrows=700){
  vals_spp = cbind(vals_mod, sample.int(nrow(vals_mod)), vals_spp) #cbind scenario vals, random numbe column, spp distrb
  
  vals_spp = vals_spp[order(vals_spp[, 3], vals_spp[, 4],  vals_spp[, 5]), ] #sort by scenario values then random
  
  vals_mod = vals_spp[ , 1:2] #extract first column which is suitability, to calc avg suit in res. extract 2nd column which is forest
  
  vals_spp = vals_spp[ , 6:(ncol(vals_spp)) ] #select only columns of sppnames, rows are sorted by conversion order. 
  
  # Initialize objects for the loop
  ranges_ = spp_ranges #initial forested ranges_ before subtracting
  lost_area = 0
  res = res_0
  
  #for every cell converted (row i) in first 10K, find spp which occurs in that cell (non-NA), then minus 1 cell from that species' original range
  for (i in 1:nrows){
    
    spp_which_occur = which(is.na(vals_spp[i,])==FALSE & vals_spp[i,]>0 & vals_mod[i,2]==1) # & if spp occur in cell i and cell i is forested
    ranges_[spp_which_occur] = ranges_[spp_which_occur]-1 #remove 1 from the range of spp that occur in this cell(row)
    lost_area = lost_area + 1
    
    #record the info for every 10 cells
    if(i%%10 == 0){
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) )
    }
    
    #this code is for Expected range loss under the different scenarios for all African primate species, at different amounts of rubber expansion (EWT-2024, Industry-2027)
    
    if(i==245){
      spp_ranges_remaining_low = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==390){
      spp_ranges_remaining_high = ranges_
      # res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    if(i==166){
      spp_ranges_remaining_low_EWT = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==670){
      spp_ranges_remaining_high_EWT = ranges_
      #  res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    #rm(spp_which_occur)
    
  }
  output <- list(res, spp_ranges_remaining_low, spp_ranges_remaining_high, spp_ranges_remaining_low_EWT, spp_ranges_remaining_high_EWT)
  return(output) #output
}




#### Function for 3 criteria to convert on
# inputs same as above but
# vals_mod is a data.matrix/table/frame with 5 columns: suit, forest, and the three criteria variables the scenario is based on. each row represents a cell in the grid.
# In Country Simulations, the first criteria [,3] is conv.ord, which is the order for the first 41 cells to be converted within the 11 countries. 
# [,4] is comprom.vuln and [,5] is optim4 
# [,6] is a random number column (which allows for randomization within duplicate values)


scen_results_nvar3 <- function(vals_mod, vals_spp, spp_ranges, nrows=700){
  vals_spp = cbind(vals_mod, sample.int(nrow(vals_mod)), vals_spp) #cbind scenario vals, random numbe column, spp distrb
  
  vals_spp = vals_spp[order(vals_spp[, 3], vals_spp[, 4], vals_spp[, 5], vals_spp[, 6]), ] #sort by scenario values then random
  
  vals_mod = vals_spp[ , 1:2] #extract first column which is suitability, to calc avg suit in res. extract 2nd column which is forest
  
  vals_spp = vals_spp[ , 7:(ncol(vals_spp)) ] #select only columns of sppnames, rows are sorted by conversion order. 
  
  # Initialize objects for the loop
  ranges_ = spp_ranges #initial forested ranges_ before subtracting
  lost_area = 0
  res = res_0
  
  #for every cell converted (row i) in first 10K, find spp which occurs in that cell (non-NA), then minus 1 cell from that species' original range
  for (i in 1:nrows){
    
    spp_which_occur = which(is.na(vals_spp[i,])==FALSE & vals_spp[i,]>0 & vals_mod[i,2]==1) # & if spp occur in cell i and cell i is forested
    ranges_[spp_which_occur] = ranges_[spp_which_occur]-1 #remove 1 from the range of spp that occur in this cell(row)
    lost_area = lost_area + 1
    
    #record the info for every 10 cells
    if(i%%10 == 0){
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) )
    }
    
    #this code is for Expected range loss under the different scenarios for all forest dependent species, at different amounts of rubber expansion (EWT-2024, Industry-2027)
    
    if(i==245){
      spp_ranges_remaining_low = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==390){
      spp_ranges_remaining_high = ranges_
      # res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    if(i==166){
      spp_ranges_remaining_low_EWT = ranges_
      res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) 
    }
    
    if(i==670){
      spp_ranges_remaining_high_EWT = ranges_
      #  res=rbind(res, c(lost_area, sum(spp_ranges-ranges_), sum(1-(ranges_/spp_ranges)), sum(ranges_/spp_ranges < 1), sum(ranges_/spp_ranges < 0.9), sum(vals_mod[1:i])/i ) ) # Omit this line if demand is multiple of 10, res would already be recorded
    }
    
    #rm(spp_which_occur)
    
  }
  output <- list(res, spp_ranges_remaining_low, spp_ranges_remaining_high, spp_ranges_remaining_low_EWT, spp_ranges_remaining_high_EWT)
  return(output) #output
}



###### + This section is for loading libraries/data/parameters used in all scenarios simulations ##### 
#### + Load libraries ####
library(dplyr)
library(data.table)
library(parallel)
# # /home/bop17mw/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs
# update.packages(lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs")


#### + Set wd ####
#setwd('/data/bop17mw/rubberxbiodiversity/')
#setwd("C:/Users/bop17mw/Google Drive/1-Uni-of-Sheffield/1-PhD/Ch1-CultivationVsConservation/rubberxbiodiversity") #on PC



#### + Load data ####

# Spp matrix
#scen_spp <- fread('input/suit_vuln_scenario_combspp.csv') 
scen_spp <- fread('output/suit_vuln_scenario_combspp.csv') #on PC
scen_spp <- data.matrix(scen_spp) #6.1Gb if replace NAs with 0s/3Gb if not

# suit vuln vals
#suit_vuln_vals3 <- fread('input/suit_vuln_vals3_scenario.csv')
suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv') #on PC
suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]


# spp_ranges, res_0
spp_ranges = colSums(suit_vuln_vals3$forest>0 & scen_spp, na.rm=TRUE)
res_0 = matrix(c(0,0,0,0,0,0), nrow=1)

# extract subset of suit_vuln_vals3 for not 11 countries
`%notin%` <- Negate(`%in%`)

suit_vuln_not11 <- suit_vuln_vals3 %>% 
  filter(country %notin% c("Thailand", "Indonesia", "Malaysia", "India", "Vietnam", "China", "Cambodia", "Myanmar", "Laos", "Sri Lanka", "Philippines")) %>% 
  mutate(conv.ord = 1) %>%  #normal expansion is allowed in outside the ll countriess 
 # mutate(conv.ord = 42) %>% #if we force 0.41Mha conversion in 11 countries first
  as.data.table() #165306



# load list of sorted scen_in with 41 cells in 11 countries
#list_scen_in_c11 <- list.files("input/country_scenario/", "^scen_in_c11_", full.names = TRUE)
list_scen_in_c11 <- list.files("output/country_scenario/", "^scen_in_c11_", full.names = TRUE)
list_scen_in_c11 <- lapply(list_scen_in_c11, fread)
#print(list.files("input/country_scenario/", "^scen_in_c11_"))
print(list.files("output/country_scenario/", "^scen_in_c11_"))



####### Scenario 1 (example for 1 rep) #####

####### + Combine 11 countries + not 11 countries with conv.order to suit_vuln_vals3 ####
scen_in_c11 <- list_scen_in_c11[[1]]  ## pre-sorted by avg.suit.acc
#scen_in_c11$conv.ord <- 1:41  #if we force 0.41Mha conversion in 11 countries first, in pre-sorted order
scen_in_c11$conv.ord <- 1  #only these 41 cells are allowed to be converted (in any order along with cells outside 11 countries)

#combine 
scen_in <- rbind(scen_in_c11, suit_vuln_not11, fill=TRUE) %>% dplyr::select(cell.id, conv.ord)

scen_in <- left_join(suit_vuln_vals3, scen_in) %>% as.data.table() 

#table(scen_in$conv.ord)
#unique(scen_in$conv.ord) #NAs for the non-converted areas in 11 countries (not the 41 cells)


#### Extract needed columns for simulation 
scen_in <- scen_in[order(cell.id, x, y, region)] #same order as ori suit_vuln_vals3
scen_in <- scen_in[ , .(suit, forest, conv.ord, avg.suit.acc)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
#rm(list=setdiff(ls(), c("scen_results_nvar2", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
#gc()

system.time(scen_suit_rep <- scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges) ) #16s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/country_scenario/countrysim_scen_df1_test.csv') #country simulation results

spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/country_scenario/countrysim_spprangelosstbl_scen1_test.csv')


####### Scenario 2 (example for 1 rep) #####

####### + Combine 11 countries + not 11 countries with conv.order to suit_vuln_vals3 ####
scen_in_c11 <- list_scen_in_c11[[2]]  ## pre-sorted by avg.suit.acc
scen_in_c11$conv.ord <- 1  #new column for conversion order in 11 countries (first 41 cells/0.407Mha)
#scen_in_c11$conv.ord <- 1:41  #new column for conversion order in 11 countries (first 41 cells/0.407Mha)

#combine 
scen_in <- rbind(scen_in_c11, suit_vuln_not11, fill=TRUE) %>% dplyr::select(cell.id, conv.ord)

scen_in <- left_join(suit_vuln_vals3, scen_in) %>% as.data.table() 

#table(scen_in$conv.ord)
#unique(scen_in$conv.ord) #NAs for the non-converted areas in 11 countries (not the 41 cells)


#### Extract needed columns for simulation 
scen_in <- scen_in[order(cell.id, x, y, region)] #same order as ori suit_vuln_vals3
scen_in <- scen_in[ , .(suit, forest, conv.ord, avg.vulnT.carb)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
#rm(list=setdiff(ls(), c("scen_results_nvar2", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
#gc()

system.time(scen_suit_rep <- scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges) ) #11s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/country_scenario/countrysim_scen_df2_test.csv') #country simulation results

spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/country_scenario/countrysim_spprangelosstbl_scen2_test.csv')





##### Scenario 3a (example for sorting on 2 criteria; 1 rep) #######
print(list.files("output/country_scenario/", "^scen_in_c11_"))

####### + Combine 11 countries + not 11 countries with conv.order to suit_vuln_vals3 ####
scen_in_c11 <- list_scen_in_c11[[3]]  ## pre-sorted by comprom and optim
scen_in_c11$conv.ord <- 1
#scen_in_c11$conv.ord <- 1:41  #new column for conversion order in 11 countries (first 41 cells/0.407Mha)

#combine 
scen_in <- rbind(scen_in_c11, suit_vuln_not11, fill=TRUE) %>% dplyr::select(cell.id, conv.ord)

scen_in <- left_join(suit_vuln_vals3, scen_in) %>% as.data.table() 

#table(scen_in$conv.ord)
#unique(scen_in$conv.ord) #NAs for the non-converted areas in 11 countries (not the 41 cells)

#### Extract needed columns for simulation 
scen_in <- scen_in[order(cell.id, x, y, region)] #same order as ori suit_vuln_vals3
scen_in <- scen_in[ , .(suit, forest, conv.ord, comprom.vulnT.1, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
#rm(list=setdiff(ls(), c("scen_results_nvar3", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
#gc()

system.time(scen_suit_rep <- scen_results_nvar3(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)) #10s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/country_scenario/countrysim_scen_df3a_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/country_scenario/countrysim_spprangelosstbl_scen3a_test.csv')



##### Scenario 3b (example for sorting on 2 criteria; 1 rep) #######
print(list.files("output/country_scenario/", "^scen_in_c11_"))

####### + Combine 11 countries + not 11 countries with conv.order to suit_vuln_vals3 ####
scen_in_c11 <- list_scen_in_c11[[4]]  ## pre-sorted by comprom and optim
scen_in_c11$conv.ord <- 1
#scen_in_c11$conv.ord <- 1:41  #new column for conversion order in 11 countries (first 41 cells/0.407Mha)

#combine 
scen_in <- rbind(scen_in_c11, suit_vuln_not11, fill=TRUE) %>% dplyr::select(cell.id, conv.ord)

scen_in <- left_join(suit_vuln_vals3, scen_in) %>% as.data.table() 

#table(scen_in$conv.ord)
#unique(scen_in$conv.ord) #NAs for the non-converted areas in 11 countries (not the 41 cells)

#### Extract needed columns for simulation 
scen_in <- scen_in[order(cell.id, x, y, region)] #same order as ori suit_vuln_vals3
scen_in <- scen_in[ , .(suit, forest, conv.ord, comprom.vulnT.2, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
#rm(list=setdiff(ls(), c("scen_results_nvar3", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
#gc()

system.time(scen_suit_rep <- scen_results_nvar3(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)) #14s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/country_scenario/countrysim_scen_df3b_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/country_scenario/countrysim_spprangelosstbl_scen3b_test.csv')



##### Scenario 3c (example for sorting on 2 criteria; 1 rep) #######
print(list.files("output/country_scenario/", "^scen_in_c11_"))

####### + Combine 11 countries + not 11 countries with conv.order to suit_vuln_vals3 ####
scen_in_c11 <- list_scen_in_c11[[5]]  ## pre-sorted by comprom and optim
scen_in_c11$conv.ord <- 1
#scen_in_c11$conv.ord <- 1:41  #new column for conversion order in 11 countries (first 41 cells/0.407Mha)

#combine 
scen_in <- rbind(scen_in_c11, suit_vuln_not11, fill=TRUE) %>% dplyr::select(cell.id, conv.ord)

scen_in <- left_join(suit_vuln_vals3, scen_in) %>% as.data.table() 

#table(scen_in$conv.ord)
#unique(scen_in$conv.ord) #NAs for the non-converted areas in 11 countries (not the 41 cells)

#### Extract needed columns for simulation 
scen_in <- scen_in[order(cell.id, x, y, region)] #same order as ori suit_vuln_vals3
scen_in <- scen_in[ , .(suit, forest, conv.ord, comprom.vulnT.3, optim4.vulnT)]
scen_in <- data.matrix(scen_in) 


##### + Start conversion simulation #####
#rm(list=setdiff(ls(), c("scen_results_nvar3", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
#gc()

system.time(scen_suit_rep <- scen_results_nvar3(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)) #9s for single rep, 1.5mb output

scen_df <- scen_suit_rep[[1]]
scen_df
fwrite(scen_df, 'output/country_scenario/countrysim_scen_df3c_test.csv') #country simulation results


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=scen_suit_rep[[2]],
  range_remaining_high=scen_suit_rep[[3]],
  range_remaining_low_EWT=scen_suit_rep[[4]],
  range_remaining_high_EWT=scen_suit_rep[[5]])

head(spprangeloss_df)

fwrite(spprangeloss_df, 'output/country_scenario/countrysim_spprangelosstbl_scen3c_test.csv')




