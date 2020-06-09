#### rubberxbiodiversityCB 
#### 12.0-country_scenarios_fxn.R ####

# Written for use in HPC, adapt as required for your HPC system

## Required input data for this script were created in the previous script, 21.5-scenarios-prep.R:
# scen_spp <- fread('input/suit_vuln_scenario_combspp.csv') 
# suit_vuln_vals3 <- fread('input/suit_vuln_vals3_scenario.csv')



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
# In Country Simulations, the first criteria [,3] is conv.ord.41, which is the order for the first 41 cells to be converted within the 11 countries. 
# [,4] is comprom.vuln and [,5] is optim4 
# [,6] is a random number column (which allows for randomization within duplicate values)

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
# In Country Simulations, the first criteria [,3] is conv.ord.41, which is the order for the first 41 cells to be converted within the 11 countries. 
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





###### This section is for loading libraries/data/parameters used in all scenarios simulations ##### 
#### Load libraries ####
library(dplyr)
library(data.table)
library(parallel)
# # /home/bop17mw/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs
# update.packages(lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs")


#### Set wd ####
setwd('/data/bop17mw/rubberxbiodiversity/')
#setwd("C:/Users/bop17mw/Google Drive/1-Uni-of-Sheffield/1-PhD/Ch1-CultivationVsConservation/rubberxbiodiversity") #on PC



#### Load data ####

suit_vuln_vals3 <- fread('input/suit_vuln_vals3_scenario.csv')
#suit_vuln_vals3 <- fread('output/suit_vuln_vals3_scenario.csv')
suit_vuln_vals3 <- suit_vuln_vals3[order(cell.id, x, y, region)]

scen_spp <- fread('input/suit_vuln_scenario_combspp.csv') 
#scen_spp <- fread('output/suit_vuln_scenario_combspp.csv') 
scen_spp <- data.matrix(scen_spp)

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

rm(`%notin%`)


# load subset of sorted scen_in for 11 countries
list_scen_in_c11 <- list.files("input/country_scenario/", "^scen_in_c11_", full.names = TRUE)
#list_scen_in_c11 <- list.files("output/country_scenario/", "^scen_in_c11_", full.names = TRUE)
list_scen_in_c11 <- lapply(list_scen_in_c11, fread)
print(list.files("input/country_scenario/", "^scen_in_c11_"))
#print(list.files("output/country_scenario/", "^scen_in_c11_"))


#### Parameters for mclapply ####
# for final results, use 1000 reps and 4 cores
nrep = 1000 
ncores = 4

print(nrep); print(ncores)
