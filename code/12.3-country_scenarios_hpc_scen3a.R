######## 08-country_scenarios_hpc_scen3a.R ############
# script for running scenario simulations on HPC

# module load apps/R/3.5.1/gcc-4.8.5
# R
# cd rubberxbiodiversity
# qsub my_job.sh

# http://docs.hpc.shef.ac.uk/en/latest/sharc/software/apps/R.html

########################################################
#rm(list=ls())


#### Set wd ####
setwd('/data/bop17mw/rubberxbiodiversity/')
#setwd("C:/Users/bop17mw/Google Drive/1-Uni-of-Sheffield/1-PhD/Ch1-CultivationVsConservation/rubberxbiodiversity") #on PC


# Load function
source('21.8-country_scenarios_fxn.R')


# Scenario 3a ####
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
rm(list=setdiff(ls(), c("scen_results_nvar3", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
gc()


#### McLapply method ####
scen_suit_rep <- mclapply(1:nrep, function(i, ...) {scen_results_nvar3(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)} , mc.cores=ncores, mc.set.seed=TRUE)
#mc.set.seed if set to TRUE then each parallel process first sets its seed to something different from other processes
#getOption("mc.cores")

rm(scen_in_mat, scen_spp)

reslist <- lapply(scen_suit_rep, "[[", 1) 
scen_df <- Reduce("+", reslist) / length(reslist)

fwrite(scen_df, 'output/countrysim_scen_df3a.csv')


spprangeloss_df <- data.frame( 
  ori_range=spp_ranges, 
  range_remaining_low=rowMeans(sapply(scen_suit_rep, "[[", 2), na.rm=TRUE),
  range_remaining_high=rowMeans(sapply(scen_suit_rep, "[[", 3), na.rm=TRUE),
  range_remaining_low_EWT=rowMeans(sapply(scen_suit_rep, "[[", 4), na.rm=TRUE),
  range_remaining_high_EWT=rowMeans(sapply(scen_suit_rep, "[[", 5), na.rm=TRUE))

fwrite(spprangeloss_df, 'output/countrysim_spprangelosstbl_scen3a.csv')


