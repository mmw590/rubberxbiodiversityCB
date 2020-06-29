######## 06-scenarios_hpc_scen3b_vulnA.R ############
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
source('code/13.0-unrestricted_scenarios_fxn.R')


# Scenario 3b: Compromise (Compromising production prioritise biodiversity), averaged suit, vuln (vulnA), acc and carbon (optim4). ####
scen_in <- suit_vuln_vals3[ , .(suit, forest, comprom.vulnA.2, optim4.vulnA)]
scen_in <- data.matrix(scen_in) #2.9mb as matrix

rm(list=setdiff(ls(), c("scen_results_nvar2", "scen_in", "scen_spp", "spp_ranges", "res_0", "nrep", "ncores")))
gc()

#### McLapply method ####
scen_suit_rep <- mclapply(1:nrep, function(i, ...) {scen_results_nvar2(vals_mod=scen_in, vals_spp=scen_spp, spp_ranges=spp_ranges)} , mc.cores=ncores, mc.set.seed=TRUE)
#mc.set.seed if set to TRUE then each parallel process first sets its seed to something different from other processes
#getOption("mc.cores")

rm(scen_in)


reslist <- lapply(scen_suit_rep, "[[", 1) #if used lapply to do the replicates
scen_df <- Reduce("+", reslist) / length(reslist)

fwrite(scen_df, 'output/scen_df_vulnA_3b.csv')

spprangeloss_df <- data.frame(#spp=names(foo_spp), 
  #iucn_status=foo_iucn_status, 
  ori_range=spp_ranges, 
  range_remaining_low=rowMeans(sapply(scen_suit_rep, "[[", 2), na.rm=TRUE),
  range_remaining_high=rowMeans(sapply(scen_suit_rep, "[[", 3), na.rm=TRUE),
  range_remaining_low_EWT=rowMeans(sapply(scen_suit_rep, "[[", 4), na.rm=TRUE),
  range_remaining_high_EWT=rowMeans(sapply(scen_suit_rep, "[[", 5), na.rm=TRUE))

fwrite(spprangeloss_df, 'output/spprangelosstbl_vulnA_scen3b.csv')

