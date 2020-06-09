#!/bin/bash
#$ -cwd                                 # Run job from current directory

# Request 4 cores from ShARC's scheduler (max 16 cores on a public node), smp is all cores on same node
#$ -pe smp 4

#Requests XGig real (rmem) memory per task, Maximum allowed is 256G
#$ -l rmem=36G

# Requests X hours (the maximum) per task
#$ -l h_rt=2:30:00 

#Send notifications to this email:
#$ -M youremail@email.com

#Send notifications for 'b' begin, 'e' end , 'a' abort
#$ -m bea

#Set the name of the task
#$ -N scenarios

# Tell programs that use the OpenMP library to use 4 cores (1 thread per core). I'm not sure if you need this for R but no harm putting it in?
export OMP_NUM_THREADS=4


#Load R
module load apps/R/3.5.1/gcc-4.8.5     # Recommended to load a specific version of R


#Run parallelised R script - 1st directory points to the R script, second directory is the log file to be created for each task
R CMD BATCH  --no-save --no-restore 12.1-country_scenarios_hpc_scen1.R logs/scenarios.$JOB_ID
