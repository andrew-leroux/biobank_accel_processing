#!/bin/sh
#$ -cwd
#$ -R y
#$ -l mem_free=100G,h_vmem=100G
module load conda_R
Rscript ./code/create_scalar_accel_vars.R 
