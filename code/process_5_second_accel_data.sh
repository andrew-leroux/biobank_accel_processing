#!/bin/sh
#$ -cwd
#$ -t 1-200
#$ -R y
#$ -l mem_free=5G,h_vmem=5G
module load conda_R
Rscript create_1min_accel_indvidial_cluster.R $SGE_TASK_ID
