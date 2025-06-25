#!/bin/bash

#SBATCH --job-name=simulationTest
#SBATCH --output=simulationTest_%a.out
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=500M
#SBATCH --array=1-10:2 -N1

R CMD BATCH "--args $SLURM_ARRAY_TASK_ID" fwer_sim.R
