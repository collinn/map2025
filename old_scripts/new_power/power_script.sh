#!/bin/bash
#SBATCH --job-name=powerSim
#SBATCH --array=1-10
#SBATCH --output=log_%A_%a.out
#SBATCH --error=log_%A_%a.err
#SBATCH --time=01:00:00
#SBATCH --mem=4G
#SBATCH --cpus-per-task=2

# Load R or set R library path
# module load R         # <- only if your cluster uses modules
# export R_LIBS_USER=/data/home/yantiffa/R/x86_64-pc-linux-gnu-library/4.3
# delete later ^

# Run the script with current SLURM task ID
R CMD BATCH "--args $SLURM_ARRAY_TASK_ID" power_simulation.R
