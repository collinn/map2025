#!/bin/bash
#SBATCH --job-name=powerSim
#SBATCH --array=1-3
#SBATCH --output=log_%A_%a.out
#SBATCH --error=log_%A_%a.err
#SBATCH --time=01:00:00
#SBATCH --mem=4G
#SBATCH --cpus-per-task=2

# Load R or set R library path
# module load R         # <- only if your cluster uses modules
export R_LIBS_USER=/data/home/yantiffa/R/x86_64-pc-linux-gnu-library/4.3

# Run the script with current SLURM task ID
Rscript ~/bdots_project/map2025/old_scripts/new_power/power_simulation.R $SLURM_ARRAY_TASK_ID
