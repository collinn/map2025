User Guide

1. power_simulation.R
--------------------
This is the main simulation script.  
It is run by the shell script (see 3. power_script.sh) and takes an index number (1–10) to decide which simulation condition to run.  
The output is one `.rds` file containing 1000 simulation results per condition.

Details:
- Loads simulation settings based on the given index.
- Simulates fixation data using `createPlineData()` with different slope, AR(1), and error assumptions.
- Fits the data using `bfit()` from the `bdots` package.
- Runs three significance tests:
   - Homogeneous bootstrap
   - Heterogeneous bootstrap
   - Permutation test
- Repeats 1000 times.
- Saves the results as a `.rds` file.
- Logs progress to a text file in the folder `prog_txt/`.

2. over_time_power_plot.R
-------------------------
This file contains functions for analyzing the simulation results.

Functions:
- `timetiePower()`: Converts results to vectors and shows which time points were significant.
- `getDiffSlices()`: Generates basic R plots presenting simulation results.
- `getDiffSlicesgg()`: Generates `ggplot2` plots showing power over time, including helpful reference lines for expected timing.

3. power_script.sh
-----------------
This shell script runs the simulation by calling `power_simulation.R` with different indices.

4. calc_power.R (optional)
--------------------------
This script reads simulation result files (`.rds`), calculates summary statistics for power analysis, and produces formatted tables showing:
- Type I error
- Type II error
- Power for different testing methods

5. rds_files/
--------------
This folder stores output files from simulation runs.  
Each file is saved in R’s binary `.rds` format and contains results from multiple simulation iterations for a given simulation setting.
