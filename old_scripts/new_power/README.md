<<<<<<< HEAD

=======
User Guide
>>>>>>> 83112a16bf0dd1360e6290aefa6e276143ea3bab
1. power_simulation.R
This is the main simulation script. 
This can be runned by the shell script (see 3. power_script.sh) and takes an index number (1–10) to decide which simulation condition to run. The output of the file is one `.rds` file with 1000 simulation results per condition.

Details:
Steps:
- Loads simulation settings based on the given index.
- Simulates fixation data using `createPlineData()` with different slope, AR(1), and error assumptions.
- Fits the data using `bfit()` from the `bdots` package.
- Runs three significance tests:
  - Homogeneous bootstrap
  - Heterogeneous bootstrap
  - Permutation test
- Repeats 1000 times.
- Saves the results as a `.rds` file.
- Logs progress to a text file in `prog_txt/`.

2. over_time_power_plot.R
This file contains the functions for analyzing the simulated results.

Functions:
- `timetiePower()`: Turns results to vectors & show which time points were significant.
- `getDiffSlices()`: Makes basic R plots to present the results from the simulation.
- `getDiffSlicesgg()`: Makes `ggplot2` plots showing power over time, including helpful lines for expected timing.

3. power_script.sh
This is a shell script that runs the simulation.

4. calc_power.R *(optional)
This script reads the simulation result files (saved as `.rds`), calculates summary statistics for power analysis, and produces formatted summary tables showing Type I error, Type II error, and power for different methods.

5. rds_files
This folder stores the output files from the simulation runs. Each file is saved in R’s binary format (`.rds`) and contains the results from multiple simulation iterations for a given simulation setting.
