# 📊 fwer README

### ✅ **Fwer_sim.R**
- Main purpose:  
  Generate **25 simulations** for each setting  
  Fit models  
  Store which time points are found to be significant

---

### ✅ **functions.R**
- Main purpose:  
  Contains helper functions to:
  - Calculate family-wise error rate (FWER)
  - Check which time points are significant
  - Combine and format results for tables

---

## 📂 Output folders
| Folder         | What’s inside                                    |
|----------------|--------------------------------------------------|
| `prog_txt/`    | Progress logs while running simulations         |
| `rds_files/`   | Results of each simulation saved as .rds files  |

---

## 🛠 How to run
```r
# Run simulation (replace 1 with your scenario index)
Rscript Fwer_sim.R 1

# Run analysis after simulations
Rscript functions.R


 
