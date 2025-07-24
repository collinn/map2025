# Load required R packages
library(bdots)
library(eyetrackSim)

## This compute overall TIE across all time points
getFWER <- function(y) {
  sigs <- readRDS(y) # Load simulation results from RDS file
  # Compute overall FWER: proportion of sims with any significant timepoint

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean() # homogeneous bootstrap method
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean() # Heterogeneous bootstrap method
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean() # Permutation method
               
  # Compute per-timepoint error rates across simulations
  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans() # homogeneous bootstrap per-timepoint error rates
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans() # Heterogeneous bootstrap per-timepoint error rates
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans() # Permutation per-timepoint error rates

  fwer <- data.table(sm = sm, mm = mm, pm = pm)
  tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
                  dimnames = list(c("sm", "mm", "pm"), NULL))
  return(list(fwer = fwer, timeSliceMat = tsmat))
}

## This takes the list returned by getfwer
timeSliceFwer <- function(y, f = median) {
  nn <- length(y)
  y <- lapply(y, `[[`, 2) # Extract time-slice matrices
  y <- lapply(y, function(z) {
    apply(z, 1, f) # Apply summary function across timepoints
  })
  y <- Reduce(rbind, y)
  y
}

## Given signifiance mat, need to return length 401 bool vector
timetie <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec) # Default: return all FALSE if no significance
    return(vec)
  }

  # Convert significance windows to individual timepoints
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, 4)))
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}

##########3---------------------------------------------------------
# Load simulation output files
ff <- list.files("~/Desktop/2025-map/map2025/new_scripts/new_fwer/fwer_rds/", full.names = TRUE)

## Sim settings - recreate parameter grid from simulation
sds <- expand.grid(mm = c(T,F), # many-means/single-means parameter
                   ar = c(T,F), # AR(1) parameter
                   bcor = F) # Bootstrap correlation
bcor_row <- data.frame(mms = F, ar = T, bcor = T) # Additional parameter combo for bcor = T
sds <- rbind(bcor_row, sds)

# Process all simulation files to compute FWER
gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- as.data.table(cbind(sds, fwer))[order(rev(mms), ar, decreasing = TRUE), ]
tfwer <- as.data.table(cbind(sds, tfwer))[order(rev(mms), ar, decreasing = TRUE)]

# Re-sort by mms for order
fwer <- fwer[order(mms), ]
tfwer <- tfwer[order(mms), ]

# Convert logical parameters to readable labels for tables
fwer[, `:=`(bcor = ifelse(bcor, "Yes", "No"),
            ar = ifelse(ar, "Yes", "No"),
            mms = ifelse(mms, "Yes", "No"))]

tfwer[, `:=`(bcor = ifelse(bcor, "Yes", "No"),
            ar = ifelse(ar, "Yes", "No"),
            mms = ifelse(mms, "Yes", "No"))]

fwer <- fwer[, .(mms, ar, bcor, sm, mm, pm)]
tfwer <- tfwer[, .(mms, ar, bcor, sm, mm, pm)]

## For LaTeX table generation
# rev(paired)
library(xtable)
## unpaired
#fwers[[1]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
# Paired #1
#fwers[[2]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
#fwers[[3]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)

#tfwers[[1]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
#tfwers[[2]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)
#tfwers[[3]][, c(1:3,6:8)] |> xtable() |> print(include.rownames = FALSE)

#twfer |> xtable() |> print(include.rownames = FALSE)

