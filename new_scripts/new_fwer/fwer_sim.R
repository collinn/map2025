## Run FWER simulations for different parameters using eyetrackSim and bdots
library(eyetrackSim)
library(bdots)

## The study examines 5 parameter combinations across different data generation conditions:
## idx from 1-5
sds <- expand.grid(mm = c(T,F), # many-means/single-means parameter
                   ar = c(T,F), # AR(1) parameter
                   bcor = F) # Bootstrap 
bcor_row <- data.frame(mm = F, ar = T, bcor = T) # Additional parameter combo for bcor = T
sds <- rbind(bcor_row, sds)

## Get simulation index from command line arguments
idx <- as.numeric(commandArgs(TRUE))

## Select the parameter set for this simulation run
sidx <- sds[idx, ]

#' Creates fits using generated data and bdots
#' @param sidx set of parameters
createFits <- function(sidx) {
  ## Generate data
  dat <- createData(paired = FALSE,
                    ar1 = sidx$ar,
                    manymeans = sidx$mm)

  ## Make them less reliable
  newtime <- seq(0, 1600, by = 40)
  dat$dts <- dat$dts[time %in% newtime, ]

  fit <- bfit(data = dat$dts,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveFun = logistic(),
                  cores = ccores,
                  cor = sidx$bcor)
  fit
}

N <- 25 # Number of replications/observations in each method
sims <- vector("list", length = N)
attr(sims, "settings") <- sidx

# Create file/path names for info storage
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("fwer_rds/", nn, ".rds")

ccores <- 4 # number of CPU for parallel processing

sink(file = sf)
print(paste0("starting index: ", idx)) # Print progress update
for (i in seq_len(N)) {
  fit <- createFits(sidx)
  
  # sm: Single means bootstrap
  sm <- bboot(formula = fixations ~ group(A, B),
              bdObj = fit, singleMeans = TRUE,
              cores = ccores, permutation = FALSE)$sigTime
  # mm = Many mean bootstrap
  mm <- bboot(formula = fixations ~ group(A, B),
              bdObj = fit, singleMeans = FALSE,
              cores = ccores, permutation = FALSE)$sigTime

  # permutation = No bootstrap, data go through resampling
  pm <- suppressMessages(bboot(formula = fixations ~ group(A, B),
                               bdObj = fit, permutation = TRUE, skipDist = FALSE,
                               cores = ccores))$sigTime
  
  # Store results from all three methods for this observation
  sims[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  # Provides progress update every 10 iterations
  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(sims, rf)
