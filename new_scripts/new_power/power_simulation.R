# Run power simulations for different parameter settings using bdots and eyetrackSim packages
library(bdots)
library(eyetrackSim)

# idx from 1-16
sds <- expand.grid(mm = c(T,F),
                   ar = c(T,F), 
                   # remove bcor
                   sigVal = c(0.025, 0.01),
                   slope = c(0.025, 0.25))

# Get simulation index from command line arguments
idx <- as.numeric(commandArgs(TRUE))

# Select the parameter set for this simulation run
sidx <- sds[idx, ]

#' Creates fits using generated data and bdots
#' @param sidx set of parameters
#' @param nit number of iterations
createFits <- function(sidx, nit = 250) {

  slp <- sidx$slope
  ppars <- c(0, slp)

  ## only setting one of the times, generate piecewise linear data based on simulation parameters
  dat <- createPlineData(manymeans = sidx$mm,
                         ar1 = FALSE, #sidx$ar,
                         distSig = sidx$sigVal,
                         paired = FALSE,
                         pars = ppars,
                         TIME = seq(-1,1, by = 0.005))

  fit <- bfit(data = dat,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveFun = plinePars(),
                  cores = detectCores() - 1L,
                  cor = FALSE) #sidx$bcor)


  sm <- bboot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE, Niter = nit)$sigTime
  mm <- bboot(formula = fixations ~ group(A, B),
                  bdObj = fit, Niter = nit)$sigTime
  pm <- suppressMessages(bboot(formula = fixations ~ group(A, B),
                  bdObj = fit, skipDist = TRUE, Niter = nit,
                  permutation = TRUE)$sigTime)
  list(singlemean = sm,
       manymean = mm,
       permutation = pm)
}

N <- 100 # Number of simulations
sims <- vector("list", length = N)
attr(sims, "simsettings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("negative_one_to_one_rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  sims[[i]] <- createFits(sidx)
  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}

# Save simulation results to .rds file
saveRDS(sims, rf)