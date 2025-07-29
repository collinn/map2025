## Run power simulations for different parameter settings using bdots and eyetrackSim packages
library(bdots)
library(eyetrackSim)

## idx from 1-5
sds <- expand.grid(mm = c(T, F), 
                   ar = c(T, F),
                   bcor = F)
bcor_row <- data.frame(mm = F, ar = T, bcor = T)
sds <- rbind(bcor_row, sds)

## Get simulation index from command line arguments
idx <- as.numeric(commandArgs(TRUE))

## Select the parameter set for this simulation run
sidx <- sds[idx, ]

#' Creates fits using generated data and bdots
#' @param sidx set of parameters
createFits <- function(sidx) {
  ## Slopes for piecewise lines
  ppars <- c(0, 0.25)

  ## Generate piecewise linear data based on simulation parameters
  dat <- createPlineData(manymeans = sidx$mm,
                         ar1 = sidx$ar,
                         distSig = 0.05,
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
                  cor = sidx$bcor)
  fit
}

N <- 25 # Number of simulations
sims <- vector("list", length = N)
attr(sims, "simsettings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("power_rds/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  fit <- createFits(sidx)
  
  sm <- bboot(formula = fixations ~ group(A, B),
              bdObj = fit, singleMeans = TRUE,
              permutation = FALSE)$sigTime
  
  mm <- bboot(formula = fixations ~ group(A, B),
              bdObj = fit, singleMeans = FALSE,
              permutation = FALSE)$sigTime
  
  pm <- suppressMessages(bboot(formula = fixations ~ group(A, B),
                               bdObj = fit, skipDist = FALSE,
                               permutation = TRUE))$sigTime
  sims[[i]] <- list(singlemean = sm,
                   manymeans = mm,
                   permutation = pm)
  
  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}

# Save simulation results to .rds file
saveRDS(sims, rf)
