## Run FWER simulations for different parameters using eyetrackSim and bdots
library(eyetrackSim)
library(bdots)

## idx from 1-4
sds <- expand.grid(mm = c(T,F), ar = c(T,F))

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
                  cor = FALSE)
  fit
}

N <- 25
res <- vector("list", length = N)
attr(res, "settings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("fwer_rds/", nn, ".rds")

ccores <- 4


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  fit <- createFits(sidx)

  sm <- bboot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE,
                  cores = ccores, permutation = FALSE)$sigTime

  mm <- bboot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = FALSE,
                  cores = ccores, permutation = FALSE)$sigTime

  pm <- suppressMessages(bboot(formula = fixations ~ group(A, B),
                                   bdObj = fit, permutation = TRUE, skipDist = FALSE,
                                   cores = ccores))$sigTime

  res[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(res, rf)
