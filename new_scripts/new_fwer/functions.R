library(bdots)
library(eyetrackSim)

## This compute overall TIE across all time points
getFWER <- function(y) {
  sigs <- readRDS(y)

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()

  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans()
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans()
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans()

  fwer <- data.table(sm = sm, mm = mm, pm = pm)
  tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
                  dimnames = list(c("sm", "mm", "pm"), NULL))
  return(list(fwer = fwer, timeSliceMat = tsmat))
}

## This takes the list returned by getfwer
timeSliceFwer <- function(y, f = median) {
  nn <- length(y)
  y <- lapply(y, `[[`, 2)
  y <- lapply(y, function(z) {
    apply(z, 1, f)
  })
  y <- Reduce(rbind, y)
  y
}

## Given signifiance mat, need to return length 401 bool vector
timetie <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, 4)))
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}

##########3---------------------------------------------------------

ff <- list.files("~/Desktop/2025-map/map2025/new_scripts/new_fwer/fwer_rds/", full.names = TRUE)

## Sim settings
sds <- expand.grid(mms = c(T,F), ar = c(T,F), bcor = F)
bcor_row <- data.frame(mms = F, ar = T, bcor = T)
sds <- rbind(bcor_row, sds)

gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- as.data.table(cbind(sds, fwer))[order(rev(mms), ar, decreasing = TRUE), ]
tfwer <- as.data.table(cbind(sds, tfwer))[order(rev(mms), ar, decreasing = TRUE)]

fwer <- fwer[order(mms), ]
tfwer <- tfwer[order(mms), ]

fwer[, `:=`(bcor = ifelse(bcor, "Yes", "No"),
            ar = ifelse(ar, "Yes", "No"),
            mms = ifelse(mms, "Yes", "No"))]

tfwer[, `:=`(bcor = ifelse(bcor, "Yes", "No"),
            ar = ifelse(ar, "Yes", "No"),
            mms = ifelse(mms, "Yes", "No"))]

fwer <- fwer[, .(mms, ar, bcor, sm, mm, pm)]
tfwer <- tfwer[, .(mms, ar, bcor, sm, mm, pm)]


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

