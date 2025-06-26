library(bdots)
library(eyetrackSim)
library(xtable)
library(data.table)

#' Extracts the significant time
#' chunks from the large RDS file
#' @param ff path to file
getSigTimes <- function(ff) {
  rr <- readRDS(ff)
  
  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)
  
  smt <- lapply(sm, `[[`, "sigTime")
  mmt <- lapply(mm, `[[`, "sigTime")
  pmt <- lapply(pm, `[[`, "sigTime")
  
  list(smt, mmt, pmt)
}

#' Generates a power from an RDS file
#' @param ff path to file
getPowerTab <- function(ff) {
  ## change parameter name to rr and comment out
  ## the line below if running getSigTimes before
  rr <- readRDS(ff)
  
  sm <- rr[[1]]
  mm <- rr[[2]]
  pm <- rr[[3]]

  powerdetector <- function(mm) {
    # type 2 error if no difference detected
    if (is.null(mm)) return(-200)
    
    # type 1 error if difference detected before true difference
    if (min(mm) < 0) {
      return(-100)
    }
    
    # if no error occurred, return the first difference detected
    return(min(mm))
  }

  smt <- vapply(sm, powerdetector, numeric(1))
  mmt <- vapply(mm, powerdetector, numeric(1))
  pmt <- vapply(pm, powerdetector, numeric(1))

  makeSummary <- function(vv) {
    tie <- mean(vv == -100)
    t2e <- mean(vv == -200)
    pwr <- 1 - mean(vv %in% c(-100, -200))
    ssm <- summary(vv[!(vv %in% c(-100, -200))])[c(1:3, 5:6)]
    ssm <- c(tie, t2e, pwr, ssm)
    names(ssm)[1:3] <- c("Type I Error", "Type II Error", "Power")
    ssm
  }

  list(
    sm = makeSummary(smt),
    mm = makeSummary(mmt),
    pm = makeSummary(pmt)
  )
}

# File list and subsets
ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")
ff <- ff[c(1, 3:10, 2)]

## run if you have the whole RDS file, not just sigTimes
#res1 <- lapply(ff, getSigTimes)
res <- lapply(res1, getPowerTab)

res_sm <- rbindlist(lapply(res, function(z) as.list(z$sm)))
res_mm <- rbindlist(lapply(res, function(z) as.list(z$mm)))
res_pm <- rbindlist(lapply(res, function(z) as.list(z$pm)))

sds <- data.table(
  mm = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  ar = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  bcor = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)

res_sm <- cbind(sds, res_sm)[order(mm, ar)]
res_mm <- cbind(sds, res_mm)[order(mm, ar)]
res_pm <- cbind(sds, res_pm)[order(mm, ar)]

# Select relevant columns and round numeric columns
cols_to_keep <- c(1:6, 8:10)
res_sm <- res_sm[, ..cols_to_keep]
res_mm <- res_mm[, ..cols_to_keep]
res_pm <- res_pm[, ..cols_to_keep]

#res_sm[, 4:9] <- round(.SD, 3), .SDcols = 4:9
#res_mm[, 4:9] <- round(.SD, 3), .SDcols = 4:9
#res_pm[, 4:9] <- round(.SD, 3), .SDcols = 4:9

# Add method label columns
res_sm <- cbind(Method = "Hom. Boot", res_sm)
res_mm <- cbind(Method = "Het. Boot", res_mm)
res_pm <- cbind(Method = "Perm", res_pm)

# Combine all methods and label factors for readability
tab <- rbind(res_sm, res_mm, res_pm)
tab[, mm := ifelse(mm, "Yes", "No")]
tab[, ar := ifelse(ar, "Yes", "No")]
tab[, bcor := ifelse(bcor, "Yes", "No")]

tab <- tab[order(mm, ar, bcor)]

digits <- c(1,1,1,1,1,2,2,2,3,3,3)
print(xtable(tab, caption = "Power for methods", align = c("lllllcccccc"),
             label = "tab:power_methods", digits = digits),
      include.rownames = FALSE)

# Final summary table
finalSummary <- rbind(
  colMeans(res_sm[, 5:10]),
  colMeans(res_mm[, 5:10]),
  colMeans(res_pm[, 5:10])
) |> as.data.table()

finalSummary <- cbind(Method = c("Hom. Bootstrap", "Het. Bootstrap", "Permutation"), finalSummary)

print(xtable(finalSummary, caption = "Summary of methods for Type II error",
             label = "tab:type_2_summary", digits = 3),
      include.rownames = FALSE)


# Abbreviated table (slope=0.25, no bcor)
tab_abr <- tab[c(1:12)]

digits_abr <- c(1,1,1,1,2,2,2,3,3,3)
print(xtable(tab_abr, caption = "Power for methods", align = c("llllcccccc"),
             label = "tab:power_methods", digits = digits_abr),
      include.rownames = FALSE)

finalSummary_abr <- rbind(
  colMeans(res_sm[c(1,3,5), 5:10]),
  colMeans(res_mm[c(1,3,5), 5:10]),
  colMeans(res_pm[c(1,3,5), 5:10])
) |> as.data.table()

finalSummary_abr <- cbind(Method = c("Hom. Bootstrap", "Het. Bootstrap", "Permutation"), finalSummary_abr)

print(xtable(finalSummary_abr, caption = "Summary of methods for Type II error",
             label = "tab:type_2_summary", digits = 3),
      include.rownames = FALSE)
