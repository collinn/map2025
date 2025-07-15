library(bdots)
library(eyetrackSim)
library(ggplot2)
library(data.table)
library(gridExtra)

#' Computes logical vector indicating time points where difference is expected
#' @param mm: A matrix of start and end times
#' @return: A logical vector of length 401 (one per time point from -1 to 1) indicating presence of power.
timetiePower <- function(mm) {
  time <- seq(-1, 1, length.out = 401)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    time[time > m[1] & time < m[2] + 0.005]
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}

# Plots raw power values over time using base R graphics from an RDS simulation result file
getDiffSlices <- function(ff, ww) {
  rr <- readRDS(ff)

  tit <- switch(ww,
                "Homogenous Means, AR(1) Error",
                "Heterogenous Means, AR(1) Error",
                "Heterogeneous Means, No AR(1) Error")

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)

  ll <- length(rr)

  plot(time, smm, type = 'l', col = 'green', main = tit, ylim = c(0,ll), ylab = "Power")
  lines(time, mmm, col = 'black')
  lines(time, pmm, col = 'blue')
  lines(seq(-1, 0, 0.5), rep(5,3), col = 'red', lty = 2, lwd = 2)
  lines(c(0,0), c(0.05*ll, ll), col = 'red', lty = 2, lwd = 2)
  legend("bottomright", legend = c("Hom. Boot", "Het. Boot", "Permutation"),
         col = c("green", "black", "blue"), lty = 1, lwd = 1)
}

# Generates ggplot2 plots showing power over time, including helpful reference lines for expected timing
getDiffSlicesgg <- function(ff, ww, leg = FALSE) {
  rr <- readRDS(ff)

  tt <- attributes(rr)[[1]]

  mt <- ifelse(tt$mm, "Heterogeneous Means\n ", "Homogeneous Means\n")
  art <- ifelse(tt$ar, "AR(1) Error, ", "IID Error, ")
  ars <- ifelse(tt$bcor, "AR(1) Specified", "AR(1) Not Specified")

  tit <- paste0(mt, art, ars)

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)

  dat <- data.table(Method = rep(c("Hom. Boot", "Het. Boot", "Permutation"), each = 401),
                    Power = c(smm, mmm, pmm),
                    Time = rep(time, 3))

  ll <- length(rr)

  pp <- ggplot(dat, aes(Time, Power, color = Method)) + theme_bw() + ggtitle(tit) +
    geom_line(linewidth = 1) +
    geom_function(fun = Vectorize(function(x) {
      ifelse(x < 0, 0.05*ll, NA)
    }), color = 'red', linetype = "dashed", linewidth = 1.25) +
    geom_abline(slope = 1e5, intercept = 0, color = 'red', linetype = 'dashed',
                linewidth = 1.25) + ylim(c(0, ll)) +
    scale_color_manual(values = c("#00BFC4", "#7CAE00", "#C77CFF")) +
    theme(legend.position = ifelse(leg, "bottom", "none"))

  return(pp)
}

#sds <- data.table(mm = rep(c(FALSE, TRUE, TRUE, TRUE, TRUE), 2),
#                  ar = rep(c(TRUE, TRUE, TRUE, FALSE, FALSE), 2),
#                  bcor = rep(c(TRUE, TRUE, FALSE, TRUE, FALSE), 2),
#                  slope = rep(c(0.025, 0.25), each = 5))

sds <- expand.grid(mm = c(T,F),
                   ar = c(T,F), 
                   slope = c(0.025, 0.25))

ff <- list.files("~/dissertation/writing/methodology/scripts/power/rds_files",
                 full.names = TRUE, pattern = "rds")
ff <- ff[c(1, 3:10, 2)]

pdf("~/Desktop/2025-map/type_two_err_time_slice.pdf", width = 5, height = 7.5)
par(mfrow = c(3, 1))
getDiffSlices(ff[1], 1)
getDiffSlices(ff[2], 2)
getDiffSlices(ff[3], 3)
dev.off()

#pdf("~/dissertation/writing/methodology/img/type_two_error_time_a.pdf", width = 5, height = 2.5)
pdf("~/Desktop/2025-map/type_two_error_time_a.pdf", width = 5, height = 2.5)
print(getDiffSlicesgg(ff[1], 1, leg = FALSE))
dev.off()

pdf("~/Desktop/2025-map/type_two_error_time_b.pdf", width = 5, height = 2.5)
print(getDiffSlicesgg(ff[2], 2, leg = FALSE))
dev.off()

pdf("~/Desktop/2025-map/type_two_error_time_c.pdf", width = 5, height = 2.5)
print(getDiffSlicesgg(ff[3], 3, leg = FALSE))
dev.off()

# Create combined plots (example with slope=0.25 condition)
b6 <- getDiffSlicesgg(ff[6], 6, leg = FALSE)
b7 <- getDiffSlicesgg(ff[7], 7, leg = FALSE)
b8 <- getDiffSlicesgg(ff[8], 8, leg = FALSE)
b9 <- getDiffSlicesgg(ff[9], 9, leg = FALSE)
b10 <- getDiffSlicesgg(ff[10], 10, leg = TRUE)

pdf("~/Desktop/2025-map/full_power_25.pdf", width = 7, height = 8.5)
ggpubr::ggarrange(b6, b7, b8, b9, b10,
                  nrow = 3, ncol = 2,
                  common.legend = TRUE, legend = "bottom")
dev.off()

# Single plot example
b6_single <- getDiffSlicesgg(ff[9], 9, leg = TRUE) + ggtitle("Power Simulation")

pdf("~/Desktop/2025-map/power_sample.pdf", width = 5, height = 5)
print(b6_single)
dev.off()

# Combine plots side by side
pdf("~/Desktop/2025-map/method_image.pdf", width = 5, height = 3.5)
pp <- getDiffSlicesgg(ff[1], 1)
gridExtra::grid.arrange(pp, b6_single, nrow = 1)
dev.off()
