library(eyetrackSim)
library(dplyr)
library(ggplot2)
library(gridExtra)

PARS <- c(0, 0.15)
dat <- createPlineData(n = 50, distSig = 0.05, pars = PARS,
                       TIME = seq(-1, 1, length.out = 401))
dat$fixations <- NULL
dat$sample <- FALSE
time <- seq(-1, 1, length.out = 401)

td1 <- data.table(id = 900, time = time,
                  group = "A", true = pline(p = PARS, time, "A"),
                  sample = TRUE)

td2 <- data.table(id = 950, time = time,
                  group = "B", true = pline(p = c(0, 0), time, "B"),
                  sample = TRUE)

td1$Condition <- "Effect"
td2$Condition <- "No Effect"

dat <- rbind(td1, td2)

theme_set(theme_bw(base_size = 16))
plot1 <- ggplot(dat, aes(time, true, color = Condition, group = id)) + 
  geom_line(linewidth = 2) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Time", title = "Mean structure") +
  xlim(-1,1)

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

getDiffSlicesgg <- function(ff, ww, leg = FALSE) {
  rr <- readRDS(ff)
  
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
  
  theme_set(theme_bw(base_size = 16))
  pp <- ggplot(dat, aes(Time, Power / 25, color = Method)) + theme_bw() +
    geom_line(linewidth = 1) +
    geom_function(fun = Vectorize(function(x) {
      ifelse(x < 0, 0.05*ll, NA)
    }), color = 'red', linetype = "dashed", linewidth = 1.25) +
    geom_abline(slope = 1e5, intercept = 0, color = 'red', linetype = 'dashed',
                linewidth = 1.25) + ylim(c(0, ll)) +
    #scale_color_manual(values = c("#00BFC4", "#7CAE00", "#C77CFF")) +
    theme(legend.position = ifelse(leg, "bottom", "none")) +
    coord_cartesian(ylim = c(0,1)) +
    labs(y = "Proportion", title = "Power over time")
  
  return(pp)
}

sds <- expand.grid(mm = c(T, F), 
                   ar = c(T, F),
                   bcor = F)
bcor_row <- data.frame(mm = F, ar = T, bcor = T)
sds <- rbind(bcor_row, sds)

ff <- list.files("~/Desktop/2025-map/map2025/new_scripts/new_power/power_rds/",
                 full.names = TRUE, pattern = "rds")

plot2 <- getDiffSlicesgg(ff[4], 4, leg = TRUE)

png("~/Desktop/2025-map/map2025/mean_structure_power_plot.png", width = 800, height = 400)
grid.arrange(plot1, plot2, ncol = 2)
dev.off()
