
library(eyetrackSim)
library(bdots)
library(mvtnorm)
library(ggplot2)
library(dplyr)



n <- 50


## First do TIE one
## Column `true` has the samples I want
time <- seq(0, 1600, 4)
dts <- createData(n = 50, paired = FALSE)$dts

truep <- eyetrackSim:::EMPIRICAL_START_PARS$mean
tf <- logistic_f(truep, time)

dts$sample <- TRUE
truedt <- data.table(id = 999, time = time, group = "A",
                     true = tf, fixations = tf, sample = FALSE)
dts <- rbind(dts, truedt)

pdf("../logistic_distribution.pdf", width = 5, height = 4)
ggplot(dts, aes(time, true, group = id, alpha = sample)) +
  geom_line(lwd = 1) +
  scale_alpha_discrete(range = c(0.95, 0.05)) +
  theme_bw() + theme(legend.position = "none") +
  labs(y = "", x = "Time")
dev.off()

## Now do power
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
                  group = "B", true = pline(p = c(0,0.0), time, "B"),
                  sample = TRUE)

dat <- rbindlist(list(dat, td1, td2))

dat$Condition <- dat$group

dat$Condition <- ifelse(dat$Condition == "A", "Effect", "No Effect")

dat <- dat %>% 
  group_by(group, time) %>% 
  mutate(mean = mean(true),
            sd = sd(true),
            n = n(),
            se = sd/sqrt(n),
            ci_lower = mean - qt(0.975, df = n - 1) * se,
            ci_upper = mean + qt(0.975, df = n - 1) * se,
            .groups = "drop")


pdf("piecewise_distribution.pdf", width = 5, height = 4)
ggplot(dat, aes(time, true, group = id, alpha = sample, color = Condition)) +
  geom_line(lwd = 2) +
  scale_color_brewer(palette = "Set1")
  #scale_alpha_discrete(range = c(0.15, 0.95), guide = "none") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(y = "", x = "Time") #+ ggtitle("Piecewise Distribution")

#ggplot(dat, aes(time, mean, group = id, alpha = sample, color = Condition, fill = Condition)) +
#  geom_line() +
#  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.7, color = NA) +
#  scale_alpha_discrete(range = c(0.15, 0.95), guide = "none") +
#  theme_bw() + theme(legend.position = "bottom") +
#  labs(y = "", x = "Time") #+ ggtitle("Piecewise Distribution")
dev.off()
