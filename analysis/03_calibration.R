################################################################################ 
# This script calibrates the Sick-Sicker state-transition model (STM) to       #
# epidemiological targets using a Bayesian approach with the Incremental       #
# Mixture Importance Samping (IMIS) algorithm                                 #
#                                                                              # 
# Depends on:                                                                  #
#   00_general_functions.R                                                     #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   03_calibration_functions.R                                                 #
#                                                                              # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              # 
#     - Eline Krijkamp, MS                                                     #
#     - Petros Pechlivanoglou, PhD                                             #
#     - Hawre Jalal, MD, PhD                                                   #
#     - Eva A. Enns, PhD                                                       # 
################################################################################ 
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################ 
# rm(list = ls()) # to clean the workspace

#### 03.1 Load packages, data and functions ####
#### 03.1.1 Load packages and functions ####
# Calibration functionality
library(lhs) # latin hypercube sampling
library(IMIS)
library(matrixStats)
library(modeest) # to estimate mode

# Visualization
library(plotrix)       # plots with lower and upper error bands
library(psych)         # pairwise histograms
library(scatterplot3d) # 3D scatterplots

#### 03.1.2 Load inputs ####
source("analysis/01_model-inputs.R")

#### 03.1.3 Load functions ####
source("R/00_general_functions.R")
source("R/02_simulation-model_functions.R")
source("R/03_calibration_functions.R")

#### 03.1.4 Load calibration targets ####
load("data/03_calibration-targets.RData")

#### 03.2 Visualize targets ####
### TARGET 1: Survival ("Surv")
plotrix::plotCI(x = SickSicker.targets$Surv$Time, 
                y = SickSicker.targets$Surv$value, 
                ui = SickSicker.targets$Surv$ub,
                li = SickSicker.targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Alive)")

### TARGET 2: Prevalence ("Prev")
plotrix::plotCI(x = SickSicker.targets$Prev$Time, 
                y = SickSicker.targets$Prev$value, 
                ui = SickSicker.targets$Prev$ub,
                li = SickSicker.targets$Prev$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick+Sicker)")

### TARGET 3: Proportion who are Sicker ("PropSicker"), among all those 
###           afflicted (Sick+Sicker)
plotrix::plotCI(x = SickSicker.targets$PropSick$Time, 
                y = SickSicker.targets$PropSick$value, 
                ui = SickSicker.targets$PropSick$ub,
                li = SickSicker.targets$PropSick$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sicker | Sick+Sicker)")

#### 03.3 Run calibration algorithms ####
# Check that it works
v.params.calib = c(p.S1S2 = 0.105, hr.S1 = 3, hr.S2 = 10)
f.calibration_out(v.params.calib = v.params.calib, l.params.all = l.params.all)

#### 03.3.1 Specify calibration parameters ####
### Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

### Number of random samples to obtain from the posterior distribution 
n.resamp <- 1000

### Names and number of input parameters to be calibrated
v.param.names <- c("p.S1S2", "hr.S1", "hr.S2")
n.param       <- length(v.param.names)

### Vector with range on input search space
v.lb <- c(p.S1S2 = 0.01, hr.S1 = 1.0, hr.S2 = 5)  # lower bound
v.ub <- c(p.S1S2 = 0.50, hr.S1 = 4.5, hr.S2 = 15) # upper bound

### Number of calibration targets
v.target.names <- c("Surv", "Prev", "PropSick")
n.target       <- length(v.target.names)

#### 03.3.2 Run IMIS algorithm ####
l.fit.imis <- IMIS(B = 1000, # incremental sample size at each iteration of IMIS
                   B.re = n.resamp, # desired posterior sample size
                   number_k = 10, # maximum number of iterations in IMIS
                   D = 0)
### Obtain posterior
m.calib.post <- l.fit.imis$resample

#### 03.4 Exploring posterior distribution ####
#### 03.4.1 Summary statistics of posterior distribution ####
### Compute posterior mean
v.calib.post.mean <- colMeans(m.calib.post)

### Compute posterior median and 95% credible interval
m.calib.post.95cr <- colQuantiles(m.calib.post, probs = c(0.025, 0.5, 0.975))

### Compute posterior mode
v.calib.post.mode <- apply(m.calib.post, 2, 
                           function(x) as.numeric(mlv(x, method = "shorth")[1]))

# Compute posterior values for draw
v.calib.post <- exp(f.log_post(m.calib.post))

# Compute maximum-a-posteriori (MAP)
v.calib.post.map <- m.calib.post[which.max(v.calib.post), ]

# Summary statistics
df.posterior.summ <- data.frame(
  Parameter = v.param.names,
  Mean      = v.calib.post.mean,
  m.calib.post.95cr,
  Mode      = v.calib.post.mode,
  MAP       = v.calib.post.map,
  check.names = FALSE)
df.posterior.summ

### Save summary statistics of posterior distribution
## As .RData
save(df.posterior.summ, 
     file = "tables/03_summary-posterior.RData")
## As .csv
write.csv(df.posterior.summ, 
          file = "tables/03_summary-posterior.csv", 
          row.names = FALSE)

#### 03.4.2 Visualization of posterior distribution ####
### Rescale posterior to plot density of plots
v.calib.alpha <- scales::rescale(v.calib.post)

### Plot the 1000 draws from the posterior
png("figs/03_posterior-distribution-joint.png", 
    width = 8, height = 6, units = 'in', res = 300)
  s3d <- scatterplot3d(x = m.calib.post[, 1],
                       y = m.calib.post[, 2],
                       z = m.calib.post[, 3],
                       color = scales::alpha("black", v.calib.alpha),
                       xlim = c(v.lb[1], v.ub[1]), 
                       ylim = c(v.lb[2], v.ub[2]), 
                       zlim = c(v.lb[3], v.ub[3]),
                       xlab = v.param.names[1], 
                       ylab = v.param.names[2], 
                       zlab = v.param.names[3])
  ## Add center of Gaussian components
  s3d$points3d(l.fit.imis$center, col = "red", pch = 8)
  ## Add legend
  legend(s3d$xyz.convert(0.05, 1.0, 5), 
         col = c("black", "red"), 
         bg = "white", pch = c(1, 8), yjust = 0, 
         legend = c("Posterior sample", "Center of Gaussian components"), 
         cex = 1.1)
dev.off()

### Plot the 1000 draws from the posterior with marginal histograms
png("figs/03_posterior-distribution-marginal.png", 
    width = 8, height = 6, units = 'in', res = 300)
  pairs.panels(m.calib.post)
dev.off()

#### 03.5 Store posterior and MAP from IMIS calibration ####
save(m.calib.post,
     v.calib.post.map,
     file = "output/03_imis-output.RData")
