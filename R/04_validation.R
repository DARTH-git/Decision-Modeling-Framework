################################################################################ 
# This script conducts an internal validation of the Sick-Sicker               # 
# state-transition model (STM) by comparing the model-predicted outputs        #
# evaluated at the calibrated parameters vs the calibration targets. This      #
# script could be modified by adding an external validation exercise.          #
#                                                                              # 
# Depends on:                                                                  #
#   00_general_functions.R                                                     #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   03_calibration_functions.R                                                 #
#   04_validation_functions.R                                                  #
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

#### 04.1 Load packages and functions ####
#### 04.1.1 Load packages and functions ####
### Data manipulation
library(reshape2) # to reshape data from wide to long
### Visualization
library(plotrix)  # plots with lower and upper error bands

#### 04.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 04.1.3 Load functions ####
source("functions/00_general_functions.R")
source("functions/02_simulation-model_functions.R")
source("functions/03_calibration_functions.R")
source("functions/04_validation-functions.R")

#### 04.1.4 Load targets and calibrated parameters ####
load("data/03_calibration-targets.RData")
load("output/03_imis-output.RData")

#### 04.2 Compute model-predicted outputs ####
#### 04.2.1 Compute model-predicted outputs for each sample of posterior distribution ####
### Number of posterior samples
n.samp <- nrow(m.calib.post)

### Define matrices to store model outputs
m.out.surv <- matrix(NA, nrow = n.samp, ncol = nrow(SickSicker.targets$Surv))
colnames(m.out.surv) <- SickSicker.targets$Surv$Time
m.out.prev <- matrix(NA, nrow = n.samp, ncol = nrow(SickSicker.targets$Prev))
colnames(m.out.prev) <- SickSicker.targets$Prev$Time
m.out.prop <- matrix(NA, nrow = n.samp, ncol = nrow(SickSicker.targets$PropSicker))
colnames(m.out.prop) <- SickSicker.targets$PropSicker$Time

### Evaluate model at each posterior sample and store results
for(i in 1:n.samp){ # i = 1
  l.out.post <- f.calibration_out(v.params.calib = m.calib.post[i, ], 
                                  l.params.all = l.params.all)
  m.out.surv[i, ] <- l.out.post$Surv
  m.out.prev[i, ] <- l.out.post$Prev
  m.out.prop[i, ] <- l.out.post$PropSicker
  cat('\r', paste(round(i/n.samp * 100), "% done", sep = " ")) # display progress
}

### Create data frames with model predicted outputs
df.out.surv <- data.frame(Type = "Model", 
                          Target = "Survival",
                          m.out.surv, 
                          check.names = FALSE)
df.out.prev <- data.frame(Type = "Model", 
                          Target = "Prevalence",
                          m.out.prev, 
                          check.names = FALSE)
df.out.prop <- data.frame(Type = "Model", 
                          Target = "Proportion of Sicker",
                          m.out.prop, 
                          check.names = FALSE)

### Transform data frames to long format
df.out.surv.lng <- reshape2::melt(df.out.surv, 
                     id.vars = c("Type", "Target"), 
                     variable.name = "Time")
df.out.prev.lng <- reshape2::melt(df.out.prev, 
                        id.vars = c("Type", "Target"), 
                        variable.name = "Time")
df.out.prop.lng <- reshape2::melt(df.out.prop, 
                        id.vars = c("Type", "Target"), 
                        variable.name = "Time")

### Compute posterior model-predicted 95% CI
df.out.surv.sum <- f.data_summary(df.out.surv.lng, varname = "value",
                             groupnames = c("Type", "Target", "Time"))
df.out.prev.sum <- f.data_summary(df.out.prev.lng, varname = "value",
                                groupnames = c("Type", "Target", "Time"))
df.out.prop.sum <- f.data_summary(df.out.prop.lng, varname = "value",
                                groupnames = c("Type", "Target", "Time"))

#### 04.3.2 Compute model-predicted outputs at MAP estimate ####
l.out.calib.map <- f.calibration_out(v.params.calib = v.calib.post.map, 
                                     l.params.all = l.params.all)

#### 04.4 Internal validation: Model-predicted outputs vs. targets ####
### TARGET 1: Survival ("Surv")
png("figs/04_posterior-vs-targets-survival.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker.targets$Surv$Time, y = SickSicker.targets$Surv$value, 
                ui = SickSicker.targets$Surv$ub,
                li = SickSicker.targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Alive)")
lines(x = SickSicker.targets$Surv$Time,
      y = df.out.surv.sum$lb, col = "red", lty = 2)
lines(x = SickSicker.targets$Surv$Time,
      y = df.out.surv.sum$ub, col = "red", lty = 2)
points(x = SickSicker.targets$Surv$Time, 
       y = l.out.calib.map$Surv, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()

### TARGET 2: Prevalence ("Prev")
png("figs/04_posterior-vs-targets-prevalence.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker.targets$Prev$Time, y = SickSicker.targets$Prev$value, 
                ui = SickSicker.targets$Prev$ub,
                li = SickSicker.targets$Prev$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick+Sicker)")
lines(x = SickSicker.targets$Prev$Time,
      y = df.out.prev.sum$lb, col = "red", lty = 2)
lines(x = SickSicker.targets$Prev$Time,
      y = df.out.prev.sum$ub, col = "red", lty = 2)
points(x = SickSicker.targets$Prev$Time, 
       y = l.out.calib.map$Prev, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()

### TARGET 3: Proportion who are Sicker ("PropSicker"), among all those afflicted (Sick+Sicker)
png("figs/04_posterior-vs-targets-proportion-sicker.png", 
    width = 8, height = 6, units = 'in', res = 300)
plotrix::plotCI(x = SickSicker.targets$PropSick$Time, y = SickSicker.targets$PropSick$value, 
                ui = SickSicker.targets$PropSick$ub,
                li = SickSicker.targets$PropSick$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sicker | Sick+Sicker)")
lines(x = SickSicker.targets$PropSicker$Time,
      y = df.out.prop.sum$lb, col = "red", lty = 2)
lines(x = SickSicker.targets$PropSicker$Time,
      y = df.out.prop.sum$ub, col = "red", lty = 2)
points(x = SickSicker.targets$PropSicker$Time, 
       y = l.out.calib.map$PropSicker, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", 
                  "Model-predicted 95% CrI",
                  "Model-predicted output at MAP"),
       col = c("black", "red", "red"), 
       pch = c(1, NA, 8),
       lty = c(NA, 2, NA))
dev.off()