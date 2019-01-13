################################################################################ 
# This script calibrates the Sick-Sicker state-transition model (STM) to       #
# epidemiological targets                                                      #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   02_calibration_functions.R                                                 #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################ 

# rm(list = ls()) # to clean the workspace

#### 03.1 Load packages and functions ####
#### 03.1.1 Load packages and functions ####
# Calibration functionality
library(lhs) # latin hypercube sampling

# Visualization
library(plotrix)       # plots with lower and upper error bands
library(psych)         # pairwise histograms
library(scatterplot3d) # 3D scatterplots

#### 03.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 03.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/03_calibration_functions.R")

#### 03.1.4 Load calibration targets ####
load("data/03_calibration-targets.RData")

#### 03.2 Visualize targets ####
# TARGET 1: Survival ("Surv")
plotrix::plotCI(x = SickSicker.targets$Surv$Time, y = SickSicker.targets$Surv$value, 
                ui = SickSicker.targets$Surv$ub,
                li = SickSicker.targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Alive)")

# TARGET 2: Prevalence ("Prev")
plotrix::plotCI(x = SickSicker.targets$Prev$Time, y = SickSicker.targets$Prev$value, 
                ui = SickSicker.targets$Prev$ub,
                li = SickSicker.targets$Prev$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick+Sicker)")

# TARGET 3: Proportion who are Sick ("PropSick"), among all those afflicted (Sick+Sicker)
plotrix::plotCI(x = SickSicker.targets$PropSick$Time, y = SickSicker.targets$PropSick$value, 
                ui = SickSicker.targets$PropSick$ub,
                li = SickSicker.targets$PropSick$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick | Sick+Sicker)")

#### 03.3 Run calibration algorithms ####
# Check that it works
v.params.calib = c(p.S1S2 = 0.105, hr.S1 = 3, hr.S2 = 10)
f.calibration_out(v.params.calib = v.params.calib)

#### 03.3.1 Specify calibration parameters ####
# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n.init <- 100

# names and number of input parameters to be calibrated
v.param.names <- c("p.S1S2", "hr.S1", "hr.S2")
n.param       <- length(v.param.names)

# range on input search space
lb <- c(p.S1S2 = 0.01, hr.S1 = 1.0, hr.S2 = 5)  # lower bound
ub <- c(p.S1S2 = 0.50, hr.S1 = 4.5, hr.S2 = 15) # upper bound

# number of calibration targets
v.target.names <- c("Surv", "Prev", "PropSick")
n.target       <- length(v.target.names)

#### 03.3.2 Sample multiple random starting values for Nelder-Mead ####
# Sample unit Latin Hypercube
m.lhs.unit <- randomLHS(n.init, n.param)

# Rescale to min/max of each parameter
m.params.init <- matrix(nrow = n.init, ncol = n.param)
for (i in 1:n.param){
  m.params.init[, i] = qunif(m.lhs.unit[, i],
                            min = lb[i],
                            max = ub[i])
}
colnames(m.params.init) <- v.param.names

# Visualize sample of starting points
psych::pairs.panels(m.params.init)

#### 03.3.3 Run Nelder-Mead for each starting point ####
m.calib.res <- matrix(nrow = n.init, ncol = n.param + 1)
colnames(m.calib.res) = c(v.param.names, "Overall_fit")

for (j in 1:n.init){
  # Run NM
  fit.nm <- optim(m.params.init[j, ], f.gof, 
                  control = list(fnscale = -1, # fnscale = -1 switches from minimization to maximization
                                 maxit = 1000), 
                  hessian = T)
  # Store caibrated parameters and GOF value at last NM iteration
  m.calib.res[j, ] <- c(fit.nm$par, fit.nm$value)
}

#### 03.4 Exploring best-fitting input sets ####
# Arrange parameter sets in order of fit
m.calib.res <- m.calib.res[order(-m.calib.res[, "Overall_fit"]),]

# Examine the top 10 best-fitting sets
m.calib.res[1:10, ]
# Plot the top 10 (top 10%)
scatterplot3d(x = m.calib.res[1:10, 1],
              y = m.calib.res[1:10, 2],
              z = m.calib.res[1:10, 3],
              xlim = c(lb[1], ub[1]), 
              ylim = c(lb[2], ub[2]), 
              zlim = c(lb[3], ub[3]),
              xlab = v.param.names[1], 
              ylab = v.param.names[2], 
              zlab = v.param.names[3])

#### 03.4.1 Store best parameter set from NM calibration ####
v.params.calib.best <- m.calib.res[1, -4]
save(v.params.calib.best, file = "data/03_nm-best-set.RData")

#### 03.5 Internal validation: Model-predicted ouput at best set vs. targets ####
l.out.calib <- f.calibration_out(m.calib.res[1, ])

# TARGET 1: Survival ("Surv")
plotrix::plotCI(x = SickSicker.targets$Surv$Time, y = SickSicker.targets$Surv$value, 
                ui = SickSicker.targets$Surv$ub,
                li = SickSicker.targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Alive)")
points(x = SickSicker.targets$Surv$Time, 
       y = l.out.calib$Surv, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

# TARGET 2: Prevalence ("Prev")
plotrix::plotCI(x = SickSicker.targets$Prev$Time, y = SickSicker.targets$Prev$value, 
                ui = SickSicker.targets$Prev$ub,
                li = SickSicker.targets$Prev$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick+Sicker)")
points(x = SickSicker.targets$Prev$Time, 
       y = l.out.calib$Prev, 
       pch = 8, col = "red")
legend("bottomright", 
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

# TARGET 3: Proportion who are Sick ("PropSick"), among all those afflicted (Sick+Sicker)
plotrix::plotCI(x = SickSicker.targets$PropSick$Time, y = SickSicker.targets$PropSick$value, 
                ui = SickSicker.targets$PropSick$ub,
                li = SickSicker.targets$PropSick$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr(Sick | Sick+Sicker)")
points(x = SickSicker.targets$PropSick$Time, 
       y = l.out.calib$PropSick, 
       pch = 8, col = "red")
legend("bottomleft", 
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

