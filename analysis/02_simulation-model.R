################################################################################ 
# This script runs the cohort implementation of the Sick-Sicker                #
# state-transition model (STM)                                                 #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
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

#### 02.1 Load packages and functions ####
#### 02.1.1 Load packages and functions ####
library(dplyr)    # For data manipulation
library(survival) # For plotting state-transition diagram

#### 02.1.2 Load inputs ####
source("analysis/01_model-inputs.R")

#### 02.1.3 Load functions ####
source("R/02_simulation-model_functions.R")

#### 02.2 Run STM ####
### Create list of model output
l.out.stm <- f.decision_model(l.params.all = l.params.all)

### Plot Markov cohort trace
png("figs/02_trace-plot.png")
  matplot(l.out.stm$m.M,
          xlab = "Cycle", ylab = "Proportion")
  legend("right", legend = l.params.all$v.n, 
         pch = as.character(1:4), col = 1:4)
dev.off()

### Plot state-transition diagram
png("figs/02_model-diagram.png")
  connect <- (l.out.stm$a.P[,,1] > 0)
  survival::statefig(layout = c(2, 2), connect = connect )
dev.off()
