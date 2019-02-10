################################################################################ 
# This script generates targets from the microsimulation implementation of the #
# Sick-Sicker model for a given and known parameter set                        #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   app1_generate-targets_functions.R                                                     #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################ 
# rm(list = ls())

#### App.1 Load packages and functions ####
#### App.1.1 Load packages ####
library(dplyr)
library(epitools)
library(ggplot2)

#### App.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### App.1.3 Load functions ####
source("functions/app1_generate-targets_functions.R")

#### App.2 Generate ONE set of targets ####
#### App.2.1 Define target generation inputs ####
## Vector of true parameters
df.true.params <- f.define_init_params()
## Number of individuals to simulate
n.i <- 500  
## Time frame to generate targets
n.t.targets <- 30
#### App.2.2 Run microsimulation model to generate target ####
SickSicker.targets <- f.sicksicker_micsim(v.params = df.true.params, 
                              n.i = n.i, n.t = n.t.targets, v.n = v.n, 
                              plot.targets = TRUE,
                              seed = 20190210)

#### App.3 Save targets ####
save(SickSicker.targets, file = "data/app1_calibration-targets.RData")
