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
library(dplyr) # For data manipulation

#### 02.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 02.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")

#### 02.2 Run STM ####
### Create list of model output
l.out.stm <- f.decision_model(l.params.all = l.params.all)
### Plot Markov cohort trace
matplot(l.out.stm$m.M,
        xlab = "Cycle", ylab = "Proportion")
legend("right", legend = l.params.all$v.n, 
       pch = as.character(1:4), col = 1:4)
