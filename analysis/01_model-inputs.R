################################################################################ 
# This script generates all the required input parameters for the cohort       #
# implementation of the Sick-Sicker state-transition model (STM)               #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs_functions.R                                                #
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

#### 01.1 Load packages and functions ####
#### 01.1.1 Load packages and functions ####
library(dplyr) # For data manipulation

#### 01.1.2 Load functions ####
source("R/01_model-inputs_functions.R")

#### 01.2 Load all parameters ####
l.params.all <- f.load_all_params(file.init = "data/01_init-params.csv",
                                  file.mort = "data/01_all-cause-mortality.csv")
#### 01.3 Estimated parameters ####
