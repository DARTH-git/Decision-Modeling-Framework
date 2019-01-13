################################################################################ 
# This script runs the cohort implementation of the Sick-Sicker                #
# state-transition model (STM)                                                 #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
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
### Create list of deterministic model output
l.out.stm <- sicksicker_stm(v.params = v.params.init)

