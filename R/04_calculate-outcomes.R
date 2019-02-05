################################################################################ 
# This script runs the cost-effectiveness analysis of a hypothetical treatment #
# for the simulated cohort of the Sick-Sicker state-transition model (STM)     #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################

# rm(list = ls()) # to clean the workspace

#### 04.1 Load packages and functions ####
#### 04.1.1 Load packages ####

#### 04.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 04.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/04_calculate-outcomes_functions.R")

#### 04.2 Cost-effectiveness analysis parameters ####
## Strategy names
v.names.str <- c("No Treatment", "Treatment")  
## Number of strategies
n.str <- length(v.names.str)
v.params.basecase <- f.generate_basecase_params()

#### 04.3 Compute cost-effectiveness outcomes ####
f.calculate_ce_out(v.params.basecase)

#### 04.4 Conduct cost-effectiveness analysis ####
# m.cea <- calculate_icers(m.ce)

#### 04.5 Plot cost-effectiveness frontier ####
# l.cea.frontier <- getFrontier(m.ce)
# gg.cea.frontier <- plot(l.cea.frontier)