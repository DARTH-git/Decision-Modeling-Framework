################################################################################ 
# This script conducts the uncertianty analysis of a hypothetical treatment    #
# for the simulated cohort of the Sick-Sicker state-transition model (STM) to  #
# create a probabilistic sensitivity analysis (PSA) dataset                    #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   05b_uncertainty-analysis_functions.R                                       #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################

# rm(list = ls()) # to clean the workspace

#### 05b.1 Load packages and functions ####
#### 05b.1.1 Load packages ####
# PSA functionality
library(truncnorm) # truncated normal distribution

#### 05b.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 05b.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/04_calculate-outcomes_functions.R")
source("functions/05b_uncertainty-analysis_functions.R")
#### 05a.2 Cost-effectiveness analysis parameters ####
## Strategy names
v.names.str <- c("No Treatment", "Treatment")  
## Number of strategies
n.str <- length(v.names.str)

#### 05b.3 Setup uncertainty analysis ####
### Number of simulations
n.sim <- 1000

### Generate PSA input dataset
m.psa.input <- f.generate_psa_params(n.sim = n.sim)

### Initialize matrices for PSA output 
## Matrix of costs
m.c <- matrix(0, 
              nrow = n.sim,
              ncol = n.str)
colnames(m.c) <- v.names.str
## Matrix of effectiveness
m.e <- matrix(0, 
              nrow = n.sim,
              ncol = n.str)
colnames(m.e) <- v.names.str

#### 05b.4 Conduct uncertainty analysis ####
# Run decision model on each parameter set of PSA input dataset to produce
# PSA outputs for cost and effects
for(i in 1:n.sim){ # i <- 1
  df.out.temp <- f.calculate_ce_out(m.psa.input[i, ])
  m.c[i, ] <- df.out.temp$Cost
  m.e[i, ] <- df.out.temp$Effect
  # Display simulation progress
  if(i/(n.sim/10) == round(i/(n.sim/10),0)) { # display progress every 10%
    cat('\r', paste(i/n.sim * 100, "% done", sep = " "))
  }
}

#### 05b.5 Save PSA matrices ####
save(m.psa.input, m.c, m.e, file = "data/05b_psa-dataset.RData")
