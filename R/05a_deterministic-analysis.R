################################################################################ 
# This script runs the cost-effectiveness analysis of a hypothetical treatment #
# for the simulated cohort of the Sick-Sicker state-transition model (STM)     #
#                                                                              # 
# Depends on:                                                                  #
#   00_general_functions.R                                                     #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   05a_deterministic-analysis_functions.R                                     #
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

#### 05a.1 Load packages and functions ####
#### 05a.1.1 Load packages ####
library(reshape2)
library(dampack)

#### 05a.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 05a.1.3 Load functions ####
source("functions/00_general_functions.R")
source("functions/02_simulation-model_functions.R")
source("functions/05a_deterministic-analysis_functions.R")

#### 05a.1.4 Load calibrated parameters ####
load("data/03_imis-output.RData")

#### 05a.2 Cost-effectiveness analysis parameters ####
### Strategy names
v.names.str <- c("No Treatment", "Treatment")  
### Number of strategies
n.str <- length(v.names.str)
### Parameters for base-case CEA
## Update base-case parameters with calibrated values at MAP
l.params.basecase <- f.update_param_list(l.params.all, v.calib.post.map) 

#### 05a.3 Compute cost-effectiveness outcomes ####
df.out.ce <- f.calculate_ce_out(l.params.all = l.params.basecase, 
                                n.wtp = 150000)
df.out.ce

#### 05a.4 Conduct CEA with deterministic output ####
### Calculate incremental cost-effectiveness ratios (ICERs)
df.cea.det <- calculate_icers(cost = df.out.ce$Cost, 
                              effect = df.out.ce$Effect, 
                              strategies = v.names.str)
df.cea.det
### Save CEA table with ICERs
save(df.cea.det, 
     file = "tables/05a_deterministic-cea-results.RData")

#### 05a.5 Plot cost-effectiveness frontier ####
plot(df.cea.det)
ggsave("figs/05a_cea-frontier.png", width = 8, height = 6)

#### 05a.6 Deterministic sensitivity analysis (DSA) ####
#### 05a.6.1 One-way sensitivity analysis (OWSA) ####
owsa.nmb <- owsa_det(parms = c("c.Trt", "p.HS1", "u.S1", "u.Trt"), # parameter names
                     ranges = list("c.Trt" = c(6000, 13000),
                                   "p.HS1" = c(0.01, 0.50),
                                   "u.S1"  = c(0.75, 0.95),
                                   "u.Trt" = c(0.75, 0.95)),
                     nsamps = 100, # number of values  
                     FUN = f.calculate_ce_out, # Function to compute outputs 
                     params.basecase = l.params.basecase, # List with base-case parameters
                     outcome = "NMB",      # Output to do the OWSA on
                     strategies = v.names.str, # Names of strategies
                     n.wtp = 150000        # Extra argument to pass to FUN
                     )
plot(owsa.nmb, txtsize = 16, n_x_ticks = 5, 
     facet_scales = "free") +
  theme(legend.position = "bottom")
ggsave("figs/05a_owsa-nmb.png", width = 10, height = 6)

#### 05a.6.1 Optimal strategy with OWSA ####
owsa_opt_strat(owsa = owsa.nmb)
ggsave("figs/05a_optimal-owsa-nmb.png", width = 8, height = 6)

#### 05a.6.3 Tornado plot ####
owsa_tornado(owsa = owsa.nmb, strategy = "Treatment")
ggsave("figs/05a_tornado-Treatment-nmb.png", width = 8, height = 6)

#### 05a.6.2 Two-way sensitivity analysis (TWSA) ####
twsa.nmb <- twsa_det(parm1 = "u.S1",  # parameter 1 name
                     parm2 = "u.Trt", # parameter 2 name
                     ranges = list("u.S1"  = c(0.70, 0.80),
                                   "u.Trt" = c(0.90, 1.00)),
                     nsamps = 40, # number of values  
                     FUN = f.calculate_ce_out, # Function to compute outputs 
                     params.basecase = l.params.basecase, # Vector with base-case parameters
                     outcome = "NMB",      # Output to do the OWSA on
                     strategies = v.names.str, # Names of strategies
                     n.wtp = 150000        # Extra argument to pass to FUN
)
plot(twsa.nmb)
ggsave("figs/05a_twsa-uS1-uTrt-nmb.png", width = 8, height = 6)
