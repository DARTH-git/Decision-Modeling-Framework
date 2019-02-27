################################################################################ 
# This script conducts the value of infomration (VOI) analysis of the          #
# cost-effectiveness analysis (CEA) of a hypothetical treatment for the        #  
# simulated cohort of the Sick-Sicker state-transition model (STM)             #
# (PSA) dataset                                                                #
#                                                                              # 
# Depends on:                                                                  #
#   06_value-of-information_functions.R                                        #
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

#### 06.1 Load packages, functions and data ####
#### 06.1.1 Load packages ####
library(dampack)   # decision-analytic modeling visualization tool

#### 06.1.2 Load functions ####
#### 06.1.3 Load PSA dataset ####
load(file = "data/05b_psa-dataset.RData")

#### 06.2 Define VOI inputs ####
### Vector with willingness-to-pay (WTP) thresholds
v.wtp <- seq(0, 200000, by = 10000)

#### 06.3 Expected value of perfect information (EVPI) ####
evpi <- calc_evpi(wtp = v.wtp, psa = l.psa)
plot(evpi, effect_units = "QALY")
ggsave("figs/06_evpi.png", width = 8, height = 6)

#### 06.4 Expected value of partial perfect information (EVPPI) ####

#### 06.5 Expected value of sample information (EVSI) ####
