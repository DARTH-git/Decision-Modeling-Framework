################################################################################ 
# This script runs all the components of the DARTH fraemwork using the         #
# Sick-Sicker model as testbed. It computes the cost-effectiveness analysis of #
# a hypothetical treatment for the simulated cohort of the Sick-Sicker         #
# state-transition model (STM)                                                 #
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

#### 00 Install and load packages ####
### Uncomment if you don't have all required packages installed
# source("R/app0_packages-setup.R", echo = TRUE) 

#### 01 Load inputs ####
source("R/01_model-inputs.R", echo = TRUE)

#### 02 Load simulation model and test it ####
source("R/02_simulation-model.R", echo = TRUE)

#### 03 Calibrate simulation model ####
### Uncomment if you want to rereun the calibration component 
# source("R/03_calibration.R", echo = TRUE)

#### 04 Validate simulation model ####
### Uncomment if you want to rereun the validation component
# source("R/04_validation.R", echo = TRUE)

#### 05a Conduct deterministic analysis ####
source("R/05a_deterministic-analysis.R", echo = TRUE)

#### 05b Conduct probabilistic analysis ####
source("R/05b_probabilistic-analysis.R", echo = TRUE )

#### 06 Conduct value of information analysis ####
source("R/06_value-of-information.R", echo = TRUE )