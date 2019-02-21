################################################################################ 
# This script runs all the components of the DARTH fraemwork using the         #
# Sick-Sicker model as testbed. It computes the cost-effectiveness analysis of #
# a hypothetical treatment for the simulated cohort of the Sick-Sicker         #
# state-transition model (STM)                                                 #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################
# rm(list = ls()) # to clean the workspace

#### 00 Install and load packages ####
source("R/app0_packages-setup.R", echo = TRUE)

#### 01 Load inputs ####
source("R/01_model-inputs.R", echo = TRUE)

#### 02 Load simulation model and test it ####
source("R/02_simulation-model.R", echo = TRUE)

#### 03 Calibrate simulation model ####
# source("R/03_calibration.R", echo = TRUE)

#### 04 Validate simulation model ####
# source("R/04_validation.R", echo = TRUE)

#### 05a Conduct deterministic analysis ####
source("R/05a_deterministic-analysis.R", echo = TRUE)

#### 05b Conduct probabilistic analysis ####
source("R/05b_probabilistic-analysis.R", echo = TRUE )

#### 06 Conduct value of information analysis ####
source("R/06_value-of-information.R", echo = TRUE )