################################################################################ 
# This script generates all the required input parameters for the cohort       #
# implementation of the Sick-Sicker state-transition model (STM)               #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs-external_functions.R                                       #
#                                                                              # 
# Authors: Fernando Alarid-Escudero                                            # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################ 

# rm(list = ls()) # to clean the workspace

#### 01.1 Load packages and functions ####
#### 01.1.1 Load packages and functions ####
library(dplyr) # For data manipulation

#### 01.1.2 Load functions ####
source("functions/01_model-inputs_functions.R")

#### 01.2 External parameters ####
#### 01.2.1 General setup ####
n.age.init  <- 25  # age of starting cohort
n.t         <- 75  # time horizon, number of cycles
v.age.names <- n.age.init:(n.age.init + n.t - 1) # vector with age names
v.n <- c("H", "S1", "S2", "D") # vector with the 4 health states of the model:
                               # Healthy (H), Sick (S1), Sicker (S2), Dead (D)
n.states <- length(v.n) # number of health states 
d.c <- 0.03 # discount rate for costs 
d.e <- 0.03 # discount rate for QALYs
v.dwc <- 1 / ((1 + d.e) ^ (0:(n.t))) # vector with discount weights for costs
v.dwe <- 1 / ((1 + d.c) ^ (0:(n.t))) # vector with discount weights for QALYs
v.s.init <- c(H = 1, S1 = 0, S2 = 0, D = 0) # initial state vector

#### 01.2.2 All-cause age-, sex- and race- (ASR) specific mortality ####
df.r.asr <- read.csv("data/01_all-cause-mortality-USA-2015.csv")
v.r.asr  <- df.r.asr %>%
  dplyr::select(Total) %>%
  as.matrix()              # vector with mortality rates

#### 01.2.3 Generate initial set of base-case external parameters ####
v.params.init <- f.generate_init_params()
## Create name of parameters
v.names.params <- names(v.params.init)
## Save base-case set of parameters
write.csv(x = v.params.init, file = "data/01_init-params.csv")

#### 01.3 Estimated parameters ####
