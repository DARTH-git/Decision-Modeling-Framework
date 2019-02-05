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
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################
# rm(list = ls()) # to clean the workspace

#### 05a.1 Load packages and functions ####
#### 05a.1.1 Load packages ####

#### 05a.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 05a.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/04_calculate-outcomes_functions.R")

#### 05a.2 Cost-effectiveness analysis parameters ####
## Strategy names
v.names.str <- c("No Treatment", "Treatment")  
## Number of strategies
n.str <- length(v.names.str)
## Parameters for basecase CEA
v.params.basecase <- f.generate_basecase_params()

#### 05a.3 Compute cost-effectiveness outcomes ####
df.out.ce <- f.calculate_ce_out(v.params.basecase)

#### 05a.4 Conduct cost-effectiveness analysis ####
# m.cea <- calculate_icers(m.ce)

#### 05a.5 Plot cost-effectiveness frontier ####
# l.cea.frontier <- getFrontier(m.ce)
# gg.cea.frontier <- plot(l.cea.frontier)

#### 05a.6 Deterministic sensitivity analysis (DSA) ####
#### 05a.6.1 One-way sensitivity analysis (OWSA) ####
# create a range of low, basecase and high for the one-way sensitivity parameter
v.u.S2_range <- seq(0.40, 0.70, length.out = 50)
# Generate matrix of inputs for decision tree
m.owsa.input <- cbind(u.S2 = v.u.S2_range, 
                      v.params.basecase[-which(names(v.params.basecase) == "u.S2")])
# Initialize matrix to store outputs from OWSA CEA of decision tree
m.out.owsa.qale <- matrix(0, 
                        nrow = nrow(m.owsa.input), 
                        ncol = n.str)
# Run model and capture LE
for (i in 1:nrow(m.owsa.input)){ # i <- 1
  m.out.owsa.qale[i, ] <- f.calculate_ce_out(m.owsa.input[i, ])$Effect
}

# Plot OWSA 
paramName <- "u.S2"
outcomeName <- "Quality-adjusted life expectancy (QALE)"
## ggplot
owsa.plot.det(param = v.c.Trt_range, 
              outcomes = m.out.owsa.qale, 
              paramName = paramName, 
              strategyNames = v.names.str, 
              outcomeName = outcomeName) +
  scale_y_continuous("Years")
ggsave("figs/05b_OWSA_uS2.png", width = 8, height = 6)

#### 05a.6.2 Two-way sensitivity analysis (TWSA) ####

#### 05a.6.3 Tornado plot ####
v.p.HS1_range <- c(BaseCase = v.params.basecase$p.HS1, Low = 0.10, High = 0.20)
v.c.Trt_range <- c(BaseCase = v.params.basecase$c.Trt, Low = 6000, High = 18000)
v.u.S2_range  <- c(BaseCase = v.params.basecase$u.S2,  Low = 0.40, High = 0.70)

## Parameter names 
v.names.params.tor <- c("p.HS1", "c.Trt", "u.S2")

## List of inputs
l.tor.in <- vector("list", length(v.names.params.tor))
names(l.tor.in) <- v.names.params.tor
l.tor.in$p.HS1 <- cbind(p.HS1 = v.p.HS1_range, v.params.basecase[-which(names(v.params.basecase) == "p.HS1")])
l.tor.in$c.Trt <- cbind(c.Trt = v.c.Trt_range, v.params.basecase[-which(names(v.params.basecase) == "c.Trt")])
l.tor.in$u.S2  <- cbind(u.S2 = v.u.S2_range,   v.params.basecase[-which(names(v.params.basecase) == "u.S2")])

## List of outputs
l.tor.out <- vector("list", length(l.tor.in))
names(l.tor.out) <- v.names.params.tor

## Run multiple OWSA, one for each parameter
for (i in 1:length(l.tor.in)){ # i <- 1
  l.tor.out[[i]] <- t(apply(l.tor.in[[i]], 1, 
                            function(x) {f.calculate_ce_out(x)[2, "Effect"]}))
}

## Data structure: ymean	ymin	ymax
m.out.tor.qale <- matrix(unlist(l.tor.out), 
                       nrow = length(l.tor.in), 
                       ncol = 3, 
                       byrow = TRUE,
                       dimnames = list(v.names.params.tor, c("basecase", "low", "high"	)))
m.out.tor.qale

#### 08.4.1 Plot Tornado ####
TornadoPlot(Parms = v.names.params.tor, 
            Outcomes = m.out.tor.qale, 
            titleName = "Tornado Plot", 
            outcomeName = "Quality-Adjusted Life Expectancy",
            ylab = "Years") 
# ggsave("figs/Markov-SickSicker-Tornado.png", width = 8, height = 6)
# Different values for c.Trt do not have an effect on the QALE