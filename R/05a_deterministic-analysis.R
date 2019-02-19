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
library(dampack)

#### 05a.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 05a.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/05a_deterministic-analysis_functions.R")

#### 05a.2 Cost-effectiveness analysis parameters ####
## Strategy names
v.names.str <- c("No Treatment", "Treatment")  
## Number of strategies
n.str <- length(v.names.str)
## Parameters for basecase CEA
v.params.basecase <- f.generate_basecase_params()

#### 05a.3 Compute cost-effectiveness outcomes ####
df.out.ce <- f.calculate_ce_out(v.params = v.params.basecase, 
                                n.wtp = 150000)
df.out.ce

#### 05a.4 Conduct cost-effectiveness analysis ####
m.cea <- calculate_icers(cost = df.out.ce$Cost, 
                         effect = df.out.ce$Effect, 
                         strategies = v.names.str)

#### 05a.5 Plot cost-effectiveness frontier ####
# l.cea.frontier <- getFrontier(m.ce)
# gg.cea.frontier <- plot(l.cea.frontier)

#### 05a.6 Deterministic sensitivity analysis (DSA) ####
#### 05a.6.1 One-way sensitivity analysis (OWSA) ####
# create a range of low, basecase and high for the one-way sensitivity parameter
v.c.Trt_range <- seq(6000, 13000, length.out = 50)
# Generate matrix of inputs for decision tree
m.owsa.input <- cbind(c.Trt = v.c.Trt_range, 
                      v.params.basecase[-which(names(v.params.basecase) == "c.Trt")])
# Initialize matrix to store outputs from a OWSA of the CEA
m.out.owsa <- matrix(0, 
                     nrow = nrow(m.owsa.input), 
                     ncol = n.str)
# Run model and capture LE
for (i in 1:nrow(m.owsa.input)){ # i <- 1
  m.out.owsa[i, ] <- f.calculate_ce_out(m.owsa.input[i, ])$NMB
}

# Plot OWSA 
paramName <- "c.Trt ($)"
outcomeName <- "Net Monetary Benefit (NMB)"
## ggplot
owsa.plot.det(param = v.c.Trt_range, #v.u.S2_range, 
              outcomes = m.out.owsa, 
              paramName = paramName, 
              strategyNames = v.names.str, 
              outcomeName = outcomeName) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous("Thousand $", labels = function(x) comma(x/1000))
ggsave("figs/05b_owsa-cTrt.png", width = 8, height = 6)

#### 05a.6.2 Two-way sensitivity analysis (TWSA) ####
## Generate full factorial combinations between two different parameters
v.p.HS1_range <- seq(0.01, 0.40, length.out = 50)
v.c.Trt_range <- seq(6000, 13000, length.out = 50)
df.twsa.params <- expand.grid(p.HS1 = v.p.HS1_range, 
                              c.Trt = v.c.Trt_range)
## Generate matrix of inputs for decision tree
m.twsa.input <- cbind(df.twsa.params, 
                      v.params.basecase[-which(names(v.params.basecase) %in% c("c.Trt", "p.HS1"))])
                      
## Initialize matrix to store outputs from CEA of decision tree
m.out.twsa <- matrix(0, 
                     nrow = nrow(m.twsa.input), 
                     ncol = n.str)
## Run model and capture NMB
for (i in 1:nrow(m.twsa.input)){ # i <- 1
  m.out.twsa[i, ] <- f.calculate_ce_out(m.twsa.input[i, ])$NMB
}

# Plot TWSA
outcomeName <- "Net Monetary Benefit"
twsa.plot.det(params = df.twsa.params, 
              outcomes = m.out.twsa, 
              strategyNames = v.names.str, 
              outcomeName = outcomeName) +
  scale_y_continuous(labels = dollar)
ggsave("figs/05b_twsa-cTrt-pHS1.png", width = 8, height = 6)

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
                            function(x) {f.calculate_ce_out(x)[2, "NMB"]}))
}

## Data structure: ymean	ymin	ymax
m.out.tor <- matrix(unlist(l.tor.out), 
                    nrow = length(l.tor.in), 
                    ncol = 3, 
                    byrow = TRUE,
                    dimnames = list(v.names.params.tor, c("basecase", "low", "high"	)))
m.out.tor

# Plot Tornado
TornadoPlot(Parms = v.names.params.tor, 
            Outcomes = m.out.tor.qale, 
            titleName = "Tornado Plot", 
            outcomeName = "Net Monetary Benefit",
            ylab = "Thousand $") 
ggsave("figs/05b_tornado.png", width = 8, height = 6)