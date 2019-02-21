################################################################################ 
# This script conducts the probabilistic sencitivity analysis (PSA) of the     #
# cost-effectiveness analysis of a hypothetical treatment for the simulated    #
# cohort of the Sick-Sicker state-transition model (STM) to create PSA dataset #
#                                                                              # 
# Depends on:                                                                  #
#   01_model-inputs.R                                                          #
#   02_simulation-model_functions.R                                            #
#   05b_probabilistic-analysis_functions.R                                     #
#                                                                              # 
# Author: Fernando Alarid-Escudero                                             # 
# E-mail: fernando.alarid@cide.edu                                             # 
################################################################################
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################
# rm(list = ls()) # to clean the workspace

#### 05b.1 Load packages and functions ####
#### 05b.1.1 Load packages ####
# PSA functionality
library(truncnorm) # truncated normal distribution
library(dampack)   # decision-analytic modeling visualization tool

#### 05b.1.2 Load inputs ####
source("R/01_model-inputs.R")

#### 05b.1.3 Load functions ####
source("functions/02_simulation-model_functions.R")
source("functions/05a_deterministic-analysis_functions.R")
source("functions/05b_probabilistic-analysis_functions.R")

#### 05a.2 Cost-effectiveness analysis parameters ####
## Strategy names
v.names.str <- c("No Treatment", "Treatment")  
## Number of strategies
n.str <- length(v.names.str)

#### 05b.3 Setup probabilistic analysis ####
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

#### 05b.4 Conduct probabilistic sensitivity analysis ####
# Run decision model on each parameter set of PSA input dataset to produce
# PSA outputs for cost and effects
for(i in 1:n.sim){ # i <- 1
  df.out.temp <- f.calculate_ce_out(m.psa.input[i, ])
  m.c[i, ] <- df.out.temp$Cost
  m.e[i, ] <- df.out.temp$Effect
  # Display simulation progress
  if(i/(n.sim/10) == round(i/(n.sim/10),0)) {
    cat('\r', paste(i/n.sim * 100, "% done", sep = " "))
  }
}
### Creae PSA object for dampack
l.psa <- make_psa_obj(cost = m.c, 
                      effectiveness = m.e, 
                      parameters = m.psa.input, 
                      strategies = v.names.str)

#### 05b.5 Save PSA objects ####
save(m.psa.input, m.c, m.e, v.names.str, n.str,
     l.psa,
     file = "data/05b_psa-dataset.RData")

#### 05b.6 Create probabilistic analysis graphs ####
load(file = "data/05b_psa-dataset.RData")

### Vector with willingness-to-pay (WTP) thresholds
v.wtp <- seq(0, 200000, by = 10000)

#### 05b.6.1 Cost-effectiveness scatter plot ####
plot(l.psa)
ggsave("figs/05b_cea-plane-scatter.png", width = 8, height = 6)

#### 05b.6.2 Conduct CEA with probabilistic output ####
### Compute expected costs and effects for each strategy from the PSA
df.out.ce.psa <- summary(l.psa)
### Calculate incremental cost-effectiveness ratios (ICERs)
df.cea.psa <- calculate_icers(cost = df.out.ce.psa$meanCost, 
                              effect = df.out.ce.psa$meanEffect,
                              strategies = df.out.ce.psa$Strategy)
df.cea.psa

#### 05a.6.3 Plot cost-effectiveness frontier ####
plot(df.cea.psa)
ggsave("figs/05b_cea-frontier-psa.png", width = 8, height = 6)

#### 05b.6.4 Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ####
ceac_obj <- ceac(wtp = v.wtp, psa = l.psa)
### Regions of highest probability of cost-effectiveness for each strategy
summary(ceac_obj)
### CEAC & CEAF plot
plot(ceac_obj)
ggsave("figs/05b_ceac-ceaf.png", width = 8, height = 6)

#### 05b.6.3 Expected Loss Curves (ELCs) ####
elc_obj <- calc_exp_loss(wtp = v.wtp, psa = l.psa)
elc_obj
plot(elc_obj)
ggsave("figs/05b_elc.png", width = 8, height = 6)

#### 05b.7 Create linear regression metamodeling sensitivity analysis graphs ####
#### 05a.7.1 One-way sensitivity analysis (OWSA) ####
owsa.lrm.nmb <- owsa(l.psa, parms = c("c.Trt", "p.HS1", "u.S1", "u.Trt"),
                     outcome = "nmb", wtp = 150000)
plot(owsa.lrm.nmb, txtsize = 16, n_x_ticks = 5, 
     facet_scales = "free") +
  theme(legend.position = "bottom")
ggsave("figs/05b_owsa-lrm-nmb.png", width = 10, height = 6)  

#### 05a.7.2 Optimal strategy with OWSA ####
owsa_opt_strat(owsa = owsa.lrm.nmb)
ggsave("figs/05b_optimal-owsa-lrm-nmb.png", width = 8, height = 6)

#### 05a.7.3 Tornado plot ####
owsa_tornado(owsa = owsa.lrm.nmb, strategy = "Treatment")
ggsave("figs/05b_tornado-lrm-Treatment-nmb.png", width = 8, height = 6)

#### 05a.7.4 Two-way sensitivity analysis (TWSA) ####
twsa.lrm.nmb <- twsa(l.psa, parm1 = "u.S1", parm2 = "u.Trt",
           outcome = "nmb", wtp = 150000)
plot(twsa.lrm.nmb)
ggsave("figs/05b_twsa-lrm-uS1-uTrt-nmb.png", width = 8, height = 6)  
