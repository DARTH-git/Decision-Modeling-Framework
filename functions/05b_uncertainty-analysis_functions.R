#---------------------------------------------------#
#### Generate PSA dataset of CEA parameters ####
#---------------------------------------------------#
f.generate_psa_params <- function(seed = 7783, n.sim){ # User defined
  # Load calibrated parameters
  load("data/03_imis-output.rData")
  n.sim <- nrow(m.calib.post)
  set.seed <- seed
  
  df.psa.params <- data.frame(
    ### Calibrated parameters
    m.calib.post,
    # p.S1S2 = v.calib.post.map["p.S1S2"],# probability to become sicker when sick
    # hr.S1  = v.calib.post.map["hr.S1"], # hazard ratio of death in S1 vs healthy
    # hr.S2  = v.calib.post.map["hr.S2"], # hazard ratio of death in S2 vs healthy
    
    ## Transition probabilities (per cycle)
    p.HS1   = rbeta(n.sim, 30, 170),        # probability to become sick when healthy
    p.S1H   = rbeta(n.sim, 60, 60) ,        # probability to become healthy when sick
    
    ## State rewards
    # Costs
    c.H   = rgamma(n.sim, shape = 100, scale = 20)    , # cost of remaining one cycle in state H
    c.S1  = rgamma(n.sim, shape = 177.8, scale = 22.5), # cost of remaining one cycle in state S1
    c.S2  = rgamma(n.sim, shape = 225, scale = 66.7)  , # cost of remaining one cycle in state S2
    c.Trt = rgamma(n.sim, shape = 73.5, scale = 163.3), # cost of treatment (per cycle)
    c.D   = 0                                         , # cost of being in the death state
    # Utilities
    u.H   = rtruncnorm(n.sim, mean =    1, sd = 0.01, b = 1), # utility when healthy
    u.S1  = rtruncnorm(n.sim, mean = 0.75, sd = 0.02, b = 1), # utility when sick
    u.S2  = rtruncnorm(n.sim, mean = 0.50, sd = 0.03, b = 1), # utility when sicker
    u.D   = 0                                               , # utility when dead
    u.Trt = rtruncnorm(n.sim, mean = 0.95, sd = 0.02, b = 1)  # utility when being treated
  )
  return(df.psa.params)
}
