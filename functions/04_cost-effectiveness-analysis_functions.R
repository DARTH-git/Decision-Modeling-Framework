#------------------------------------------------------#
#### Generate base-case set of CEA Parameters       ####
#------------------------------------------------------#
f.generate_basecase_params <- function(){
  # Load calibrated parameters
  load("data/03_nm-best-set.RData")
  # Load initial parameters
  v.params.basecase <- f.generate_init_params()
  v.params.basecase["p.S1S2"] <- v.params.calib.best["p.S1S2"]
  v.params.basecase["hr.S1"]  <- v.params.calib.best["hr.S1"]
  v.params.basecase["hr.S2"]  <- v.params.calib.best["hr.S2"]
  return(v.params.basecase)  
}

#---------------------------------------------------#
#### Generate PSA dataset of CEA parameters ####
#---------------------------------------------------#
f.generate_psa_params <- function(seed = 7783){ # User defined
  # load("data/03_calibration_posterior.rData")
  # n.sim <- nrow(m.calib_post_imis)
  set.seed <- seed
  df.psa.params <- data.frame(
    ### Calibrated parameters
    # p.S1S2 = 0.105, # probability to become sicker when sick
    # hr.S1  = 3,     # hazard ratio of death in S1 vs healthy
    # hr.S2  = 10,    # hazard ratio of death in S2 vs healthy 
    m.calib_post_imis,
    
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

#---------------------------------------------------#
#### Calculate cost-effectiveness outcomes ####
#---------------------------------------------------#
f.calculate_ce_out <- function(v.params){ # User defined
  
  with(as.list(v.params), {
    # Run STM model at a parameter set for each intervention
    l.model.out.no_trt <- f.decision_model(v.params)
    l.model.out.trt    <- f.decision_model(v.params)
    
    # Cohort trace by treatment
    m.M_no_trt <- l.model.out.no_trt$m.M # No treatment
    m.M_trt    <- l.model.out.trt$m.M # Treatment
    
    # Vectors with costs and utilities by treatment
    v.u_no_trt <- c(u.H, u.S1, u.S2, u.D)
    v.u_trt    <- c(u.H, u.Trt, u.S2, u.D)
    
    v.c_no_trt <- c(c.H, c.S1, c.S2, c.D)
    v.c_trt    <- c(c.H, c.S1 + c.Trt, c.S2 + c.Trt, c.D)
    
    # Mean Costs and QALYs for Treatment and NO Treatment
    v.tu_no_trt <- m.M_no_trt %*% v.u_no_trt
    v.tu_trt    <- m.M_trt %*% v.u_trt
    
    v.tc_no_trt <- m.M_no_trt %*% v.c_no_trt
    v.tc_trt    <- m.M_trt %*% v.c_trt
    
    # Discounted Mean Costs and QALYs
    tu.d_no_trt <- t(v.tu_no_trt) %*% v.dwe  # 1x31 %*% 31x1 -> 1x1
    tu.d_trt    <- t(v.tu_trt) %*% v.dwe
    
    tc.d_no_trt <- t(v.tc_no_trt) %*% v.dwc
    tc.d_trt    <- t(v.tc_trt)    %*% v.dwc
    
    # Vector with discounted Mean Costs and QALYs
    v.tc.d <- c(tc.d_no_trt, tc.d_trt)
    v.tu.d <- c(tu.d_no_trt, tu.d_trt)
    
    # Matrix with discounted costs and effectiveness
    m.ce <- data.frame(Strategy = v.names.str,
                       Cost     = v.tc.d,
                       Effect   = v.tu.d)
    
    return(m.ce)
  }
  )
  
}
