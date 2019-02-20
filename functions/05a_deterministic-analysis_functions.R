#------------------------------------------------------#
#### Generate base-case set of CEA Parameters       ####
#------------------------------------------------------#
f.generate_basecase_params <- function(){
  # Load calibrated parameters
  load("data/03_imis-output.RData")
  # Load initial parameters
  v.params.basecase <- f.define_init_params()
  # Replace calibrated parameters with calibrated values at MAP
  v.params.basecase["p.S1S2"] <- v.calib.post.map["p.S1S2"]
  v.params.basecase["hr.S1"]  <- v.calib.post.map["hr.S1"]
  v.params.basecase["hr.S2"]  <- v.calib.post.map["hr.S2"]
  return(v.params.basecase)  
}

#---------------------------------------------#
#### Calculate cost-effectiveness outcomes ####
#---------------------------------------------#
f.calculate_ce_out <- function(v.params, n.wtp = 100000){ # User defined
  # v.params: vector of parameters to run the simluation model on
  # n.wtp: Willingness-to-pay threshold to compute net benefits
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
    
    # Total discounted mean Costs and QALYs
    tu.d_no_trt <- t(v.tu_no_trt) %*% v.dwe  # 1x31 %*% 31x1 -> 1x1
    tu.d_trt    <- t(v.tu_trt) %*% v.dwe
    
    tc.d_no_trt <- t(v.tc_no_trt) %*% v.dwc
    tc.d_trt    <- t(v.tc_trt)    %*% v.dwc
    
    # Vector with total discounted mean Costs and QALYs
    v.tc.d <- c(tc.d_no_trt, tc.d_trt)
    v.tu.d <- c(tu.d_no_trt, tu.d_trt)
    
    # Vector with discounted net monetary beneifts (NMB)
    v.nmb.d <- v.tu.d * n.wtp - v.tc.d
    
    # Matrix with discounted costs, effectiveness and NMB
    m.ce <- data.frame(Strategy = v.names.str,
                       Cost     = v.tc.d,
                       Effect   = v.tu.d,
                       NMB      = v.nmb.d)
    
    return(m.ce)
  }
  )
}