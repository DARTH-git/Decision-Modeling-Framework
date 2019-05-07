#---------------------------------------------#
#### Calculate cost-effectiveness outcomes ####
#---------------------------------------------#
f.calculate_ce_out <- function(l.params.all, n.wtp = 100000){ # User defined
  ### Definition:
  ##   Calculates costs and effects for a given vector of parameters using a 
  ##   simulation model.
  ### Arguments:  
  ##   l.params.all: List with all parameters of decision model
  ##   n.wtp: Willingness-to-pay threshold to compute net benefits
  ### Returns:
  ##   df.ce: Dataframe with discounted costs, effectiveness and NMB
  ##
  with(as.list(l.params.all), {
    ## Create discounting vectors
    v.dwc <- 1 / ((1 + d.e) ^ (0:(n.t))) # vector with discount weights for costs
    v.dwe <- 1 / ((1 + d.c) ^ (0:(n.t))) # vector with discount weights for QALYs
    
    ## Run STM model at a parameter set for each intervention
    l.model.out.no_trt <- f.decision_model(l.params.all = l.params.all)
    l.model.out.trt    <- f.decision_model(l.params.all = l.params.all)
    
    ## Cohort trace by treatment
    m.M_no_trt <- l.model.out.no_trt$m.M # No treatment
    m.M_trt    <- l.model.out.trt$m.M    # Treatment
    
    ## Vectors with costs and utilities by treatment
    v.u_no_trt <- c(u.H, u.S1, u.S2, u.D)
    v.u_trt    <- c(u.H, u.Trt, u.S2, u.D)
    
    v.c_no_trt <- c(c.H, c.S1, c.S2, c.D)
    v.c_trt    <- c(c.H, c.S1 + c.Trt, c.S2 + c.Trt, c.D)
    
    ## Mean Costs and QALYs for Treatment and NO Treatment
    v.tu_no_trt <- m.M_no_trt %*% v.u_no_trt
    v.tu_trt    <- m.M_trt %*% v.u_trt
    
    v.tc_no_trt <- m.M_no_trt %*% v.c_no_trt
    v.tc_trt    <- m.M_trt %*% v.c_trt
    
    ## Total discounted mean Costs and QALYs
    tu.d_no_trt <- t(v.tu_no_trt) %*% v.dwe 
    tu.d_trt    <- t(v.tu_trt) %*% v.dwe
    
    tc.d_no_trt <- t(v.tc_no_trt) %*% v.dwc
    tc.d_trt    <- t(v.tc_trt)    %*% v.dwc
    
    ## Vector with total discounted mean Costs and QALYs
    v.tc.d <- c(tc.d_no_trt, tc.d_trt)
    v.tu.d <- c(tu.d_no_trt, tu.d_trt)
    
    ## Vector with discounted net monetary benefits (NMB)
    v.nmb.d <- v.tu.d * n.wtp - v.tc.d
    
    ## Dataframe with discounted costs, effectiveness and NMB
    df.ce <- data.frame(Strategy = v.names.str,
                        Cost     = v.tc.d,
                        Effect   = v.tu.d,
                        NMB      = v.nmb.d)
    
    return(df.ce)
  }
  )
}