#---------------------------------------------------#
#### Generate PSA dataset of CEA parameters ####
#---------------------------------------------------#
generate_psa_params <- function(n_sim, seed = 20190220){ # User defined
  ### Definition:
  ##   Generates PSA input dataset by sampling decision model parameters from
  ##   their distributions
  ### Arguments:  
  ##   n_sim: Number of PSA samples
  ##   seed:  Seed for reproducibility of Monte Carlo sampling
  ### Returns:
  ##   df_psa_params: Dataframe with samples of parameters for PSA
  ##
  
  ## Load calibrated parameters
  load("output/03_imis_output.RData")
  n_sim <- nrow(m_calib_post)
  set_seed <- seed
  dpsa_params <- data.frame(
    ### Calibrated parameters
    m_calib_post,
    
    ### Transition probabilities (per cycle)
    p_HS1   = rbeta(n_sim, 30, 170),        # probability to become sick when healthy
    p_S1H   = rbeta(n_sim, 60, 60) ,        # probability to become healthy when sick
    
    ### State rewards
    ## Costs
    c_H   = rgamma(n_sim, shape = 100, scale = 20)    , # cost of remaining one cycle in state H
    c_S1  = rgamma(n_sim, shape = 177.8, scale = 22.5), # cost of remaining one cycle in state S1
    c_S2  = rgamma(n_sim, shape = 225, scale = 66.7)  , # cost of remaining one cycle in state S2
    c_Trt = rgamma(n_sim, shape = 73.5, scale = 163.3), # cost of treatment (per cycle)
    c_D   = 0                                         , # cost of being in the death state
    ## Utilities
    u_H   = rtruncnorm(n_sim, mean =    1, sd = 0.01, b = 1), # utility when healthy
    u_S1  = rtruncnorm(n_sim, mean = 0.75, sd = 0.02, b = 1), # utility when sick
    u_S2  = rtruncnorm(n_sim, mean = 0.50, sd = 0.03, b = 1), # utility when sicker
    u_D   = 0                                               , # utility when dead
    u_Trt = rtruncnorm(n_sim, mean = 0.95, sd = 0.02, b = 1)  # utility when being treated
  )
  return(dpsa_params)
}
