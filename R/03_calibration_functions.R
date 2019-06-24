#-------------------------------------------------------------------#
#### Generate model outputs for calibration from a parameter set ####
#-------------------------------------------------------------------#
calibration_out <- function(v_params_calib, l_params_all){ # User defined
  ### Definition:
  ##   Computes model outputs to be used for calibration routines
  ### Arguments:  
  ##   v_params_calib: vector of parameters that need to be calibrated
  ##   l_params_all: List with all parameters of the decision model
  ### Returns:
  ##   l_out: List with Survival (Surv), Prevalence of Sick and Sicker (Prev), 
  ##          and proportion of Sicker (PropSicker) out of all sick 
  ##          (Sick+Sicker) individuals
  ##
  # Substitute values of calibrated parameters in base-case with 
  # calibrated values
  l_params_all <- update_param_list(l_params_all = l_params_all, params_updated = v_params_calib)
  
  # Run model with updated calibrated parameters
  l_out_stm <- decision_model(l_params_all = l_params_all)
  
  ####### Epidemiological Output ###########################################
  #### Overall Survival (OS) ####
  v_os <- 1 - l_out_stm$m_M[, "D"]
  
  #### Disease prevalence #####
  v_prev <- rowSums(l_out_stm$m_M[, c("S1", "S2")])/v_os
  
  #### Proportion of sick in S1 state #####
  v_prop_S2 <- l_out_stm$m_M[, "S2"] / rowSums(l_out_stm$m_M[, c("S1", "S2")])
  
  ####### Return Output ###########################################
  l_out <- list(Surv = v_os[c(11, 21, 31)],
              Prev = v_prev[c(11, 21, 31)],
              PropSicker = v_prop_S2[c(11, 21, 31)])
  return(l_out)
}

#-------------------------------------------------------------------#
#### Likelihood and log-likelihood functions for a parameter set ####
#-------------------------------------------------------------------#
log_lik <- function(v_params){ # User defined
  ### Definition:
  ##  Computes a log-likelihood value for one (or multiple) parameter set(s)
  ##  using the simulation model and likelihood functions
  ### Arguments:  
  ##   v_params: Vector (or matrix) of model parameters 
  ### Returns:
  ##   v_llik_overall: Scalar (or vector) with log-likelihood values
  ##
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  
  n_samp <- nrow(v_params)
  v_llik <- matrix(0, nrow = n_samp, ncol = n_target) 
  colnames(v_llik) <- c("Surv", "Prev", "PropSick")
  v_llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( { 
    ###   Run model for parametr set "v_params" ###
    l_model_res <- calibration_out(v_params_calib = v_params[j, ], 
                                     l_params_all = l_params_all)
  
    ###  Calculate log-likelihood of model outputs to targets  ###
    ## TARGET 1: Survival ("Surv")
    ## Normal log-likelihood  
    v_llik[j, "Surv"] <- sum(dnorm(x = SickSicker.targets$Surv$value,
                                   mean = l.model.res$Surv,
                                   sd = SickSicker.targets$Surv$se,
                                   log = T))
    
    ## TARGET 2: Prevalence ("Prev")
    ## Normal log-likelihood
    v_llik[j, "Prev"] <- sum(dnorm(x = SickSicker.targets$Prev$value,
                                   mean = l_model_res$Prev,
                                   sd = SickSicker.targets$Prev$se,
                                   log = T))
    
    ## TARGET 3: Proportion Sick+Sicker who are Sick ("PropSick")
    ## Normal log-likelihood
    v_llik[j, "PropSick"] <- sum(dnorm(x = SickSicker.targets$PropSick$value,
                                       mean = l_model_res$PropSick,
                                       sd = SickSicker.targets$PropSick$se,
                                       log = T))
    
    ## OVERALL
    ## can give different targets different weights (user must change this)
    v_weights <- rep(1, n_target)
    ## weighted sum
    v_llik_overall[j] <- v_llik[j, ] %*% v_weights
    }, error = function(e) NA) 
    if(is.na(jj)) { v_llik_overall <- -Inf }
  } ## End loop over sampled parameter sets
  
  ## return GOF
  return(v_llik_overall)
}
# test if it works
# log_lik(v_params = sample.prior(n_samp = 2))

likelihood <- function(v_params){ 
  ### Definition:
  ##  Computes a likelihood value for one (or multiple) parameter set(s)
  ### Arguments:  
  ##   v_params: Vector (or matrix) of model parameters 
  ### Returns:
  ##   v_like: Scalar (or vector) with likelihood values
  ##
  v_like <- exp(log_lik(v_params)) 
  
  return(v_like)
}
# test if it works
# likelihood(v_params = sample.prior(2))

#----------------------------------------------------------------------------#
#### Function to sample from prior distributions of calibrated parameters ####
#----------------------------------------------------------------------------#
sample.prior <- function(n_samp){
  ### Definition:
  ##  Generates a sample of parameter sets from their prior distribution
  ### Arguments:  
  ##   n_samp: Number of samples
  ### Returns:
  ##   m_param_samp: Matrix with a sample of parameter sets
  ##
  m_lhs_unit   <- randomLHS(n = n_samp, k = n_param)
  m_param_samp <- matrix(nrow = n_samp, ncol = n_param)
  colnames(m_param_samp) <- v_param_names
  for (i in 1:n_param){
    m_param_samp[, i] <- qunif(m_lhs_unit[,i],
                               min = v_lb[i],
                               max = v_ub[i])
    # ALTERNATIVE prior using beta (or other) distributions
    # m_param_samp[, i] <- qbeta(m_lhs_unit[,i],
    #                            min = 1,
    #                            max = 1)
  }
  return(m_param_samp)
}
# test if it works
# pairs.panels(sample.prior(1000))

#--------------------------------------------------------------------------#
#### Functions to evaluate log-prior and prior of calibrated parameters ####
#--------------------------------------------------------------------------#
log_prior <- function(v_params){
  ### Definition:
  ##  Computes a log-prior value for one (or multiple) parameter set(s) based on
  ##  their prior distributions
  ### Arguments:  
  ##   v_params: Vector (or matrix) of model parameters 
  ### Returns:
  ##   lprior: Scalar (or vector) with log-prior values
  ##
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_samp <- nrow(v_params)
  colnames(v_params) <- v_param_names
  lprior <- rep(0, n_samp)
  for (i in 1:n_param){
    lprior <- lprior + dunif(v_params[, i],
                             min = v_lb[i],
                             max = v_ub[i], 
                             log = T)
    # ALTERNATIVE prior using beta distributions
    # lprior <- lprior + dbeta(v_params[, i],
    #                          min = 1,
    #                          max = 1, 
    #                          log = T)
  }
  return(lprior)
}
# test if it works
# log_prior(v_params = sample.prior(5))

prior <- function(v_params) { 
  ### Definition:
  ##  Computes a prior value for one (or multiple) parameter set(s)
  ### Arguments:  
  ##   v_params: Vector (or matrix) of model parameters 
  ### Returns:
  ##   v_prior: Scalar (or vector) with prior values
  ##
  v_prior <- exp(log_prior(v_params)) 
  
  return(v_prior)
}
# test if it works
# prior(v_params = sample.prior(5))

#----------------------------------------------------------------------------------#
#### Functions to evaluate log-posterior and posterior of calibrated parameters ####
#----------------------------------------------------------------------------------#
log_post <- function(v_params) { 
  ### Definition:
  ##  Computes a log-posterior value for one (or multiple) parameter set(s) based on
  ##  the simulation model, likelihood functions and prior distributions
  ### Arguments:  
  ##   v_params: Vector (or matrix) of model parameters 
  ### Returns:
  ##   v_lpost: Scalar (or vector) with log-posterior values
  ##
  v_lpost <- log_prior(v_params) + log_lik(v_params)
 
   return(v_lpost) 
}
# test if it works
# log_post(v_params = sample.prior(5))
