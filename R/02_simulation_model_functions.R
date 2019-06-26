#-----------------------------------------#
####          Decision Model           ####
#-----------------------------------------#
#' Decision Model
#'
#' \code{decision_model} implements the decision model used.
#'
#' @param l_params_all List with all parameters of decision model
#' @param verbose Logical variable to indicate print out of messages
#' @return The transition probability array and the cohort trace matrix.
#' 
decision_model <- function(l_params_all, verbose = FALSE){ # User defined
  ### Definition:
  ##   Decision model implementation function
  ### Arguments:  
  ##   l_params_all: List with all parameters of decision model
  ##   verbose: Logical variable to indicate print out of messages
  ### Returns:
  ##   a_P: Transition probability array
  ##   m_M: Matrix cohort trace
  ##
  with(as.list(l_params_all), {
    #### Age-specific transition probabilities ####
    # Mortality for healthy individuals
    p_HDage  <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)])        
    # Mortality for sick individuals
    p_S1Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S1)
    # Mortality for sicker individuals
    p_S2Dage <- 1 - exp(-v_r_mort_by_age[(n_age_init + 1) + 0:(n_t - 1)] * hr_S2)
    
    #### Create age-specific transition probability matrices in an array ####
    # Initialize array
    a_P <- array(0, dim = c(n_states, n_states, n_t),
                 dimnames = list(v_n, v_n, 0:(n_t-1)))
    # Fill in array
    # From H
    a_P["H", "H", ]  <- (1-p_HDage) * (1 - p_HS1)
    a_P["H", "S1", ] <- (1-p_HDage) * p_HS1
    a_P["H", "D", ]  <- p_HDage
    # From S1
    a_P["S1", "H", ]  <- (1-p_S1Dage) * p_S1H
    a_P["S1", "S1", ] <- (1-p_S1Dage) * (1 - (p_S1S2 + p_S1H))
    a_P["S1", "S2", ] <- (1-p_S1Dage) * p_S1S2
    a_P["S1", "D", ]  <- p_S1Dage
    # From S2
    a_P["S2", "S2", ] <- 1 - p_S2Dage
    a_P["S2", "D", ]  <- p_S2Dage
    # From D
    a_P["D", "D", ] <- 1
    
    # Check if transition probabilities are valid (i.e., in [0, 1])
    m_indices_notvalid <- arrayInd(which(a_P < 0 | a_P > 1), 
                                   dim(a_P))
    try(
    if(dim(m_indices_notvalid)[1] != 0){
      v_rows_notval   <- rownames(a_P)[m_indices_notvalid[, 1]]
      v_cols_notval   <- colnames(a_P)[m_indices_notvalid[, 2]]
      v_cycles_notval <- dimnames(a_P)[[3]][m_indices_notvalid[, 3]]
      
      df_notvalid <- data.frame(`Transition probabilities not valid:` = 
                                  matrix(paste0(paste(v_rows_notval, v_cols_notval, sep = "->"),
                                                "; at cycle ",
                                                v_cycles_notval), ncol = 1), 
                                check.names = FALSE)
      if(verbose){
        message("Not valid transition probabilities")
        # print(df_notvalid)
        stop(print(df_notvalid), call. = FALSE)
      } #else stop()
    }
    )
    
    # Check if transition probability array is valid
    valid <- apply(a_P, 3, function(x) all.equal(sum(rowSums(x)), n_states))
    if (!isTRUE(all_equal(as.numeric(sum(valid)), as.numeric(n_t)))) {
      if(verbose){
        stop("This is not a valid transition Matrix")
      } #else stop()
    }
    
    #### Compute cohort trace matrix and transition array for age-dependent STM ####
    # Initialize cohort trace matrix
    m_M <- matrix(0, 
                  nrow = (n_t + 1), ncol = n_states, 
                  dimnames = list(0:n_t, v_n))
    # Set first row of m.M with the initial state vector
    m_M[1, ] <- v_s_init
    
    # Iterate STM over time
    for(t in 1:n_t){
      m_M[t + 1, ] <- m_M[t, ] %*% a_P[, , t]
    }
    return(list(a_P = a_P,
                m_M = m_M))
  }
  )
}