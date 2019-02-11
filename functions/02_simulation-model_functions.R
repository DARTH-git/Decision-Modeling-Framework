f.decision_model <- function(v.params){# User defined
  ### Arguments:  
  #     v.params: vector of model parameters 
  #
  with(as.list(v.params), {
    #### Age-specific transition probabilities ####
    # Mortality for healthy individuals
    p.HDage  <- 1 - exp(-v.r.asr[(n.age.init + 1) + 0:(n.t - 1)])        
    # Mortality for sick individuals
    p.S1Dage <- 1 - exp(-v.r.asr[(n.age.init + 1) + 0:(n.t - 1)] * hr.S1)
    # Mortality for sicker individuals
    p.S2Dage <- 1 - exp(-v.r.asr[(n.age.init + 1) + 0:(n.t - 1)] * hr.S2)
    
    #### Create age-specific transition matrices ####
    # Initialize array
    a.P <- array(0, dim = c(n.states, n.states, n.t),
                 dimnames = list(v.n, v.n, 0:(n.t - 1)))
    # Fill in array
    # From H
    a.P["H", "H", ]  <- 1 - (p.HS1 + p.HDage)
    a.P["H", "S1", ] <- p.HS1
    a.P["H", "D", ]  <- p.HDage
    # From S1
    a.P["S1", "H", ]  <- p.S1H
    a.P["S1", "S1", ] <- 1 - (p.S1H + p.S1S2 + p.S1Dage)
    a.P["S1", "S2", ] <- p.S1S2
    a.P["S1", "D", ]  <- p.S1Dage
    # From S2
    a.P["S2", "S2", ] <- 1 - p.S2Dage
    a.P["S2", "D", ]  <- p.S2Dage
    # From D
    a.P["D", "D", ] <- 1
    
    #### Compute cohort trace matrix for age-dependent STM ####
    # Initialize cohort trace matrix
    m.M <- matrix(0, 
                  nrow = (n.t + 1), ncol = n.states, 
                  dimnames = list(0:n.t, v.n))
    # Set first row of M with the initial state vector
    m.M[1, ] <- v.s.init
    
    # Iterate STM over time
    for(t in 1:n.t){
      m.M[t + 1, ] <- m.M[t, ] %*% a.P[, , t]
    }
    return(list(m.M = m.M, a.P = a.P))
  }
  )
}
