f.decision_model <- function(v.params){ # User defined
  ### Definition:
  ##   Decision model implementation function
  ### Arguments:  
  ##   v.params: vector of model parameters 
  ### Returns:
  ##   a.P: Transition probability array
  ##   m.M: Matrix cohort trace
  ##
  with(as.list(v.params), {
    #### Age-specific transition probabilities ####
    # Mortality for healthy individuals
    p.HDage  <- 1 - exp(-v.r.mort_by_age[(n.age.init + 1) + 0:(n.t - 1)])        
    # Mortality for sick individuals
    p.S1Dage <- 1 - exp(-v.r.mort_by_age[(n.age.init + 1) + 0:(n.t - 1)] * hr.S1)
    # Mortality for sicker individuals
    p.S2Dage <- 1 - exp(-v.r.mort_by_age[(n.age.init + 1) + 0:(n.t - 1)] * hr.S2)
    
    #### Create age-specific transition probability matrices in an array ####
    # Initialize array
    a.P <- array(0, dim = c(n.states, n.states, n.t),
                 dimnames = list(v.n, v.n, 0:(n.t-1)))
    # Fill in array
    # From H
    a.P["H", "H", ]  <- (1-p.HDage) * (1 - p.HS1)
    a.P["H", "S1", ] <- (1-p.HDage) * p.HS1
    a.P["H", "D", ]  <- p.HDage
    # From S1
    a.P["S1", "H", ]  <- (1-p.S1Dage) * p.S1H
    a.P["S1", "S1", ] <- (1-p.S1Dage) * (1 - (p.S1S2 + p.S1H))
    a.P["S1", "S2", ] <- (1-p.S1Dage) * p.S1S2
    a.P["S1", "D", ]  <- p.S1Dage
    # From S2
    a.P["S2", "S2", ] <- 1 - p.S2Dage
    a.P["S2", "D", ]  <- p.S2Dage
    # From D
    a.P["D", "D", ] <- 1
    
    # Check if transition probabilities are valid (i.e., in [0, 1])
    m.indices.notvalid <- arrayInd(which(a.P < 0 | a.P > 1), 
                                   dim(a.P))
    try(
    if(dim(m.indices.notvalid)[1] != 0){
      v.rows.notval   <- rownames(a.P)[m.indices.notvalid[, 1]]
      v.cols.notval   <- colnames(a.P)[m.indices.notvalid[, 2]]
      v.cycles.notval <- dimnames(a.P)[[3]][m.indices.notvalid[, 3]]
      
      df.notvalid <- data.frame(`Transition probabilities not valid:` = 
                                  matrix(paste0(paste(v.rows.notval, v.cols.notval, sep = "->"),
                                                "; at cycle ",
                                                v.cycles.notval), ncol = 1), 
                                check.names = FALSE)
      message("Not valid transition probabilities")
      # print(df.notvalid)
      stop(print(df.notvalid), call. = FALSE)
    }
    )
    
    # Check if transition probability array is valid
    valid <- apply(a.P, 3, function(x) all.equal(sum(rowSums(x)), n.states))
    if (!isTRUE(all.equal(as.numeric(sum(valid)), as.numeric(n.t)))) {
      stop("This is not a valid transition Matrix")
    }
    
    #### Compute cohort trace matrix and tranistion array for age-dependent STM ####
    # Initialize cohort trace matrix
    m.M <- matrix(0, 
                  nrow = (n.t + 1), ncol = n.states, 
                  dimnames = list(0:n.t, v.n))
    # Initialize transition array
    a.A <- matrix(0, 
                  nrow = (n.t + 1), ncol = n.states, 
                  dimnames = list(0:n.t, v.n))
    # Set first row of M with the initial state vector
    m.M[1, ] <- v.s.init
    
    # Iterate STM over time
    for(t in 1:n.t){
      m.M[t + 1, ] <- m.M[t, ] %*% a.P[, , t]
    }
    return(list(a.P = a.P,
                m.M = m.M))
  }
  )
}