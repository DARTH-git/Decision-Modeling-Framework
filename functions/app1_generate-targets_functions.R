#---------------------------------------------------#
#### Vectorized function of categorical sampling ####
#---------------------------------------------------#
samplev <- function (probs, m = 1) {
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if (!length(lev)) 
    lev <- 1:k
  ran <- matrix(lev[1], ncol = m, nrow = n)
  U <- t(probs)
  for(i in 2:k) {
    U[i, ] <- U[i, ] + U[i - 1, ]
  }
  if (any((U[k, ] - 1) > 1e-05))
    stop("error in multinom: probabilities do not sum to 1")
  
  for (j in 1:m) {
    un <- rep(runif(n), rep(k, n))
    ran[, j] <- lev[1 + colSums(un > U)]
  }
  ran
}
#------------------------------------------------------------#
#### Function to create the transition probability array  ####
#------------------------------------------------------------#
f.sicksicker_tp <- function(v.params){
  ### Source of Model: https://bmccancer.biomedcentral.com/articles/10.1186/1471-2407-6-136
  ### Depends on:
  ###   - Package `msm` to compute the matrix exponential
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
                 dimnames = list(v.n, v.n, 0:(n.t-1)))
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
    
    ### Check if Transition Probability array is valid
    valid <- apply(a.P, 3, function(x) all.equal(sum(rowSums(x)), n.states))
    if (!isTRUE(all.equal(as.numeric(sum(valid)), as.numeric(n.t)))) {
      stop("This is not a valid transition Matrix")
    }
    
    return(a.P) # Return transition probability array
  }
  )
}
# test it
f.sicksicker_tp(df.params.init) # it works!

#-----------------------------------------------------------------#
#### Function to sample probabilities in a vectorized approach ####
#-----------------------------------------------------------------#
# Function that updates the transition probabilities at every cycle.
f.probs <- function(M_it, m.P, n.i, n.t) { 
  # M_it:    health state occupied by individual i at cycle t (character variable)
  
  m.p.it <- matrix(NA, n.states, n.i)     # create vector of state transition probabilities
  rownames(m.p.it) <- v.n            # assign names to the vector
  
  # update the v.p with the appropriate probabilities   
  m.p.it[, M_it == "H"]  <- m.P["H", ]  # transition probabilities when Normal
  m.p.it[, M_it == "S1"] <- m.P["S1", ] # transition probabilities when Small Adenoma
  m.p.it[, M_it == "S2"] <- m.P["S2", ] # transition probabilities when Large Adenoma
  m.p.it[, M_it == "D"]  <- m.P["D", ]  # transition probabilities when PreClinCRC_Early
  
  ifelse(isTRUE(all.equal(as.numeric(colSums(m.p.it)), as.numeric(rep(1, n.i)))), # colSums(m.p.it) == 1, 
         return(t(m.p.it)), ## IF TRUE
         stop("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
}       
#-----------------------------------------------------#
#### Microsimulation implementation of Sick-Sicker ####
#-----------------------------------------------------#
### Keeps track of what happens to each individual during each cycle. 
f.sicksicker_micsim <- function(v.params, n.i, n.t, v.n, 
                                TR.out = TRUE, TS.out = FALSE, seed = 1,
                                plot.targets = FALSE, verbose = TRUE){
  ### Depends on:
  ###   - Package `epitools` to compute exact binomial CI of model outputs
  #
  ### Arguments:  
  # n.i:     number of individuals
  # n.t:     total number of cycles to run the model
  # v.n:     vector of health state names
  # TR.out:  should the output include a Microsimulation trace? (default is TRUE)
  # TS.out:  should the output include a transition array between states? (default is TRUE)
  # seed:    starting seed number for random number generator (default is 1)
  # plot.targets : Should targets be plotted (default is TRUE)
  # Uses external functions:
  # f.sicksicker_tp: Computes a transition probability array
  # Probs:      Function to return transition probabilities
  with(as.list(v.params), {
    
    ### Age names
    v.ages <- n.age.init:(n.age.init + n.t-1)
    
    ### Set the seed for every individual for the random number generator
    set.seed(seed)                  
    
    ### Generate Initial state vector
    ## All individuals start Healthy
    v.M_1 <- rep("H", n.i)
    
    ### Obtain transition probability array
    a.P <- f.sicksicker_tp(v.params)
    
    ### Initialize matrix capturing the state name for all individuals at each time point 
    m.M.i <- matrix(nrow = n.i, ncol = n.t + 1, 
                  dimnames = list(paste("ind", 1:n.i, sep = " "), 
                                  paste("cycle", 0:n.t, sep = " ")))  
    
    ### Initialize population in correspoding health states
    m.M.i[, 1] <- v.M_1
    
    #### Run model over time ####
    for (t in 1:n.t) { # t = 1
      m.p <- f.probs(m.M.i[, t], a.P[, , t], n.i, n.t)           # calculate the transition probabilities at cycle t 
      
      m.M.i[, t + 1] <- samplev(prob = m.p)  # sample the next health state and store that state in matrix m.M.i 
      if(verbose){
        cat('\r', paste(round(t/n.t * 100), "% done", sep = " "))        # display the progress of the simulation
      }
    }
    if(verbose){
      print("\n")
      print("Computing Targets")
    }
    
    # t0 <- Sys.time()
    #### Compute Trace and Cases matrices ####
    if (TR.out == TRUE) { # create a  matrix of distribution of population across states
      ## Create a trace with N's
      m.M.n <- t(apply(m.M.i, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
      ## Create a trace with proportions
      m.M <- m.M.n / n.i                               
      ## Name the rows 
      rownames(m.M.n) <- rownames(m.M) <- paste("Cycle", 0:n.t, sep = " ")
      ## Name the columns 
      colnames(m.M.n) <- colnames(m.M) <- v.n
    } else {
      TR <- NULL
    } 
    
    if (TS.out == TRUE) {  # create a  matrix of transitions across states
      v.TS <- paste(m.M.i, cbind(m.M.i[, -1], NA), sep = "->") # transitions from one state to the other
      m.TS <- matrix(v.TS, nrow = n.i)
      rownames(m.TS) <- paste("Ind",   1:n.i, sep = " ")   # name the rows 
      colnames(m.TS) <- paste("Cycle", 0:n.t, sep = " ")   # name the columns 
    } else {
      TS <- NULL
    }
    
    #### Individual level outputs ####
    ### Population at risk by age
    v.n.atrisk <- rowSums(m.M.n[, c("H", "S1", "S2")])
    # names(v.n.atrisk) <- c(v.ages, n.age.init + n.t)
    
    ### Number of people in a sick state
    v.n.sick <- rowSums(m.M.n[, c("S1","S2")])
    # names(v.n.sick) <- c(v.ages, n.age.init + n.t)
    
    ### Number of people in Sicker state
    v.n.sick.S2 <- m.M.n[, c("S2")]
    # names(v.n.sick.S2) <- c(v.ages, n.age.init + n.t)
    
    #### Calibration targets ####
    ### SURVIVAL Target
    df.survival <- data.frame(Target = "Survival",
                              Time = 0:n.t,
                              Num = c(v.n.atrisk),
                              Pop = n.i)
    ## Compute SE and 95% CI
    df.survival <- df.survival %>% 
      dplyr::mutate(value = binom.exact(Num, Pop)$prop,
                    se = sqrt(value*(1-value)/Pop),
                    lb = binom.exact(Num, Pop)$lower,
                    ub = binom.exact(Num, Pop)$upper)
    
    ### PREVALENCE of Sick Targets
    df.prevalence <- data.frame(Target = "Prevalence",
                                Time = 0:n.t,
                                Num = v.n.sick,
                                Pop = v.n.atrisk)
    ## Compute SE and 95% CI
    df.prevalence <- df.prevalence %>% 
      dplyr::mutate(value = binom.exact(Num, Pop)$prop,
                    se = sqrt(value*(1-value)/Pop),
                    lb = binom.exact(Num, Pop)$lower,
                    ub = binom.exact(Num, Pop)$upper)
    
    ### Proportion of Sicker Targets
    df.proportion <- data.frame(Target = "Proportion",
                                Time = 1:n.t,
                                Num = v.n.sick.S2[-1],
                                Pop = v.n.sick[-1])
    ## Compute SE and 95% CI
    df.proportion <- df.proportion %>% 
      dplyr::mutate(value = binom.exact(Num, Pop)$prop,
                    se = sqrt(value*(1-value)/Pop),
                    lb = binom.exact(Num, Pop)$lower,
                    ub = binom.exact(Num, Pop)$upper)
    # t1 <- Sys.time()
    # tot.time <- t1-t0
    
    ### Combine targets
    df.targets.all <- rbind(df.survival, df.prevalence, df.proportion)
    
    ### Plot targets
    if(plot.targets){
      print(
        ggplot(df.targets.all, aes(x = Time, y = value, ymin = lb, ymax = ub)) +
          geom_point(shape = 1, size = 2) +
          geom_errorbar(width = 1.5) +
          facet_wrap(~ Target) +
          xlab("Time") +
          ylab("Proportion") +
          theme_bw()
      )
    }
    
    ### Combine Targets
    SickSicker.targets <- list(Surv       = df.survival[df.survival$Time %in% c(10, 20, 30), ],
                               Prev       = df.prevalence[df.prevalence$Time %in% c(10, 20, 30), ],
                               PropSicker = df.proportion[df.proportion$Time %in% c(10, 20, 30), ])
    
    return(SickSicker.targets)
  }
  )
}