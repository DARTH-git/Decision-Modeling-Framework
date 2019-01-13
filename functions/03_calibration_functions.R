#-------------------------------------------------------------------#
#### Generate model outputs for calibration from a parameter set ####
#-------------------------------------------------------------------#
f.calibration_out <- function(v.params.calib){
  ### Arguments:  
  #     v.params.calib: vector of parameters that need to be calibrated
  #
  # Create temporary variable with base-case model parameters
  v.params <- v.params.init
  # Substitute values of calibrated parameters in base-case with 
  # calibrated values
  v.params["p.S1S2"] <- v.params.calib["p.S1S2"]
  v.params["hr.S1"]  <- v.params.calib["hr.S1"]
  v.params["hr.S2"]  <- v.params.calib["hr.S2"]
  
  # Run model with updated calibrated parameters
  l.out.stm <- sicksicker_stm(v.params = v.params, n.t = n.t)
  
  ####### Epidemiological Output ###########################################
  #### Overall Survival (OS) ####
  v.os <- 1 - l.out.stm$m.M[, "D"]
  
  #### Disease prevalence #####
  v.prev <- rowSums(l.out.stm$m.M[, c("S1", "S2")])/v.os
  
  #### Proportion of sick in S1 state #####
  v.prop.S1 <- l.out.stm$m.M[, "S1"] / v.prev
  
  ####### Return Output ###########################################
  l.out <- list(Surv = v.os[2:31],
              Prev = v.prev[2:31],
              PropSick = v.prop.S1[c(11, 21, 31)])
  return(l.out)
}

#-------------------------------------------------------------------#
#### Goodness of fit function for calibration from a parameter set ####
#-------------------------------------------------------------------#
f.gof = function(v.params){
  ###   Run model for parametr set "v.params" ###
  l.model.res <- f.calibration_out(v.params)
  
  ###  Calculate goodness-of-fit of model outputs to targets  ###
  v.GOF <- numeric(n.target)
  # TARGET 1: Survival ("Surv")
  # log likelihood  
  v.GOF[1] <- sum(dnorm(x = SickSicker.targets$Surv$value,
                        mean = l.model.res$Surv,
                        sd = SickSicker.targets$Surv$se,
                        log = T))
  
  # TARGET 2: Prevalence ("Prev")
  # log likelihood
  v.GOF[2] <- sum(dnorm(x = SickSicker.targets$Prev$value,
                        mean = l.model.res$Prev,
                        sd = SickSicker.targets$Prev$se,
                        log = T))
  
  # TARGET 3: Proprotion Sick+Sicker who are Sick
  # log likelihood
  v.GOF[3] <- sum(dnorm(x = SickSicker.targets$PropSick$value,
                        mean = l.model.res$PropSick,
                        sd = SickSicker.targets$PropSick$se,
                        log = T))
  
  # OVERALL
  # can give different targets different weights
  v.weights <- rep(1, n.target)
  # weighted sum
  GOF.overall <- sum(v.GOF[1:n.target] * v.weights)
  
  # return GOF
  return(GOF.overall)
}