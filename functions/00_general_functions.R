#---------------------------------------------------------------#
#### Function to compute one-way sensitivity analysis (OWSA) ####
#---------------------------------------------------------------#
#' Number of ticks for \code{ggplot2} plots
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#' @param parms Vector with strings with the name of the parameters of interest
#' @param FUN Function that produces outcomes of interest
#' @param params.basecase Vector with parameters for the base case
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param ranges A named list of the form c("parm" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of paramter values. If NULL, 100 parameter values are 
#' used.
#' @keywords owsa
#' @section Details:
#' Based on function \code{pretty}.
#'
owsa_det <- function(parms,
                     FUN, 
                     params.basecase,
                     ranges,
                     nsamps = 100, # vector or not
                     outcome, ...){
  # Generate matrix of inputs for decision tree
  m.owsa.input <- cbind(placeholder_name = seq(n.min, n.max, length.out = n.length.out), 
                        v.params.basecase[-which(names(v.params.basecase) == param)])
  names(m.owsa.input)[names(m.owsa.input) == "placeholder_name"] <- param
  
  # Initialize matrix to store outcomes from a OWSA of the CEA
  m.out.owsa <- matrix(0, 
                       nrow = nrow(m.owsa.input), 
                       ncol = n.str)
  # Run model and capture LE
  for (i in 1:nrow(m.owsa.input)){ # i <- 1
    m.out.owsa[i, ] <- FUN(m.owsa.input[i, ], ...)[[outcome]]
  }
  
  df.owsa <- data.frame(parameter = param,
                        m.owsa.input[, param],
                        m.out.owsa)
  
  
  names(df.owsa)[-1] <- c("param_val", v.names.str)
  
  df.owsa.lng <- melt(df.owsa, id.vars = c("parameter", "param_val"), 
                      variable.name = "strategy", 
                      value.name = "outcome_val")
  
  class(df.owsa.lng) <- c("owsa", "data.frame")
  
  return(df.owsa.lng)
}

owsa_det.v0 <- function(param, # parameter name
                     n.min, # vector with min
                     n.max, #vector with max
                     n.length.out = 100, # vector or not
                     FUN, 
                     v.params.basecase,
                     outcome, ...){
  # Generate matrix of inputs for decision tree
  m.owsa.input <- cbind(placeholder_name = seq(n.min, n.max, length.out = n.length.out), 
                        v.params.basecase[-which(names(v.params.basecase) == param)])
  names(m.owsa.input)[names(m.owsa.input) == "placeholder_name"] <- param
  
  # Initialize matrix to store outcomes from a OWSA of the CEA
  m.out.owsa <- matrix(0, 
                       nrow = nrow(m.owsa.input), 
                       ncol = n.str)
  # Run model and capture LE
  for (i in 1:nrow(m.owsa.input)){ # i <- 1
    m.out.owsa[i, ] <- FUN(m.owsa.input[i, ], ...)[[outcome]]
  }
  
  df.owsa <- data.frame(parameter = param,
                        m.owsa.input[, param],
                        m.out.owsa)
  
  
  names(df.owsa)[-1] <- c("param_val", v.names.str)
  
  df.owsa.lng <- melt(df.owsa, id.vars = c("parameter", "param_val"), 
                      variable.name = "strategy", 
                      value.name = "outcome_val")
  
  class(df.owsa.lng) <- c("owsa", "data.frame")
  
  return(df.owsa.lng)
}
