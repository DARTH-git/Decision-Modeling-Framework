#---------------------------------------------------------------#
#### Function to compute one-way sensitivity analysis (OWSA) ####
#---------------------------------------------------------------#
#' Number of ticks for \code{ggplot2} plots
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#' @param parms Vector with strings with the name of the parameters of interest
#' @param params.basecase Vector with parameters for the base case
#' @param ranges A named list of the form c("parm" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of paramter values. If NULL, 100 parameter values are 
#' used
#' @param FUN Function that takes \code{params.basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param stratehies vector of strategynames. The default (NULL) will use 
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return A dataframe with the results of the sensitivity analysis. Can be 
#' visualized with \code{plot.owsa}, and \code{owsa_tornado}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of teh columns must be outcomes.
#'
owsa_det <- function(parms, params.basecase, ranges, nsamps = 100, FUN, outcome, 
                     strategies = NULL, ...){
  if(length(parms) != length(ranges)){
    stop("The number of parameters is not the same as the number of ranges")
  }
  
  if(sum(parms==names(ranges)) != length(parms)){
    stop("The name of parameters in parms does not match the name in ranges")
  }
  
  jj <- tryCatch({
    funtest <- FUN(params.basecase, ...)  
  },error = function(e) NA)
  if(is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by 'params.basecase' and ...")
  }
  funtest <- FUN(params.basecase, ...)
  if(is.null(strategies)){
    strategies <- funtest[,1]
    n.str <- length(strategies) 
  }
  if(length(strategies)!=length(funtest[,1])){
    stop("Number of strategies not the same as in FUN")
  }
  v.outcomes <- colnames(funtest)[-1]
  
  if(!(outcome %in% v.outcomes)){
    stop("outcome is not part of FUN outcomes")
  }
  
  df.owsa.all <- NULL
  for (i in 1:length(parms)) { # i <- 2
    # Generate matrix of inputs
    m.owsa.input <- cbind(placeholder_name = seq(ranges[[i]][1], 
                                                 ranges[[i]][2], 
                                                 length.out = nsamps), 
                          params.basecase[-which(names(params.basecase) == parms[i])])
    names(m.owsa.input)[names(m.owsa.input) == "placeholder_name"] <- parms[i]  
    # Initialize matrix to store outcomes from a OWSA of the CEA
    m.out.owsa <- matrix(0, 
                         nrow = nrow(m.owsa.input), 
                         ncol = n.str)
    # Run model and capture LE
    for (j in 1:nrow(m.owsa.input)){ # j <- 1
      m.out.owsa[j, ] <- FUN(m.owsa.input[j, ], ...)[[outcome]]
    }
    
    df.owsa <- data.frame(parameter = parms[i],
                          m.owsa.input[, parms[i]],
                          m.out.owsa)
    names(df.owsa)[-1] <- c("param_val", strategies)
    
    df.owsa.all <- rbind(df.owsa.all, df.owsa)
  }
  
  df.owsa.lng <- melt(df.owsa.all, id.vars = c("parameter", "param_val"), 
                      variable.name = "strategy", 
                      value.name = "outcome_val")
  
  class(df.owsa.lng) <- c("owsa", "data.frame")
  
  return(df.owsa.lng)
}
