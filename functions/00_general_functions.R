#---------------------------------------------------------------#
#### Function to compute one-way sensitivity analysis (OWSA) ####
#---------------------------------------------------------------#
#' One-way sensitivity analysis (OWSA)
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#' @param parms Vector with strings with the name of the parameters of interest
#' @param ranges A named list of the form c("parm" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param params.basecase Vector with parameters for the base case
#' @param FUN Function that takes \code{params.basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use 
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return A dataframe with the results of the sensitivity analysis. Can be 
#' visualized with \code{plot.owsa}, and \code{owsa_tornado}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of teh columns must be outcomes.
#'
owsa_det <- function(parms, ranges, nsamps = 100, params.basecase, FUN, outcome, 
                     strategies = NULL, ...){
  if(sum(parms %in% names(params.basecase)) != length(parms)){
    stop("parms should be in names of params.basecase")
  }
  
  if(typeof(ranges)!="list"){
    stop("ranges should be a list")
  }
  
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
    # Run model and capture outcome
    for (j in 1:nrow(m.owsa.input)){ # j <- 1
      m.out.owsa[j, ] <- FUN(m.owsa.input[j, ], ...)[[outcome]]
    }
    
    df.owsa <- data.frame(parameter = parms[i],
                          m.owsa.input[, parms[i]],
                          m.out.owsa)
    names(df.owsa)[-1] <- c("param_val", strategies)
    
    df.owsa.all <- rbind(df.owsa.all, df.owsa)
  }
  
  df.owsa.lng <- reshape2::melt(df.owsa.all, 
                                id.vars = c("parameter", "param_val"), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")
  
  class(df.owsa.lng) <- c("owsa", "data.frame")
  
  return(df.owsa.lng)
}

#---------------------------------------------------------------#
#### Function to compute two-way sensitivity analysis (TWSA) ####
#---------------------------------------------------------------#
#' Two-way sensitivity analysis (TWSA)
#'
#' This function runs a deterministic two-way sensitivity analysis (TWSA) on a
#' given function that produces outcomes.
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest
#' @param ranges A named list of the form list("parm1" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param params.basecase Vector with parameters for the base case
#' @param FUN Function that takes \code{params.basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use 
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return A dataframe with the results of the sensitivity analysis. Can be 
#' visualized with \code{plot.owsa}, and \code{owsa_tornado}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of teh columns must be outcomes.
#'
twsa_det <- function(parm1, parm2, ranges, nsamps = 40, params.basecase, FUN, outcome, 
                     strategies = NULL, ...){
  
  if(sum(c(parm1, parm2) %in% names(params.basecase)) != 2){
    stop("parm1 and parm2 should be in names of params.basecase")
  }
  
  if(typeof(ranges)!="list"){
    stop("ranges should be a list")
  }
  
  if(length(ranges)!=2){
    stop("The number of elements in ranges has to be two")
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
  
  # Generate matrix of inputs
  df.twsa.params <- expand.grid(placeholder_name1 = seq(ranges[[1]][1], 
                                                        ranges[[1]][2], 
                                                        length.out = nsamps), 
                                placeholder_name2 = seq(ranges[[2]][1], 
                                                        ranges[[2]][2], 
                                                        length.out = nsamps))
  
  m.twsa.input <- cbind(df.twsa.params, 
                        v.params.basecase[-which(names(v.params.basecase) %in% c(parm1, parm2))])
  
  names(m.twsa.input)[names(m.twsa.input) == "placeholder_name1"] <- parm1
  names(m.twsa.input)[names(m.twsa.input) == "placeholder_name2"] <- parm2
  # Initialize matrix to store outcomes from a OWSA of the CEA
  m.out.twsa <- matrix(0, 
                       nrow = nrow(m.twsa.input), 
                       ncol = n.str)
  
  # Run model and capture outcome
  for (i in 1:nrow(m.twsa.input)){ # i <- 1
    m.out.twsa[i, ] <- FUN(m.twsa.input[i, ], ...)[[outcome]]
    # Display simulation progress
    if(i/(nrow(m.twsa.input)/10) == round(i/(nrow(m.twsa.input)/10),0)) {
      cat('\r', paste(i/nrow(m.twsa.input) * 100, "% done", sep = " "))
    }
  }
  
  df.twsa <- data.frame(m.twsa.input[, 1:2],
                        m.out.twsa)
  names(df.twsa)[-c(1:2)] <- strategies
  
  
  df.twsa.lng <- reshape2::melt(df.twsa, id.vars = c(parm1, parm2), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")
  
  class(df.twsa.lng) <- c("twsa", "data.frame")
  
  return(df.twsa.lng)
}

