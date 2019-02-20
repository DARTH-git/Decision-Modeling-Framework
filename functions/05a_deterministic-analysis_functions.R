#------------------------------------------------------#
#### Generate base-case set of CEA Parameters       ####
#------------------------------------------------------#
f.generate_basecase_params <- function(){
  # Load calibrated parameters
  load("data/03_imis-output.RData")
  # Load initial parameters
  v.params.basecase <- f.define_init_params()
  # Replace calibrated parameters with calibrated values at MAP
  v.params.basecase["p.S1S2"] <- v.calib.post.map["p.S1S2"]
  v.params.basecase["hr.S1"]  <- v.calib.post.map["hr.S1"]
  v.params.basecase["hr.S2"]  <- v.calib.post.map["hr.S2"]
  return(v.params.basecase)  
}

#---------------------------------------------#
#### Calculate cost-effectiveness outcomes ####
#---------------------------------------------#
f.calculate_ce_out <- function(v.params, n.wtp = 100000){ # User defined
# Arguments
  # v.params: vector of parameters to run the simluation model on
  # n.wtp: Willingness-to-pay threshold to compute net benefits
# Returns
  # m.ce: a matrix with discounted costs, effectiveness and NMB 
  with(as.list(v.params), {
    # Run STM model at a parameter set for each intervention
    l.model.out.no_trt <- f.decision_model(v.params)
    l.model.out.trt    <- f.decision_model(v.params)
    
    # Cohort trace by treatment
    m.M_no_trt <- l.model.out.no_trt$m.M # No treatment
    m.M_trt    <- l.model.out.trt$m.M # Treatment
    
    # Vectors with costs and utilities by treatment
    v.u_no_trt <- c(u.H, u.S1, u.S2, u.D)
    v.u_trt    <- c(u.H, u.Trt, u.S2, u.D)
    
    v.c_no_trt <- c(c.H, c.S1, c.S2, c.D)
    v.c_trt    <- c(c.H, c.S1 + c.Trt, c.S2 + c.Trt, c.D)
    
    # Mean Costs and QALYs for Treatment and NO Treatment
    v.tu_no_trt <- m.M_no_trt %*% v.u_no_trt
    v.tu_trt    <- m.M_trt %*% v.u_trt
    
    v.tc_no_trt <- m.M_no_trt %*% v.c_no_trt
    v.tc_trt    <- m.M_trt %*% v.c_trt
    
    # Total discounted mean Costs and QALYs
    tu.d_no_trt <- t(v.tu_no_trt) %*% v.dwe  # 1x31 %*% 31x1 -> 1x1
    tu.d_trt    <- t(v.tu_trt) %*% v.dwe
    
    tc.d_no_trt <- t(v.tc_no_trt) %*% v.dwc
    tc.d_trt    <- t(v.tc_trt)    %*% v.dwc
    
    # Vector with total discounted mean Costs and QALYs
    v.tc.d <- c(tc.d_no_trt, tc.d_trt)
    v.tu.d <- c(tu.d_no_trt, tu.d_trt)
    
    # Vector with discounted net monetary beneifts (NMB)
    v.nmb.d <- v.tu.d * n.wtp - v.tc.d
    
    # Matrix with discounted costs, effectiveness and NMB
    m.ce <- data.frame(Strategy = v.names.str,
                       Cost     = v.tc.d,
                       Effect   = v.tu.d,
                       NMB      = v.nmb.d)
    
    return(m.ce)
  }
  )
}

#-----------------------------------------------#
### Function for plotting two-way SA Diagrams ###
#-----------------------------------------------#
twsa.plot.det <- function(params, outcomes, strategyNames, outcomeName = "Outcome", mx = TRUE){
  ## Load required packages
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  parm1 <- colnames(params)[1]
  parm2 <- colnames(params)[2]
  if (mx == T){
    strategy <- factor(max.col(outcomes), labels = strategyNames)
  } else {
    strategy <- factor(max.col(-outcomes), labels = strategyNames)
  }
  
  twsa.df <- data.frame(strategy)
  #A simple trick to define my variables in my functions environment
  twsa.df$parm1 <- params[, parm1]
  twsa.df$parm2 <- params[, parm2]
  
  twsa.gg <- ggplot(twsa.df, aes(x = parm1, y = parm2)) + 
    geom_tile(aes(fill = strategy)) +
    theme_bw(base_size = 14) +
    xlab(parm1) +
    ylab(parm2) +
    ggtitle("Two-way sensitivity analysis", subtitle = outcomeName) +
    scale_fill_discrete("Strategy: ", l = 50) +
    theme(legend.position = "bottom")
  
  return(twsa.gg)
}

#--------------------------------------------#
### Function for plotting Tornado Diagrams ###
#--------------------------------------------#
TornadoPlot <- function(Parms, Outcomes, titleName, outcomeName, ylab = "$"){
  # Parm:        vector with parameter names  
  # Outcomes:    matrix including parameter specific outcomes (number of Parm x 3)
  # titleName:   title of the plot (e.g Tornado Plot)
  # outcomeName: name of the outcome shown in the Tornado plot
  
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Grouped Bar Plot
  # Determine the overall optimal strategy
  paramNames2 <- Parms
  
  # Combine the parameter list with the data
  ymean <- Outcomes[1, 1]
  
  yMin <- Outcomes[, 2] - ymean
  yMax <- Outcomes[, 3] - ymean
  ySize <- abs(yMax - yMin)  # High value - Low value
  
  rankY<- order(ySize)
  nParams <- length(paramNames2)
  
  Tor <- data.frame(
    Parameter = c(paramNames2[rankY], paramNames2[rankY]),  
    Level = c(rep("Low", nParams), rep("High", nParams)),
    value = ymean + c(yMin[rankY], yMax[rankY]),
    sort = seq(1, nParams)
  )
  
  #re-order the levels in the order of appearance in the data.frame
  Tor$Parameter2 <- ordered(Tor$Parameter, Tor$Parameter[1:(length(Tor$Parameter) / 2)])
  # Tor$Parameter2 <- factor(Tor$Parameter, as.character(Tor$Parameter))
  #Define offset as a new axis transformation. Source: http://blog.ggplot2.org/post/25938265813/defining-a-new-transformation-for-ggplot2-scales  
  offset_trans <- function(offset = 0) {
    trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x + offset)
  }
  #Plot the Tornado diagram.
  txtsize <- 12
  tor.gg <- ggplot(Tor[Tor$Level == "Low", ], aes(x = Parameter2, y = value, fill = level)) +
    geom_bar(stat = "identity", fill = "blue") +
    ggtitle("Tornado Plot", subtitle = outcomeName) +
    scale_fill_discrete("Parameter Level: ", l = 50) +
    scale_y_continuous(name = ylab, trans = offset_trans(offset = ymean)) +
    scale_x_discrete(name = "Parameter") +
    geom_bar(data = Tor[Tor$Level == "High", ], aes(x = Parameter2, y = value, fill = level), stat = "identity", fill = "red", alpha = 0.5) +
    geom_hline(yintercept = ymean, linetype = "dotted", size = 0.5) +
    theme_bw(base_size = 14) +
    coord_flip() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = txtsize, angle = 0, hjust = 1),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face = "bold", size = txtsize),
          axis.title.y = element_text(face = "bold", size = txtsize),
          axis.text.y = element_text(size = txtsize),
          axis.text.x = element_text(size = txtsize),
          axis.ticks.y = element_blank())
  
  return(tor.gg)
  # ggsave(paste("results/", titleName,".png"))
}