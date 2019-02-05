#-----------------------------------------------#
### Function for plotting One-way SA Diagrams ###
#-----------------------------------------------#
owsa.plot.det <- function(param, outcomes,
                          paramName,
                          strategyNames, 
                          outcomeName = "Outcome"){
  ## Load required packages
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  owsa.df <- data.frame(outcomes)
  colnames(owsa.df) <- strategyNames
  owsa.df$param <- param
  
  owsa.gg <- ggplot(data = melt(owsa.df, id.vars = "param", 
                                variable.name = "Strategy"), 
                    aes(x = param, y = value, color = Strategy)) +
    geom_line() +
    xlab(paramName) +
    ggtitle("One-way sensitivity analysis", subtitle = outcomeName)+
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom")
  return(owsa.gg)
}

#-----------------------------------------------#
### Function for plotting two-way SA Diagrams ###
#-----------------------------------------------#
twsa.plot.det <- function(params, outcomes, strategyNames, outcomeName = "Outcome", mx = T){
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