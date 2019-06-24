#-------------------------------------------------# 
#### Function to summarize posterior output  ######
#-------------------------------------------------# 
data_summary <- function(data, varname, groupnames){
  #+++++++++++++++++++++++++
  # Function to calculate the mean, standard deviation and 95% Credible Interval
  # for each group
  # Source: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summarized
  # groupnames : vector of column names to be used as
  # grouping variables
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE),
      median = quantile(x[[col]], probs = 0.5, names = FALSE),
      sd = sd(x[[col]], na.rm=TRUE),
      lb = quantile(x[[col]], probs = 0.025, names = FALSE),
      ub = quantile(x[[col]], probs = 0.975, names = FALSE))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, 
                    varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}