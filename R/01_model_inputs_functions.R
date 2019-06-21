#----------------------------------------#
#### Load Mortality Data              ####
#----------------------------------------#
### Definition:
##   Loads age-specific mortality from .csv file into vector
### Arguments:  
##   file: String with the location and name of the file with mortality data
### Returns:
##   v.r.mort_by_age: Vector with mortality by age
##
f.load_mort_data <- function(file = "data/01_all-cause-mortality.csv"){
  df.r.mort_by_age <- read.csv(file = file)
  v.r.mort_by_age  <- df.r.mort_by_age %>%
    dplyr::select(Total) %>%
    as.matrix()              # vector with mortality rates
  return(v.r.mort_by_age)
}

#-----------------------------------------#
#### Load all parameters               ####
#-----------------------------------------#
f.load_all_params <- function(file.init = "data/01_init-params.csv",
                              file.mort = "data/01_all-cause-mortality.csv"){ # User defined
  ### Definition:
  ##   Loads all parameters for the decision model from multiple sources and 
  ##   creates a list
  ### Arguments:  
  ##   file.init: String with the location and name of the file with initial 
  ##              set of parameters
  ##   file.mort: String with the location and name of the file with mortality 
  ##              data
  ### Returns:
  ##   l.all.params: List with all parameters used for the decision model
  ##
  
  #### Load initial set of initial parameters form .csv file ####
  df.params.init  <- read.csv(file = file.init)
  
  #### All-cause age-specific mortality from .csv file ####
  v.r.mort_by_age <- f.load_mort_data(file = file.mort)
  
  l.params.all <- with(as.list(df.params.init), {
    #### General setup ####
    v.age.names <- n.age.init:(n.age.init + n.t - 1) # vector with age names
    v.n <- c("H", "S1", "S2", "D")  # vector with the 4 health states of the model:
                                    # Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n.states <- length(v.n)         # number of health states 
    v.s.init <- c(H = 1, S1 = 0, S2 = 0, D = 0) # initial state vector
    #### Create list with all parameters ####
    l.params.all <- list(
      n.age.init  = n.age.init, 
      n.t         = n.t       , 
      v.age.names = v.age.names,
      v.n = v.n,
      n.states = n.states,
      v.s.init = c(H = 1, S1 = 0, S2 = 0, D = 0),
      v.r.mort_by_age = v.r.mort_by_age
    )
    return(l.params.all)
  }
  )
  
  l.params.all <- c(l.params.all, 
                    df.params.init) # Add initial set of parameters
}