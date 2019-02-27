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
#--------------------------------------------------#
#### Load base-case parameters set              ####
#--------------------------------------------------#
f.load_init_params_from_file <- function(file = "data/01_init-params.csv"){ # User defined
  ### Definition:
  ##   Loads base-case set of parameters from .csv file into dataframe
  ### Arguments:  
  ##   file: String with the location and name of the file with initial set 
  ##         of parameters
  ### Returns:
  ##   df.params.init: Dataframe with initial set of parameters
  ##
  df.params.init <- read.csv(file = file)

  df.params.init <- data.frame(
    ### External
    ## Costs
    c.H   = df.params.init$c.H,   # cost of remaining one cycle healthy 
    c.S1  = df.params.init$c.S1,  # cost of remaining one cycle sick 
    c.S2  = df.params.init$c.S2,  # cost of remaining one cycle sicker 
    c.D   = df.params.init$c.D,   # cost of being dead (per cycle)
    c.Trt = df.params.init$c.Trt, # cost of treatment (per cycle 
    ## Utilities
    u.H   = df.params.init$u.H  , # utility when healthy 
    u.S1  = df.params.init$u.S1 , # utility when sick 
    u.S2  = df.params.init$u.S2 , # utility when sicker
    u.D   = df.params.init$u.D  , # utility when healthy 
    u.Trt = df.params.init$u.Trt, # utility when being treated
    ## Transition probabilities (per cycle)
    p.HS1 = df.params.init$p.HS1, # probability to become sick when healthy
    p.S1H = df.params.init$p.S1H, # probability to become healthy when sick
    
    ### Estimated parameters (values are place holders)
    ## Cost of S1
    
    ### Calibrated parameters (values are place holders)
    p.S1S2 = df.params.init$p.S1S2, # probability to become sicker when sick
    hr.S1  = df.params.init$hr.S1 , # hazard ratio of death in S1 vs healthy
    hr.S2  = df.params.init$hr.S2   # hazard ratio of death in S2 vs healthy 
  )
  return(df.params.init)
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
  
  #### General setup ####
  n.age.init  <- 25               # age of starting cohort
  n.t         <- 75               # time horizon, number of cycles
  v.age.names <- n.age.init:(n.age.init + n.t - 1) # vector with age names
  v.n <- c("H", "S1", "S2", "D")  # vector with the 4 health states of the model:
                                  # Healthy (H), Sick (S1), Sicker (S2), Dead (D)
  n.states <- length(v.n)         # number of health states 
  v.s.init <- c(H = 1, S1 = 0, S2 = 0, D = 0) # initial state vector
  #### All-cause age-specific mortality ####
  v.r.mort_by_age <- f.load_mort_data(file = file.mort)
  #### Load initial set of base-case external parameters ####
  df.params.init  <- f.load_init_params_from_file(file = file.init)
  
  #### Create list with all parameters ####
  l.params.all <- list(
    n.age.init  = n.age.init, 
    n.t         = n.t       , 
    v.age.names = v.age.names,
    v.n = v.n,
    n.states = n.states,
    d.c = 0.03, # discount rate for costs
    d.e = 0.03, # discount rate for QALYs
    v.s.init = c(H = 1, S1 = 0, S2 = 0, D = 0),
    v.r.mort_by_age = v.r.mort_by_age
  )
  l.params.all <- c(l.params.all, 
                    df.params.init) # Add initial set of parameters
}