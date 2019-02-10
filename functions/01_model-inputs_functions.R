#------------------------------------------------------#
#### Define base-case parameters set              ####
#------------------------------------------------------#
f.define_init_params <- function(){ # User defined
  ## Load base-case set of parameters from .csv file into data.frame
  df.params.init <- read.csv(file = "data/01_init-params.csv")

  df.params.init <- data.frame(
    ## External
    # Costs
    c.H   = df.params.init$c.H,   # cost of remaining one cycle healthy 
    c.S1  = df.params.init$c.S1,  # cost of remaining one cycle sick 
    c.S2  = df.params.init$c.S2,  # cost of remaining one cycle sicker 
    c.D   = df.params.init$c.D,   # cost of being dead (per cycle)
    c.Trt = df.params.init$c.Trt, # cost of treatment (per cycle 
    # Utilities
    u.H   = df.params.init$u.H  , # utility when healthy 
    u.S1  = df.params.init$u.S1 , # utility when sick 
    u.S2  = df.params.init$u.S2 , # utility when sicker
    u.D   = df.params.init$u.D  , # utility when healthy 
    u.Trt = df.params.init$u.Trt, # utility when being treated
    # Transition probabilities (per cycle)
    p.HS1 = df.params.init$p.HS1,  # probability to become sick when healthy
    p.S1H = df.params.init$p.S1H,   # probability to become healthy when sick
    
    ## Estimated parameters (values are place holders)
    # Cost of S1
    
    ## Calibrated parameters (values are place holders)
    p.S1S2 = df.params.init$p.S1S2, # probability to become sicker when sick
    hr.S1  = df.params.init$hr.S1 , # hazard ratio of death in S1 vs healthy
    hr.S2  = df.params.init$hr.S2   # hazard ratio of death in S2 vs healthy 
  )
  return(df.params.init)
}