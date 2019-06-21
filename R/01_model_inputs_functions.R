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
load_mort_data <- function(file = "data/01_all_cause_mortality.csv"){
  df_r_mort_by_age <- read.csv(file = file)
  v_r_mort_by_age  <- df_r_mort_by_age %>%
    dplyr::select(Total) %>%
    as.matrix()              # vector with mortality rates
  return(v_r_mort_by_age)
}

#-----------------------------------------#
#### Load all parameters               ####
#-----------------------------------------#
load_all_params <- function(file.init = "data/01_init_params.csv",
                            file.mort = "data/01_all_cause_mortality.csv"){ # User defined
  ### Definition:
  ##   Loads all parameters for the decision model from multiple sources and 
  ##   creates a list
  ### Arguments:  
  ##   file.init: String with the location and name of the file with initial 
  ##              set of parameters
  ##   file.mort: String with the location and name of the file with mortality 
  ##              data
  ### Returns:
  ##   l_all_params: List with all parameters used for the decision model
  ##
  
  #### Load initial set of initial parameters form .csv file ####
  df_params_init  <- read.csv(file = file.init)
  
  #### All-cause age-specific mortality from .csv file ####
  v_r_mort_by_age <- load_mort_data(file = file.mort)
  
  l_params_all <- with(as.list(df_params_init), {
    #### General setup ####
    v_age_names <- n_age_init:(n_age_init + n_t - 1) # vector with age names
    v_n <- c("H", "S1", "S2", "D")  # vector with the 4 health states of the model:
                                    # Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n_states <- length(v_n)         # number of health states 
    v_s_init <- c(H = 1, S1 = 0, S2 = 0, D = 0) # initial state vector
    #### Create list with all parameters ####
    l_params_all <- list(
      n_age_init  = n_age_init, 
      n_t         = n_t       , 
      v_age_names = v_age_names,
      v_n = v_n,
      n_states = n_states,
      v_s_init = c(H = 1, S1 = 0, S2 = 0, D = 0),
      v_r_mort_by_age = v_r_mort_by_age
    )
    return(l_params_all)
  }
  )
  
  l_params_all <- c(l_params_all, 
                    df_params_init) # Add initial set of parameters
}