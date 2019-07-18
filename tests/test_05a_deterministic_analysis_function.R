context("testing 05a_deterministic_analysis_functions.R")

library(dplyr)    # For data manipulation

workdir <- getwd()
setwd("..")
workdir <- getwd()

#### Load inputs ####
source(paste0(workdir, "/analysis/01_model_inputs.R"))

#### Load functions ####
source(paste0(workdir, "/R/02_simulation_model_functions.R"))
source(paste0(workdir, "/R/05a_deterministic_analysis_functions.R"))


test_that("checking output", {
  ce_table <- calculate_ce_out(l_params_all)
  
  expect_true(all(colnames(ce_table) == c("Strategy", "Cost", "Effect", "NMB")))
  expect_true(is.factor(ce_table$Strategy))
  expect_true(all(is.numeric(ce_table$Cost), is.numeric(ce_table$Effect), is.numeric(ce_table$NMB)))
  expect_true(all(ce_table$Strategy == l_params_all$v_names_str))
})


