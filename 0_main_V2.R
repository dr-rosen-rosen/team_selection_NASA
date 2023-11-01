####################################################################################################
####################################################################################################
################################ Main analysis file
################################ for NASA data anlaysis
####################################################################################################
####################################################################################################
library(config)
library(tidyverse)
library(reticulate)
library(here)
library(readxl)
library(comprehenr)

debuggingState(on = FALSE)
# start ve with: source python3/bin/activate in project folder
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()
Sys.setenv(RETICULATE_PYTHON = config$py_version)
reticulate::source_python("1_funcs.py")
source(here("1_funcs.R"), echo = TRUE)
# source(here('2a_connect.R'), echo = TRUE)

cmbDF <- getAndLinkSurveyData(
  sync_df = sync_df_long,
  jhTraitFile = here(config$root_data_dir,"HERA_surveys",config$jhTraitDataFile),
  nasaWorkloadFile = here(config$root_data_dir,"HERA_surveys",config$nasaWorkloadFile),
  nasaTeamCohesionFile = here(config$root_data_dir,"HERA_surveys",config$nasaTeamCohesionFile),
  nasaTeamPerformanceFile = here(config$root_data_dir,"HERA_surveys",config$nasaTeamPerformanceFile),
  nasaTeamPerformanceMCCFile = here(config$root_data_dir,"HERA_surveys",config$nasaTeamPerformanceMCCFile),
  nasaIPIPFile = here(config$root_data_dir,"HERA_surveys",config$nasaIPIPFile),
  nasaPOMSFile = here(config$root_data_dir,"HERA_surveys",config$nasaPOMSFile)
  )
head(cmbDF)


  
  
  