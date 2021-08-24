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

debuggingState(on = FALSE)
# start ve with: source python3/bin/activate in project folder
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()
Sys.setenv(RETICULATE_PYTHON = config$py_version)
reticulate::source_python("1_funcs.py")
source(here("1_funcs.R"), echo = TRUE)
# source(here('2a_connect.R'), echo = TRUE)


tasks_df <- get_task_list(
  task_lists_and_categories = config$task_lists_and_categories,
  data_dir = config$root_data_dir
)


sync_df <- get_E4_synchronies(
  tasks_df = tasks_df,
  offset = config$sync_offset,
  roles = config$sync_roles,
  measures = config$sync_measures,
  sampling_freq = config$sync_sampling_freq,
  corr_method = config$sync_corr_method,
  use_residuals = config$synch_use_residuals,
  missing_E4_flag = config$sync_missing_E4_flag,
  NA_E4_flag = config$sync_NA_E4_flag,
  db_path = paste0(config$root_data_dir, "/", config$sync_db_path),
  save_csv = FALSE
)

sync_df_long <- reshape_sync_data(
  sync_df = sync_df,
  tasks_df = tasks_df,
  roles = config$sync_roles,
  measures = config$sync_measures,
  metrics = config$sync_metrics,
  data_dir = config$root_data_dir,
  save_csv = FALSE
)