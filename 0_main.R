####################################################################################################
####################################################################################################
################################ Main analysis file
################################ for NASA data anlaysis
####################################################################################################
####################################################################################################
library(config)
library(tidyverse)
#library(reticulate)
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


tasks_df <- get_task_list(
  task_lists_and_categories = config$task_lists_and_categories,
  data_dir = config$root_data_dir
)

set_hours <- function(ts, hr) {
  lubridate::hour(ts) <- hr
  lubridate::minute(ts) <- 0
  return(ts)
}

all_day_tasks <- tasks_df %>% group_by(Team, Mission_day) %>%
  rename(start_time = 'Start Time') %>%
  summarise(
    start_time = min(start_time), 
    CDR = first(na.omit(CDR)),
    FE = first(na.omit(FE)),
    MS1 = first(na.omit(MS1)),
    MS2 = first(na.omit(MS2)),
    Mission_day = first(na.omit(Mission_day))
  ) %>%
  ungroup() %>%
  mutate(
    start_time = set_hours(start_time,7), 
    stop_time = set_hours(start_time, 23)) %>%
  rename('Start Time' = start_time, 'Stop Time' = stop_time)
all_day_tasks$Task_num <- rep(1:nrow(all_day_tasks))
all_day_tasks['Activity Name'] <- 'all_day'
all_day_tasks['Crew Members'] <- 'MS-1, CDR, MS-2, FE-1'
all_day_tasks['Task_category'] <- 'all_day'

write_csv(all_day_tasks,'all_day_tasks.csv')
all_day_tasks <- read_csv('all_day_tasks.csv')
# test <- all_day_tasks %>%
#   filter(Team == 'C5M1', Mission_day == 3)

sync_df <- get_E4_synchronies(
  tasks_df = tasks_df, #all_day_tasks,#
  offset = config$sync_offset,
  roles = config$sync_roles,
  measures = config$sync_measures,
  sampling_freq = config$sync_sampling_freq,
  corr_method = config$sync_corr_method,
  use_residuals = config$synch_use_residuals,
  missing_E4_flag = config$sync_missing_E4_flag,
  NA_E4_flag = config$sync_NA_E4_flag,
  db_path = here(config$root_data_dir, config$sync_db_path),
  save_csv = FALSE
)

sync_df_long <- reshape_sync_data(
  sync_df = sync_df,
  tasks_df = all_day_tasks,#tasks_df,
  roles = config$sync_roles,
  measures = config$sync_measures,
  metrics = config$sync_metrics,
  data_dir = config$root_data_dir,
  save_csv = FALSE
) 


  
  
  