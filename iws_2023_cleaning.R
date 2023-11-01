####################################################################################################
####################################################################################################
################################ data cleaning
################################ for IWS 2022
####################################################################################################
####################################################################################################
library(tidyverse)
library(here)
# Get synchrony data
sync_df <- read.csv(here('data','sync_df_all.csv')) %>% filter(s_metric_type != 's_e') %>%
  rename(role = team_or_role_id) %>% dplyr::select(-c(X))
# Link to part ids
role_to_ids <- read_excel(here('data','HERA_Roles_to_IDs_C5_C5.xlsx')) %>%
  mutate(Role = tolower(Role)) %>%
  rename(team = Mission, part_id = ID, role = Role)
sync_df <- sync_df %>% left_join(role_to_ids,by = c('role' = 'role', 'team' = 'team'))
# reshpe
sync_df <- sync_df %>% pivot_wider(
  names_from = c(physio_signal,s_metric_type,offset_secs,task_category),
  values_from = synch_coef
)

sync_df <- sync_df %>%
  dplyr::select(-c(task_num,role)) %>%
  mutate(part_id = as.character(part_id)) %>%
  group_by(team,part_id,md) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

sync_df <- sync_df %>%
  mutate(
    eda_driver_50_avg = across(starts_with("eda_driver_50")) %>% rowSums(na.rm = TRUE),
    hr_driver_50_avg = across(starts_with("hr_driver_50")) %>% rowSums(na.rm = TRUE),
    eda_empath_scores_50_avg = across(starts_with("eda_empath_scores_50")) %>% rowSums(na.rm = TRUE),
    hr_empath_scores_50_avg = across(starts_with("hr_empath_scores_50")) %>% rowSums(na.rm = TRUE),
    
    eda_driver_20_avg = across(starts_with("eda_driver_20")) %>% rowSums(na.rm = TRUE),
    hr_driver_20_avg = across(starts_with("hr_driver_20")) %>% rowSums(na.rm = TRUE),
    eda_empath_scores_20_avg = across(starts_with("eda_empath_scores_20")) %>% rowSums(na.rm = TRUE),
    hr_empath_scores_20_avg = across(starts_with("hr_empath_scores_20")) %>% rowSums(na.rm = TRUE),
    
    eda_driver_5_avg = across(starts_with("eda_driver_5")) %>% rowSums(na.rm = TRUE),
    hr_driver_5_avg = across(starts_with("hr_driver_5")) %>% rowSums(na.rm = TRUE),
    eda_empath_scores_5_avg = across(starts_with("eda_empath_scores_5")) %>% rowSums(na.rm = TRUE),
    hr_empath_scores_5_avg = across(starts_with("hr_empath_scores_5")) %>% rowSums(na.rm = TRUE)
         )

# Team perf ratings
tp_df <- read_excel(here('data','surveys','C5 Team Performance.xlsx'), sheet = 'Data', col_names = TRUE) %>%
  filter(MissionPhase_Nom == 'In-Mission') %>%
  mutate(
    md = as.numeric(gsub('MD',"",MissionDay_Nom)),
    team = gsub('HERA',"",ID_Crew)) %>%
  # dplyr::select(ID, Mission_day, TeamPerf_Indiv,TeamPerf_TeamFinal) %>%
  rename(part_id = ID) %>%
  mutate(part_id = as.character(part_id))

tp_df2 <- read_excel(here('data','surveys','C6_M1_M3_Team_Perf_Crew.xlsx'), sheet = 'Data', col_names = TRUE) %>%
  filter(MissionPhase_Nom == 'In-Mission') %>%
  mutate(
    md = as.numeric(gsub('MD',"",MissionDay_Nom)),
    team = gsub('HERA',"",ID_Crew)) %>%
  # dplyr::select(ID, Mission_day, TeamPerf_Indiv,TeamPerf_TeamFinal) %>%
  rename(part_id = ID) #%>%
  # mutate(part_id = as.character(part_id))
tp_df <- rbind(tp_df,tp_df2) %>% 
  dplyr::select(md,team,part_id,TeamPerf_Indiv,TeamPerf_TeamFinal)

iws_2023_df <- sync_df %>% left_join(tp_df, by = c('team' ='team', 'part_id' = 'part_id','md' = 'md'))

trait_df <- read.csv(here('data','surveys','HERA_trait_C5_C6.csv')) %>%
  rename(part_id = participant_id) %>%
  mutate(part_id = as.character(part_id)) %>%
  dplyr::select(-X)
iws_2023_df <- iws_2023_df %>% left_join(trait_df, by = 'part_id')






tp_df <- rbind(tp_df,tp_df2)