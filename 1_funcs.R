get_mahalanobis_distance <- function(df, auto_drop, re_center) {
  df$mahal <- stats::mahalanobis(df, colMeans(df), cov(df))
  df$p <- pchisq(df$mahal, df = ncol(df) - 1, lower.tail = FALSE)
  print(nrow(df %>% filter(p < .001)))
  # if (auto_drop) {
  #   df <- df %>%
  #     filter(p >=.001)
  #   if (re_center) {
  #     #df <- df %>%
  #     #  mutate(across(!mahal & !p, ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  #   }
  # }
  #df <- df %>% select(!mahal & !p)
  #df <- subset(df, select = -c(mahal, p))
  return(df)
}

getAndLinkSurveyData <- function(
  sync_df,
  jhTraitFile,
  nasaWorkloadFile,
  nasaTeamCohesionFile,
  nasaTeamPerformanceFile,
  nasaTeamPerformanceMCCFile,
  nasaIPIPFile,
  nasaPOMSFile) {

  trait_df <- read_csv(jhTraitFile) %>%
    dplyr::select(ID, All_Dimensions, Survey, value) %>%
    pivot_wider(names_from = c(Survey,All_Dimensions), values_from = value) %>%
    rename(Part_ID = ID) %>%
    mutate(Part_ID = as.character(Part_ID))
  print(nrow(trait_df))
  cmbDF <- sync_df %>% left_join(trait_df) %>%
    mutate(Mission_day = as.numeric(Mission_day))
  # Add in IPIP data_data
  trait_df <- read_excel(nasaIPIPFile, sheet = 'Data', col_names = TRUE) %>%
    dplyr::select(ID, IPIP_Neuroticism, IPIP_Extraversion, IPIP_Openness, IPIP_Agreeableness, IPIP_Conscientiousness) %>%
    rename(Part_ID = ID) %>%
    mutate(Part_ID = as.character(Part_ID))
  cmbDF <- cmbDF %>%
    left_join(trait_df)
  # Add in workload, team perforamcne, and team cohesion
  wl_df <- read_excel(nasaWorkloadFile, sheet = 'Data', col_names = TRUE) %>%
    filter(MissionPhase_Nom == 'In-Mission') %>%
    mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
    dplyr::select(ID, Mission_day, Workload, Sleep_Interfere, Sleep_OtherPerf, Daytime_Performance_Mean) %>%
    rename(Part_ID = ID) %>%
    mutate(Part_ID = as.character(Part_ID))
  print(nrow(wl_df))
  print(nrow(cmbDF))
  cmbDF <- cmbDF %>%
    left_join(wl_df, by = c("Part_ID", "Mission_day"))
  print(nrow(cmbDF))
  # Add in team cohesion ratings from crew
  tc_df <- read_excel(nasaTeamCohesionFile, sheet = 'Data', col_names = TRUE) %>%
    filter(MissionPhase_Nom == 'In-Mission') %>%
    mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
    dplyr::select(ID, Mission_day, CohesionTask_Indiv, CohesionSoc_Indiv, CohesionOverall_Indiv, CohesionTask_TeamFinal, CohesionSoc_TeamFinal, CohesionOverall_TeamFinal) %>%
    rename(Part_ID = ID) %>%
    mutate(Part_ID = as.character(Part_ID))
  cmbDF <- cmbDF %>%
    left_join(tc_df, by = c("Part_ID", "Mission_day"))
  print(nrow(cmbDF))

  # Add in team performance ratings from crew
  tp_df <- read_excel(nasaTeamPerformanceFile, sheet = 'Data', col_names = TRUE) %>%
    filter(MissionPhase_Nom == 'In-Mission') %>%
    mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
    # dplyr::select(ID, Mission_day, TeamPerf_Indiv,TeamPerf_TeamFinal) %>%
    rename(Part_ID = ID) %>%
    mutate(Part_ID = as.character(Part_ID))
  # join in individual ratings
  cmbDF <- cmbDF %>%
    left_join(tp_df[, c("Part_ID", "Mission_day", "TeamPerf_Indiv")] , by = c("Part_ID", "Mission_day"))
  print(nrow(cmbDF))
  # join in average daily ratings
  tp_df$Team <- paste0('C',tp_df$Campaign,'M',tp_df$Mission)
  tp_df <- tp_df %>%
    dplyr::select(Team, Mission_day, TeamPerf_TeamFinal)
  cmbDF <- cmbDF %>%
    left_join(tp_df, by = c("Team", "Mission_day"))
  print(nrow(cmbDF))
  # Add in team performance ratings from MCC
  tp_df <- read_excel(nasaTeamPerformanceMCCFile, sheet = 'Data', col_names = TRUE) %>%
    filter(MissionPhase_Nom == 'In-Mission') %>%
    mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
    dplyr::select(Campaign, Mission, Mission_day, TeamPerf_MCC_Daily)
  tp_df <- unique(tp_df) # there were multiple rows for individual MCC ratings; but we're just usting the average score
  tp_df$Team <- paste0('C',tp_df$Campaign,'M',tp_df$Mission)
  cmbDF <- cmbDF %>%
    left_join(tp_df[,c("Team","Mission_day","TeamPerf_MCC_Daily")], by = c("Team", "Mission_day"))
  print(nrow(cmbDF))

  # Add nasaPOMSFile
  poms_df <- read_excel(nasaPOMSFile, sheet = 'Data', col_names = TRUE) %>%
  filter(MissionPhase_Nom == 'In-Mission') %>%
  mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
  dplyr::select(ID, Mission_day, POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum, POMS_AngerHostility_Sum, POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum, POMS_TotalMoodDist_Sum) %>%
      rename(Part_ID = ID) %>%
      mutate(Part_ID = as.character(Part_ID))
  cmbDF <- cmbDF %>%
    left_join(poms_df, by = c("Part_ID", "Mission_day"))
  print(nrow(cmbDF))
  return(unique(cmbDF))
}
