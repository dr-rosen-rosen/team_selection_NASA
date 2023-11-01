####################################################################################################
####################################################################################################
################################ Q1 Analyses -- Does Synchrony Predict things we care about?
################################ Done for IWS 2023
####################################################################################################
####################################################################################################

library(lme4)
library(gamlss)
library(tidyverse)
# names(cmbDF)
skimr::skim(iws_2023_df)
df <- iws_2023_df %>%
  mutate(md = md -1) %>% # Starts mission day at 0
  mutate(part_id = as.factor(part_id)) %>%
  mutate(team = as.factor(team)) %>%
  # dplyr::select(team, part_id, Mission_day,
  #               EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
  #               Workload,
  #               CohesionTask_Indiv,CohesionSoc_Indiv,CohesionOverall_Indiv,CohesionTask_TeamFinal,
  #               CohesionSoc_TeamFinal,CohesionOverall_TeamFinal,
  #               TeamPerf_Indiv,TeamPerf_TeamFinal,TeamPerf_MCC_Daily,
  #               POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,POMS_AngerHostility_Sum,
  #               POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum, 
  #               POMS_TotalMoodDist_Sum
  # ) %>%
  #drop_na()
  # mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  mutate(across(contains('eda'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(across(contains('hr'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 


#########################################
######### Team Performance
#########################################

## Find distribution for outcome
df_TP <- df %>%
  dplyr::select(team, part_id, md,
                # eda_driver_50_avg, hr_driver_50_avg,
                
                # eda_empath_scores_50_social,
                # hr_empath_scores_50_social,
                # eda_empath_scores_50_team_action,
                # hr_empath_scores_50_team_action,
                # eda_empath_scores_50_team_transition,
                # hr_empath_scores_50_team_transition,

                # eda_empath_scores_20_social,
                # hr_empath_scores_20_social,
                # eda_empath_scores_20_team_action,
                # hr_empath_scores_20_team_action,
                # eda_empath_scores_20_team_transition,
                # hr_empath_scores_20_team_transition,
                # 
                # eda_empath_scores_5_social,
                # hr_empath_scores_5_social,
                # eda_empath_scores_5_team_action,
                # hr_empath_scores_5_team_action,
                # eda_empath_scores_5_team_transition,
                # hr_empath_scores_5_team_transition,
                
                # eda_empath_scores_50_avg,hr_empath_scores_50_avg,
                # eda_empath_scores_20_avg,hr_empath_scores_20_avg,
                eda_empath_scores_5_avg,hr_empath_scores_5_avg,
                
                TeamPerf_Indiv,TeamPerf_TeamFinal) %>%
  na.omit() %>%
  mutate(TeamPerf_Indiv_scale = TeamPerf_Indiv / 7)
  # mutate(TeamPerf_Indiv_dich = ifelse(TeamPerf_Indiv < 7, 1, 0)) #%>%
  # mutate(TeamPerf_MCC_Daily_dich = ifelse(TeamPerf_MCC_Daily < 7, 1, 0)) 
# mutate(across(contains('TeamPerf'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 



#################### LMER
################## Team Performance

TP.lmer <- lmer(TeamPerf_Indiv ~
                  md +
                  eda_empath_scores_5_avg +
                  hr_empath_scores_5_avg +
                  md*eda_empath_scores_5_avg +
                  md*hr_empath_scores_5_avg +
                  
                  # eda_empath_scores_5_social +
                  # hr_empath_scores_5_social +
                  # md*eda_empath_scores_5_social +
                  # md*hr_empath_scores_5_social +
                  
                  # eda_empath_scores_5_team_action +
                  # hr_empath_scores_5_team_action +
                  # md*eda_empath_scores_5_team_action +
                  # md*hr_empath_scores_5_team_action +
                  
                  # eda_empath_scores_5_team_transition +
                  # hr_empath_scores_5_team_transition +
                  # md*eda_empath_scores_5_team_transition +
                  # md*hr_empath_scores_5_team_transition +
                  
                  # md^2 +
                  # poly(md,2) +
                  # # eda_driver_50_avg +
                  # # hr_driver_50_avg +
                  # eda_empath_scores_50_avg +
                  # hr_empath_scores_50_avg +
                  # poly(md,2)*hr_empath_scores_50_avg +
                  # poly(md,2) +
                  # eda_empath_scores_50_social +
                  # eda_empath_scores_50_team_action +
                  # hr_empath_scores_50_social +
                  # hr_empath_scores_50_team_action +
                  # md:eda_empath_scores_50_social +
                  # md:eda_empath_scores_50_team_action +
                  # eda_empath_scores_50_team_transition +
                  # md:eda_empath_scores_50_team_transition +
                  # hr_empath_scores_50_team_transition +
                  # md:hr_empath_scores_50_social +
                  # md:hr_empath_scores_50_team_action +
                  (1|part_id), data = df_TP,
                  REML = FALSE,
                  control = lmerControl(optimizer ="Nelder_Mead"))
lme4::allFit(TP.lmer)
sjPlot::plot_model(TP.lmer, type = 're') #+ ggthemes::theme_tufte()
sjPlot::tab_model(TP.lmer,
                  title = 'Overall Empath Scores and Daily Team Performance Ratings',
                  dv.labels = 'Team Performance Ratings',
                  show.aic = TRUE#,
                  # file = here('output','overallEmpathTPR.html')
                  )

eda_em_overall <- sjPlot::plot_model(TP.lmer,type = 'int', show.legend = FALSE, show.values = FALSE)[[1]] + 
  ggthemes::theme_tufte() + 
  labs(y = 'Team Perf Rating', x = 'Mission Day', title = 'Predicted values of team perf ratings') +
  # scale_fill_discrete(labels=c('High Program', 'Low Program'))
  guides(
    fill = guide_legend(title = 'EDA Empath')
    ) + theme(legend.position="bottom") 
library(patchwork)
hr_em_overall / eda_em_overall




hist(df_TP$TeamPerf_Indiv_scale)

TP.fit <- gamlss::fitDist(df_TP$TeamPerf_Indiv, type = 'realAll')
TP.fit$fits
TP.hist <- gamlss::histDist(TeamPerf_Indiv, family = GA, nbins = 30, data = df_TP)

## Fit model
## Fit model
TP.m_gamlss <- gamlss::gamlss(
  formula = TeamPerf_Indiv_scale ~ 
    md + 
    # eda_empath_scores_50_avg +
    # hr_empath_scores_50_avg +
    # eda_empath_scores_20_avg +
    # hr_empath_scores_20_avg +
    eda_empath_scores_5_avg +
    hr_empath_scores_5_avg +
    md*eda_empath_scores_5_avg +
    md*hr_empath_scores_5_avg +
    
    eda_empath_scores_5_social +
    hr_empath_scores_5_social +
    eda_empath_scores_5_team_action +
    hr_empath_scores_5_team_action +
    eda_empath_scores_5_team_transition +
    hr_empath_scores_5_team_transition +
    # eda_empath_scores_50_team_transition +
    # hr_empath_scores_50_team_transition +
    # md*eda_empath_scores_50_team_transition +
    # md*hr_empath_scores_50_team_transition +
    # eda_empath_scores_50_social +
    # hr_empath_scores_50_social +
    # md*eda_empath_scores_50_social +
    # md*hr_empath_scores_50_social +
    # eda_empath_scores_50_avg + 
    # hr_empath_scores_50_avg +
    # md*eda_empath_scores_50_avg + 
  # md*hr_empath_scores_50_avg +
  re(random = ~1|part_id),
  family = GA(), data = df_TP, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(TP.m_gamlss)
Rsq(TP.m_gamlss)
plot(TP.m_gamlss)
wp(TP.m_gamlss)
sjPlot::tab_model(TP.m_gamlss)
# sjPlot::plot_model(TP.m_gamlss)

## Evaluate

