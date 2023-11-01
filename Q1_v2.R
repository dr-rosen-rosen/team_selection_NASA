####################################################################################################
####################################################################################################
################################ Q1 Analyses -- Does Synchrony Predict things we care about?
####################################################################################################
####################################################################################################

library(lme4)
library(gamlss)
library(tidyverse)
names(cmbDF)

df <- cmbDF %>%
  mutate(Mission_day = Mission_day -1) %>% # Starts mission day at 0
  mutate(Part_ID = as.factor(Part_ID)) %>%
  mutate(Team = as.factor(Team)) %>%
  dplyr::select(Team, Part_ID, Mission_day,
    EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
    Workload,
    CohesionTask_Indiv,CohesionSoc_Indiv,CohesionOverall_Indiv,CohesionTask_TeamFinal,
    CohesionSoc_TeamFinal,CohesionOverall_TeamFinal,
    TeamPerf_Indiv,TeamPerf_TeamFinal,TeamPerf_MCC_Daily,
    POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,POMS_AngerHostility_Sum,
    POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum, 
    POMS_TotalMoodDist_Sum
    ) %>%
  #drop_na()
  # mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  mutate(across(contains('EDA'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(across(contains('HR'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 

#########################################
######### Workload
#########################################

## Find distribution for outcome
df_WL <- df %>%
  dplyr::select(Team, Part_ID, Mission_day,
         EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
         Workload) %>%
  na.omit() %>%
  mutate(across(contains('Workload'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 
  # mutate(Workload0to1 = Workload / 100)
hist(df_WL$Workload)

WL.fit <- gamlss::fitDist(df_WL$Workload, type = 'realAll')
WL.fit$fits
WL.hist <- gamlss::histDist(Workload, family = SEP3, nbins = 30, data = df_WL)

## Fit model
WL.m_gamlss <- gamlss::gamlss(
  formula = Workload ~ 
    Mission_day + 
    EDA.Empath.Avg + EDA.Driver.Avg + HR.Empath.Avg + HR.Driver.Avg + 
    # poly(Mission_day,2) +

    re(random = ~1|Part_ID),
  family = SEP3(), data = df_WL, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(WL.m_gamlss)
Rsq(WL.m_gamlss)
plot(WL.m_gamlss)
wp(WL.m_gamlss)

modelsummary::modelsummary(
  WL.m_gamlss,
  estimate = "{estimate} ({std.error}){stars}", 
  statistic = NULL,
  coef_omit = c("(Intercept)")
)

#########################################
######### Cohesion
#########################################

## Find distribution for outcome
df_CH <- df %>%
  dplyr::select(Team, Part_ID, Mission_day,
                EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
                CohesionTask_Indiv,CohesionSoc_Indiv,CohesionOverall_Indiv) %>%
  na.omit() %>%
  mutate(CohesionTask_Indiv_dich = ifelse(CohesionTask_Indiv < 7, 1, 0)) %>%
  mutate(CohesionSoc_Indiv_dich = ifelse(CohesionSoc_Indiv < 7, 1, 0)) %>%
  mutate(CohesionOverall_Indiv_dich = ifelse(CohesionOverall_Indiv < 7, 1, 0))
  # mutate(across(contains('Cohesion'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
hist(df_CH$CohesionSoc_Indiv)

CHo.fit <- gamlss::fitDist(df_CH$CohesionOverall_Indiv_dich, type = 'counts')
CHo.fit$fits
CHo.hist <- gamlss::histDist(CohesionOverall_Indiv_dich, family = ZAZIPF, nbins = 30, data = df_CH)

CHt.fit <- gamlss::fitDist(df_CH$CohesionTask_Indiv, type = 'realAll')
CHt.fit$fits
CHt.hist <- gamlss::histDist(CohesionTask_Indiv, family = ST2, nbins = 30, data = df_CH)

CHs.fit <- gamlss::fitDist(df_CH$CohesionSoc_Indiv, type = 'realAll')
CHs.fit$fits
CHs.hist <- gamlss::histDist(CohesionSoc_Indiv, family = BCT, nbins = 30, data = df_CH)

## Fit model
CHs.m_gamlss <- gamlss::gamlss(
  formula = CohesionOverall_Indiv_dich ~ 
    EDA.Empath.Avg + 
    # EDA.Driver.Avg + 
    HR.Empath.Avg + 
    HR.Driver.Avg + 
    # poly(Mission_day,2) +
    Mission_day +
    re(random = ~1|Part_ID),
  family = DPO(), data = df_CH, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(CHs.m_gamlss)
Rsq(CHs.m_gamlss)
plot(CHs.m_gamlss)
wp(CHs.m_gamlss)

#########################################
######### Team Performance
#########################################

## Find distribution for outcome
df_TP <- df %>%
  dplyr::select(Team, Part_ID, Mission_day,
                EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
                TeamPerf_Indiv,TeamPerf_MCC_Daily) %>%
  na.omit() %>%
  mutate(TeamPerf_Indiv_dich = ifelse(TeamPerf_Indiv < 7, 1, 0)) %>%
  mutate(TeamPerf_MCC_Daily_dich = ifelse(TeamPerf_MCC_Daily < 7, 1, 0)) 
  # mutate(across(contains('TeamPerf'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 

hist(df_TP$TeamPerf_Indiv)

TP.fit <- gamlss::fitDist(df_TP$TeamPerf_Indiv_dich, type = 'count')
TP.fit$fits
TP.hist <- gamlss::histDist(TeamPerf_Indiv_dich, family = DPO, nbins = 30, data = df_TP)

## Fit model
## Fit model
TP.m_gamlss <- gamlss::gamlss(
  formula = TeamPerf_Indiv_dich ~ 
    EDA.Empath.Avg + 
    # EDA.Driver.Avg + 
    HR.Empath.Avg + 
    HR.Driver.Avg + 
    # poly(Mission_day,2) +
    Mission_day +
    re(random = ~1|Part_ID),
  family = DPO(), data = df_TP, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(TP.m_gamlss)
Rsq(TP.m_gamlss)
plot(TP.m_gamlss)
wp(TP.m_gamlss)

## Evaluate


#################### LMER
################## Workload

WL.m_lmer <- lmer(Workload ~
                    Mission_day + 
                    EDA.Empath.Avg + 
                    # EDA.Driver.Avg +
                    HR.Empath.Avg +
                    HR.Driver.Avg +
                    # poly(Mission_day,2) +
                    (1|Part_ID), data = df_WL,
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)
sjPlot::tab_model(WL.m_lmer)

################## Cohesion

CHs.lmer <- lmer(CohesionSoc_Indiv ~ 
                   Mission_day + 
                   EDA.Empath.Avg + 
                   # EDA.Driver.Avg +
                   HR.Empath.Avg +
                   HR.Driver.Avg +
                   # poly(Mission_day,2) +
                   (1|Team/Part_ID), data = df_CH,
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
                 )
sjPlot::tab_model(CHs.lmer)

CHo.glmer <- glmer(CohesionOverall_Indiv_dich ~ 
                     Mission_day +
                   EDA.Empath.Avg + 
                   # EDA.Driver.Avg +
                   HR.Empath.Avg +
                   HR.Driver.Avg +
                   # poly(Mission_day,2) +
                   (1|Part_ID), data = df_CH, family = binomial,
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=2e5))
)
sjPlot::tab_model(CHo.glmer)
lme4::allFit(CHs.glmer)

################## Team Performance

TP.lmer <- lmer(TeamPerf_MCC_Daily ~ 
                  Mission_day + 
                   EDA.Empath.Avg + 
                   # EDA.Driver.Avg +
                   HR.Empath.Avg +
                   HR.Driver.Avg +
                   # poly(Mission_day,2) +
                   (1|Part_ID), data = df_TP
)
sjPlot::tab_model(TP.lmer)

TP.glmer <- glmer(TeamPerf_Indiv_dich ~ 
                    Mission_day +
                    EDA.Empath.Avg + 
                  # EDA.Driver.Avg +
                  HR.Empath.Avg +
                  HR.Driver.Avg +
                  # poly(Mission_day,2) +
                  
                    (1|Part_ID), data = df_TP, family = binomial,
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5))
)
sjPlot::tab_model(TP.glmer)

################## POMS
df_POMS <- df %>%
  dplyr::select(Team, Part_ID, Mission_day,
                EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
                POMS_TensionAnxiety_Sum, POMS_DepressionDejection_Sum,POMS_AngerHostility_Sum,
                POMS_FatigueInertia_Sum, POMS_VigorActivity_Sum, POMS_ConfusionBewilderment_Sum, 
                POMS_TotalMoodDist_Sum) %>%
  na.omit() %>%
  mutate(DepressionDejection_dich = ifelse(POMS_DepressionDejection_Sum > 0, 1, 0)) %>%
  mutate(TensionAnxiety_dich = ifelse(POMS_TensionAnxiety_Sum > 0, 1, 0)) %>%
  mutate(AngerHostility_dich = ifelse(POMS_AngerHostility_Sum > 0, 1, 0)) %>%
  mutate(FatigueInertia_dich = ifelse(POMS_FatigueInertia_Sum > 0, 1, 0)) %>%
  mutate(VigorActivity_dich = ifelse(POMS_VigorActivity_Sum > 0, 1, 0)) %>%
  mutate(ConfusionBewilderment_dich = ifelse(POMS_ConfusionBewilderment_Sum > 0, 1, 0)) %>%
  mutate(across(contains('POMS'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 
  

POMS.lmer <- lmer(POMS_TotalMoodDist_Sum ~ 
                    Mission_day +
                    EDA.Empath.Avg + 
                  # EDA.Driver.Avg +
                  HR.Empath.Avg +
                  HR.Driver.Avg +
                  # poly(Mission_day,2) +
                  
                    (1|Part_ID), data = df_POMS
)
sjPlot::tab_model(POMS.lmer)
# DepressionDejection_dich
# TensionAnxiety_dich
# AngerHostility_dich
# FatigueInertia_dich
# VigorActivity_dich
# ConfusionBewilderment_dich

POMS.5.glmer <- glmer(ConfusionBewilderment_dich ~ 
                      Mission_day +
                      EDA.Empath.Avg + 
                    # EDA.Driver.Avg +
                    HR.Empath.Avg +
                    HR.Driver.Avg +
                    #poly(Mission_day,2) +
                    
                    (1|Part_ID), data = df_POMS, family = binomial,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5))
)
sjPlot::tab_model(POMS.glmer)


#### Tables

sjPlot::tab_model(POMS.1.glmer,POMS.2.glmer,POMS.4.glmer)

sjPlot::tab_model(
  WL.m_lmer,TP.glmer,CHo.glmer,
  dv.labels = c("Workload", "Team Performance","Team Cohesion")
  )
