####################################################################################################
####################################################################################################
################################ Q2 Analyses -- Is there a trait component
################################ For IWS 2023
####################################################################################################
####################################################################################################

library(lme4)
library(gamlss)
# names(cmbDF)

df <- iws_2023_df %>%
  mutate(md = md -1) %>% # Starts mission day at 0
  mutate(md = as.character(md)) %>%
  mutate(part_id = as.factor(part_id)) %>%
  mutate(team = as.factor(team)) %>%
  dplyr::select(team, part_id, md,
                # eda_driver_50_avg, hr_driver_50_avg,
                # eda_empath_scores_50_avg,hr_empath_scores_50_avg,
                # eda_empath_scores_20_avg,hr_empath_scores_20_avg,
                eda_empath_scores_5_avg,hr_empath_scores_5_avg,
                IdInfAttr_MLQ, IdInfBeh_MLQ,InMot_MLQ, InSti_MLQ,InCons_MLQ,CoRew_MLQ,
                LFL_MLQ,Sat_MLQ,Fantasy_Total_IRI,EmpathicConcern_Total_IRI,
                PerspectiveTaking_Total_IRI,PersonalDistress_Total_IRI,Mean_IRI,PerMea_NRI,EmSel_NRI,
                ConfAvoi_NRI,Mean_NRI,Affliation_CO,Dominance_CO,Mean_CO,Affliation_CO_NEW,Dominance_CO_NEW,
                Mean_CO_NEW
                # IPIP_Neuroticism,IPIP_Extraversion,IPIP_Openness,IPIP_Agreeableness,IPIP_Conscientiousness
  ) %>%
  drop_na() %>%
  mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(md = as.integer(md))
  # mutate(across(contains('EDA'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  # mutate(across(contains('HR'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 

hist(df$eda_empath_scores_5_avg)

HR_D_trait.0 <- lm(as.formula('hr_empath_scores_5_avg ~1'), data = df)
f <- as.formula(hr_empath_scores_5_avg~1 + (poly(md,2)|part_id))
HR_D_trait.1 <- lmer(f, data = df, REML = TRUE)
anova(HR_D_trait.1,HR_D_trait.0)
summary(HR_D_trait.1)
lattice::qqmath(HR_D_trait.1)
sjPlot::tab_model(HR_D_trait.0,HR_D_trait.1)

HR_D_trait.2 <- stats::update(HR_D_trait.1, .~. + 
                       # md +
                        #  IdInfAttr_MLQ +
                        # IdInfBeh_MLQ+
                        #  InMot_MLQ+
                        #  InSti_MLQ+
                        #  InCons_MLQ+
                        #  CoRew_MLQ+
                        #  LFL_MLQ+
                        #  Sat_MLQ+
                        # Fantasy_Total_IRI+
                         # EmpathicConcern_Total_IRI+
                         # PerspectiveTaking_Total_IRI+
                         # PersonalDistress_Total_IRI+
                         # Mean_IRI+
                         PerMea_NRI#+
                         # EmSel_NRI+
                         # ConfAvoi_NRI#+
                       # Mean_NRI+
                         # Affliation_CO+
                         # Dominance_CO+
                         # Mean_CO+
                         # Affliation_CO_NEW+
                         # Dominance_CO_NEW#+
                       # Mean_CO_NEW
                       # MRM_MRM_Mean +
                       # IPIP_Neuroticism + IPIP_Extraversion + IPIP_Openness + IPIP_Agreeableness + IPIP_Conscientiousness
                     )
anova(HR_D_trait.1,HR_D_trait.2)
summary(HR_D_trait.2)
lattice::qqmath(EDA_E_trait.2)
# sjPlot::tab_model(EDA_D_trait.0,EDA_D_trait.1,EDA_D_trait.2)
# sjPlot::tab_model(EDA_E_trait.0,EDA_E_trait.1,EDA_E_trait.2)
sjPlot::tab_model(HR_D_trait.0,HR_D_trait.1,HR_D_trait.2)
sjPlot::plot_model(HR_D_trait.2, type = 're') + ggthemes::theme_tufte() + 
  labs(title = 'HR Empath Random Effects', y = element_blank())


# sjPlot::tab_model(HR_E_trait.0,HR_E_trait.1,HR_E_trait.2)
sjPlot::tab_model(
  EDA_D_trait.2,EDA_E_trait.2,HR_D_trait.2,HR_E_trait.2,
  pred.labels = c('(Intercept)','Mission day','Empathy toward Self (NRI)',
                  'Empathic Conern (IRI)','Perspective Taking (IRI)','Collective Orientation',
                  'Mind Reading Motivation','Neuroticism','Extraversion','Openness','Agreeableness',
                  'Conscientiousness')
  # file = here('HERA_TPD_Trait.html')
  )



EDA_D_trait.0 <- lm(as.formula('eda_empath_scores_5_avg ~1'), data = df)
f <- as.formula(eda_empath_scores_5_avg~1 + (poly(md,2)|part_id))
EDA_D_trait.1 <- lmer(f, data = df, REML = TRUE)
anova(EDA_D_trait.1,EDA_D_trait.0)
summary(EDA_D_trait.1)
lattice::qqmath(EDA_D_trait.1)
sjPlot::tab_model(HR_D_trait.0,HR_D_trait.1)

EDA_D_trait.2 <- stats::update(EDA_D_trait.1, .~. + 
                                # md +
                              #  IdInfAttr_MLQ +
                              # IdInfBeh_MLQ+
                              #  InMot_MLQ+
                              #  InSti_MLQ+
                              #  InCons_MLQ+
                              #  CoRew_MLQ+
                              #  LFL_MLQ+
                              #  Sat_MLQ+
                              # Fantasy_Total_IRI+
                              # EmpathicConcern_Total_IRI+
                              # PerspectiveTaking_Total_IRI+
                              # PersonalDistress_Total_IRI+
                              # Mean_IRI+
                              # PerMea_NRI+
                              # EmSel_NRI+
                              # ConfAvoi_NRI+
                              # Mean_NRI+
                              # Affliation_CO+
                              # Dominance_CO+
                              # Mean_CO+
                              Affliation_CO_NEW#+
                              # Dominance_CO_NEW#+
                              # Mean_CO_NEW
                              # MRM_MRM_Mean +
                              # IPIP_Neuroticism + IPIP_Extraversion + IPIP_Openness + IPIP_Agreeableness + IPIP_Conscientiousness
)
anova(EDA_D_trait.1,EDA_D_trait.2)
summary(EDA_D_trait.2)
lattice::qqmath(EDA_D_trait.2)
# sjPlot::tab_model(EDA_D_trait.0,EDA_D_trait.1,EDA_D_trait.2)
# sjPlot::tab_model(EDA_E_trait.0,EDA_E_trait.1,EDA_E_trait.2)
sjPlot::tab_model(EDA_D_trait.1,EDA_D_trait.2)
sjPlot::plot_model(EDA_D_trait.2, type = 're') + ggthemes::theme_tufte() +
  labs(title = 'EDA Empath Random Effects', y = element_blank())

library(lcmm)
df <- df %>% mutate(part_id = as.integer(as.character(part_id)))
df <- as.data.frame(lapply(df, unlist))
mult_lin_0 <- lcmm::multlcmm(eda_empath_scores_5_avg + hr_empath_scores_5_avg ~ 1 + poly(md,2),
                             random = ~ md,
                             # mixture = ~1,
                             subject = 'part_id',
                             data = df,
                             # B = mult_lin_0,
                             ng = 1)
mult_lin <- lcmm::multlcmm(eda_empath_scores_5_avg + hr_empath_scores_5_avg ~ 1 + poly(md,2),
                           random = ~ 1 + poly(md,2),
                           mixture = ~1 + poly(md,2),
                           subject = 'part_id',
                           data = df,
                           B = mult_lin_0,
                           ng = 2)
summary(mult_lin)
mult_lin$pprob
class_df <- mult_lin$pprob %>% dplyr::select(part_id,class)# %>% mutate(part_id = as.factor(part_id))
df <- df %>% mutate(part_id = as.character(part_id)) %>% 
                      left_join(class_df, by = 'part_id') %>% 
                      mutate(part_id = as.character(part_id))

df %>% pivot_longer(cols = c(eda_empath_scores_5_avg,hr_empath_scores_5_avg),
                    names_to = "physio_signal", values_to = "sync_metric") %>%
  group_by(class, md, physio_signal) %>%
    summarise(
    # EDA = mean(eda_empath_scores_5_avg),
    # HR = mean(hr_empath_scores_5_avg)
      empath_score = mean(sync_metric)
  ) %>%
  ungroup() %>%
  mutate(class = as.factor(class), physio_signal = as.factor(physio_signal)) %>%
  ggplot(aes(x = as.factor(md), y = empath_score, color = physio_signal, group = physio_signal)) + geom_point() + geom_line() +
  # facet_wrap(~class) +
  facet_grid(rows = vars(class)) +
  ggthemes::theme_tufte()

mult_lin_2 <- lcmm::multlcmm(eda_empath_scores_5_avg + hr_empath_scores_5_avg ~ 1 + poly(md,2),
                           random = ~ 1 + poly(md,2),
                           mixture = ~1 + poly(md,2),
                           subject = 'part_id',
                           data = df,
                           B = mult_lin_0,
                           ng = 3)
summary(mult_lin_2)
mult_lin_2$pprob

