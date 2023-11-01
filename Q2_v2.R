####################################################################################################
####################################################################################################
################################ Q2 Analyses -- Is there a trait component
####################################################################################################
####################################################################################################

library(lme4)
library(gamlss)
names(cmbDF)

df <- cmbDF %>%
  mutate(Mission_day = Mission_day -1) %>% # Starts mission day at 0
  mutate(Part_ID = as.factor(Part_ID)) %>%
  mutate(Team = as.factor(Team)) %>%
  dplyr::select(Team, Part_ID, Mission_day,
                EDA.Empath.Avg, EDA.Driver.Avg,HR.Empath.Avg,HR.Driver.Avg,
                NRI_PerMea,NRI_EmSel,NRI_EmOth,NRI_SelfImp,NRI_ConfAvoi,NRI_NRI_Mean,IRI_Fantasy_Mean,
                IRI_EmpathicConcern_Mean,IRI_PerspectiveTaking_Mean,IRI_PersonalDistress_Mean,IRI_IRI_Mean,
                CO_Affiliation_CO,CO_Dominance_CO,CO_Mean_CO,MRM_MRM_Mean,
                IPIP_Neuroticism,IPIP_Extraversion,IPIP_Openness,IPIP_Agreeableness,IPIP_Conscientiousness
  ) %>%
  drop_na() %>%
  mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  # mutate(across(contains('EDA'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  # mutate(across(contains('HR'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 

hist(df$HR.Empath.Avg)

HR_D_trait.0 <- lm(as.formula('HR.Driver.Avg ~1'), data = df)
f <- as.formula(HR.Driver.Avg ~1 + (1|Part_ID))
HR_D_trait.1 <- lmer(f, data = df, REML = TRUE)
anova(HR_D_trait.1,HR_D_trait.0)
summary(HR_D_trait.1)
lattice::qqmath(HR_D_trait.1)
sjPlot::tab_model(HR_D_trait.0,HR_D_trait.1)

HR_D_trait.2 <- stats::update(HR_D_trait.1, .~. + 
                       Mission_day + 
                       NRI_EmSel + IRI_EmpathicConcern_Mean +  IRI_PerspectiveTaking_Mean +
                       # NRI_PerMea + NRI_EmSel + NRI_EmOth + NRI_SelfImp + NRI_ConfAvoi + 
                       # NRI_NRI_Mean +
                       # IRI_Fantasy_Mean + IRI_EmpathicConcern_Mean +  IRI_PerspectiveTaking_Mean + IRI_PersonalDistress_Mean + 
                       # IRI_IRI_Mean +
                       # CO_Affiliation_CO + CO_Dominance_CO +
                       CO_Mean_CO +
                       MRM_MRM_Mean +
                       IPIP_Neuroticism + IPIP_Extraversion + IPIP_Openness + IPIP_Agreeableness + IPIP_Conscientiousness
                     )
anova(HR_D_trait.1,HR_D_trait.2)
summary(HR_D_trait.2)
lattice::qqmath(EDA_E_trait.2)
# sjPlot::tab_model(EDA_D_trait.0,EDA_D_trait.1,EDA_D_trait.2)
# sjPlot::tab_model(EDA_E_trait.0,EDA_E_trait.1,EDA_E_trait.2)
# sjPlot::tab_model(HR_D_trait.0,HR_D_trait.1,HR_D_trait.2)
# sjPlot::tab_model(HR_E_trait.0,HR_E_trait.1,HR_E_trait.2)
sjPlot::tab_model(
  EDA_D_trait.2,EDA_E_trait.2,HR_D_trait.2,HR_E_trait.2,
  pred.labels = c('(Intercept)','Mission day','Empathy toward Self (NRI)',
                  'Empathic Conern (IRI)','Perspective Taking (IRI)','Collective Orientation',
                  'Mind Reading Motivation','Neuroticism','Extraversion','Openness','Agreeableness',
                  'Conscientiousness')
  # file = here('HERA_TPD_Trait.html')
  )
