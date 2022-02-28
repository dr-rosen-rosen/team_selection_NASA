####################################################################################################
####################################################################################################
################################ Main analysis file
################################ for IWS 2022
####################################################################################################
####################################################################################################
library(tidyLPA)

df <- read_csv(
  here(config$root_data_dir,
       'Summary_linked_files',
       config$fname_1s)
)

df <- df %>%
  rename(edaDriverAvg = 'EDA.Driver.Avg',
         edaEmpathAvg = 'EDA.Empath.Avg',
         hrDriverAvg = 'HR.Driver.Avg',
         hrEmpathAvg = 'HR.Empath.Avg',
         edaEmpathDyadAction = 'EDA.Empath.Dyad_action_task',
         edaEmpathTeamAction = 'EDA.Empath.Team_action_task',
         edaEmpathSocial = 'EDA.Empath.Social',
         edaEmpathTransition = 'EDA.Empath.Team_transition_task',
         hrEmpathDyadAction = 'HR.Empath.Dyad_action_task',
         hrEmpathTeamAction = 'HR.Empath.Team_action_task',
         hrEmpathSocial = 'HR.Empath.Social',
         hrEmpathTransition = 'HR.Empath.Team_transition_task')

# names(df)
# df <- df %>%
#   separate(temp_i,c('taskNum','team','partNum','day'), sep = '_')

df <- df %>%
  #select(edaDriverAvg, edaEmpathAvg, hrDriverAvg, hrEmpathAvg) %>%
  #drop_na(edaDriverAvg, edaEmpathAvg, hrDriverAvg, hrEmpathAvg) %>%
  mutate(edaEmpathTeamAvg = rowMeans(.[,c('edaEmpathTeamAction', 'edaEmpathDyadAction', 'edaEmpathTransition')], na.rm = TRUE)) %>%
  mutate(hrEmpathTeamAvg = rowMeans(.[,c('hrEmpathTeamAction', 'hrEmpathDyadAction', 'hrEmpathTransition')], na.rm = TRUE)) %>%
  
  drop_na(edaEmpathTeamAvg, edaEmpathSocial, hrEmpathTeamAvg, hrEmpathSocial) %>%
  #get_mahalanobis_distance(.,auto_drop = FALSE, re_center = FALSE) %>%
  # filter(p >=.001)
  #mutate(across(c(edaDriverAvg, edaEmpathAvg, hrDriverAvg, hrEmpathAvg), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  mutate(across(c(edaEmpathTeamAvg, edaEmpathSocial, hrEmpathTeamAvg, hrEmpathSocial), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
#lpa_df <- df[c('edaDriverAvg', 'edaEmpathAvg', 'hrDriverAvg', 'hrEmpathAvg')]
lpa_df <- df[c('edaEmpathTeamAvg', 'edaEmpathSocial', 'hrEmpathTeamAvg', 'hrEmpathSocial')]
fit10s <- lpa_df %>%
  single_imputation() %>%
  # estimate_profiles(2:10,
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC")) %>%
  estimate_profiles(7) 
get_estimates(fit10s)
fit10s %>%
  plot_profiles() +
  ggthemes::theme_tufte()
lpa_df <- df %>% bind_cols(get_data(fit10s)) %>%
  mutate(Class = factor(Class))

lpa_df %>%
  ggplot(aes(x = Class, y = Team_Perf_Indiv_Mean, fill = Part_ID)) + geom_boxplot()

library(ordinal)
m1 <- ordinal::clm(Class ~
                     IRI_PerspectiveTaking_Mean +
                     NRI_EmSel +
                     (1|Part_ID),
                   data = lpa_df)
summary(m1)
plot(m1)

library(lme4)
lpa_df <- lpa_df %>%
  #mutate(Team_Perf_Indiv_Mean = Team_Perf_Indiv_Mean / 100) %>%
  drop_na(Team_Perf_Indiv_Mean)
m1 <- lmer(Team_Perf_Indiv_Mean ~ PerspectiveTaking_Mean + CO_Dominance_CO + CO_Affiliation_CO + Class + (1|Part_ID), data = lpa_df)
summary(m1)
sjPlot::plot_model(m1)
sjPlot::tab_model(m1)

library(tidyverse)
df$Team_Perf_Indiv_Mean <- df$Team_Perf_Indiv_Mean / 100
test_df <- df %>%
  dplyr::select(Mission_day,Team_Perf_Indiv_Mean,edaEmpathAvg,hrEmpathAvg,Part_ID) %>%
  drop_na() %>%
  mutate(Team_Perf_Indiv_Mean_0to1  = Team_Perf_Indiv_Mean / 100) #%>%
  # filter(Team_Perf_Indiv_Mean_0to1 <1 )
hist(test_df$Team_Perf_Indiv_Mean_0to1, breaks = 30)
  #filter(Team_Perf_Indiv_Mean <= 1 & Team_Perf_Indiv_Mean >=0)
f1 <- gamlss::fitDist(na.omit(test_df$edaEmpathAvg), type = 'real0to1')
f1$fits
gamlss::histDist(Team_Perf_Indiv_Mean_0to1, family = BEINF1, nbins = 30, data = test_df)
gamlss::histDist(edaEmpathAvg, family = exGAUS, nbins = 30, data = test_df)

library(gamlss)

m.1.gm <- gamlss::gamlss(
  formula = Team_Perf_Indiv_Mean_0to1 ~ edaEmpathAvg + hrEmpathAvg + 
    (Mission_day|Part_ID),
  family = BEINF1, data = test_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000))

summary(m.1.gm)
Rsq(m.1.gm)
plot(m.1.gm)
wp(m.1.gm)



