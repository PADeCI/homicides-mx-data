#------------------------------------------------------------------------------#
# 
# Goal:   Create new homicide data frame for 2019 
# Author: Regina Isabel Medina Rosales
# Date:   December 2nd
#
#------------------------------------------------------------------------------#

# 0. Initial set up ------------------------------------------------------------
# Load libraries 
library(dplyr)

# Clean workspace 
rm(list=ls())

# 1. Load data -----------------------------------------------------------------
load("data/inegi/df_homicides_state_monthly_inegi_mortality.RData")

# 2. Data clean up -------------------------------------------------------------
# Create annual data frame 
df_2019_state <- df_homicides_state_monthly_inegi_mortality     %>% 
        mutate(year = 2019)                                     %>% 
        rename(cases = homicidios)                              %>%
        filter(state != "National")                             %>% 
        group_by(state, year)                                   %>% 
        summarise(cases = sum(cases))                           %>% 
        dplyr::select(year, state, cases)

df_2019_national <- df_homicides_state_monthly_inegi_mortality     %>% 
        mutate(year = 2019)                                     %>% 
        rename(cases = homicidios)                              %>%
        filter(state == "National")                             %>% 
        group_by(state, year)                                   %>% 
        summarise(cases = sum(cases))                           %>% 
        dplyr::select(year, state, cases)

        
# Save data set ----------------------------------------------------------------
save(df_2019_state, file = "data/inegi/df_2019_state.RData")
save(df_2019_state, file = "data/inegi/df_2019_national.RData")
