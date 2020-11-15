##******************************************************************************
## Script Name: Make a unified data frame with homicide data from all sources
## Purpose:         
## 
##
## Created:             2020-22-15 
## Last update:         2020-22-22
## Authors: Regina Isabel Medina Rosales
##          
##******************************************************************************

# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(dplyr)

# Clean the workspace
rm(list = ls()) 

# 01. Load Data ----------------------------------------------------------------
# State data by INEGI
load("~/GitHub/homicides-mx-data/data/inegi/df_homicides_state_monthly_inegi_ocurrencia.Rdata")
load("~/GitHub/homicides-mx-data/data/inegi/df_homicides_state_monthly_inegi_registro.Rdata")


# State data by SSCP interinstitutional group
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata")

# State data by SSCP open souces 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_monthly_sspc_fuentesabiertas.RData")

# 02. Data wrangling  ----------------------------------------------------------
# Reestimate mortality rate and create label for each source 
df_inegi_ocurr <- df_homicides_state_monthly_inegi_ocurrencia                 %>% 
        mutate(mort_rate = round(homicidios*100/population, 2), 
                source = "INEGI_OCURRENCE")                              %>% 
        mutate(año = as.factor(año), year = as.factor(year))            %>% 
        select(source, entidad, state,  año, year, mes, month, population,
                homicidios, mort_rate)

df_inegi_regis <- df_homicides_state_monthly_inegi_registro                   %>% 
        mutate(mort_rate = round(homicidios*100/population, 2), 
                source = "INEGI_REGISTER")                              %>% 
        mutate(año = as.factor(año), year = as.factor(year))            %>% 
        select(source, entidad, state,  año, year, mes, month, population,
                homicidios, mort_rate)


df_inter <- df_homicides_state_monthly_sspc_gpointerinstitucional       %>% 
        mutate(mort_rate = round(homicidios*100/population, 2), 
                source = "SSCP_GPO_INTER")                              %>% 
        mutate(año = as.factor(año), year = as.factor(year))            %>% 
        select(source, entidad, state, año, year, mes, month, population,
                homicidios, mort_rate)

df_open  <- df_homicides_state_monthly_sspc_fuentesabiertas             %>% 
        mutate(mort_rate = round(homicidios*100/population, 2), 
                source = "SSCP_OPEN_SOURCE")                            %>% 
        mutate(año = as.factor(año), year = as.factor(year))            %>% 
        select(source, entidad, state, año, year, mes, month, population, 
                homicidios, mort_rate)

# Combine all sources in long format 
df_combined_long <- df_inegi_regis      %>% 
        bind_rows(df_inegi_ocurr)       %>% 
        bind_rows(df_inter)             %>% 
        bind_rows(df_open)              %>% 
        mutate(entidad = as.factor(entidad), state = as.factor(state))  

        
# Combine all sources in wide format 
df_combined_wide <- df_inegi_regis                                           %>%
        select(-source)                                                      %>% 
        rename(homicidios_inegi_register = homicidios, mort_rate_inegi_register = mort_rate)   %>% 
        full_join(df_inegi_ocurr, by = c("entidad", "state", "population", "año", "year", "mes", "month")) %>% 
        select(-source) %>% 
        rename(homicidios_inegi_ocurrence = homicidios, mort_rate_inegi_ocurrence = mort_rate) %>% 
        full_join(df_inter, by = c("entidad", "state", "population", "año", "year", 
                "mes", "month"))                                             %>% 
        select(-source)                                                      %>% 
        rename(homicidios_sscp_gpo = homicidios, mort_rate_sspc_gpo = mort_rate) %>% 
        full_join(df_open, by = c("entidad", "state", "population", "año", "year", 
                "mes", "month"))                                             %>% 
        select(-source)                                                      %>% 
        rename(homicidios_sscp_fa = homicidios, mort_rate_sscp_fa = mort_rate) %>%
        mutate(entidad = as.factor(entidad), state = as.factor(state))       %>% 
        select(entidad, state, año, year, mes, month, population, 
                homicidios_inegi_register,  
                homicidios_inegi_ocurrence, 
                homicidios_sscp_gpo, 
                homicidios_sscp_fa, 
                mort_rate_inegi_register, 
                mort_rate_inegi_ocurrence,
                mort_rate_sspc_gpo, 
                mort_rate_sscp_fa)

# 02. Save data ----------------------------------------------------------------
# Rename 
df_homicides_state_month_all_sources_long <- df_combined_long
df_homicides_state_month_all_sources_wide <- df_combined_wide

## Pendientes: 
        # Estimar el nacional para fuentes abiertas 
        # Estimar el total anual para ambas SSCP 

# Save 
save(df_homicides_state_month_all_sources_long, 
        file = "data/df_homicides_state_month_all_sources_long.RData")

save(df_homicides_state_month_all_sources_wide, 
        file = "data/df_homicides_state_month_all_sources_wide.RData")
