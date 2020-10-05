################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at state level (gpo.interinst.)        ##
#                                                                             ##
#    Goal: Add population data and estimate mortality rate                    ##
#    Authors: Regina Isabel Medina and Mariana Consuelo Fernandez Espinosa    ##
#    Date:               2020-09-07                                           ##
#    Last update:        2020-09-27                                           ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(repmis) # For exporting data from GitHub

# Clean the workspace
rm(list = ls()) 



# 01. Load Data ----------------------------------------------------------------

# Population data at county level 
load("~/GitHub/homicides-mx-data/data_raw/df_pop_county_2019_2020.Rdata")

# Homicide data from interinstitutional group
df_homicides <- read.csv("data_raw/gpo_interinstitucional/2019_2020/df_homicides_daily_2019_2020_gpointerinstitucional.csv", 
        encoding = "UTF-8")




# 02. Data wrangling  ----------------------------------------------------------

# 02.1 Relabel, filter,  group and select  -------------------------------------

# Change variable names for compatibility, eliminate non-identified counties 
df_pop <- df_pop_county_2019_2020                                       %>%
        rename(entidad    = entidad,    
                municipio = county_name_esp,    
                county    = county_name_eng,    
                año       = year)                                       %>%
        mutate(entidad = case_when(entidad =="México" ~ "Estado de México", 
                        entidad == entidad ~ entidad))                  %>%
        filter(entidad != is.na(entidad))                               %>% 
        mutate(population = as.numeric(population))     

# Create state level population data frame
df_pop_state <- df_pop                                                  %>% 
        group_by(entidad, state, año)                                   %>%
        summarise(population = sum(population))

# Rename homicide data frame
df_homicides_gpo <- df_homicides                                        %>% 
        rename("entidad" = Entidad, 
                "homicidios" = Homicidios,
                "fecha" = Fecha)                                        %>% 
        mutate(entidad = as.character(entidad))                         %>% 
        mutate(entidad = case_when(entidad == "Total" ~ "Nacional", 
                                   entidad == entidad ~ entidad))

# 02.2 Add population for interinstitutional group  ----------------------------

# Join data frames
df_homicides_state <- df_homicides_gpo                                  %>% 
        mutate(año = year(as.Date(fecha)))                              %>% 
        left_join(df_pop_state, by = c("entidad", "año")) 


# 02.3 Estimate mortality rate and fill missing dates  -------------------------

# Estimate number of homicides per 100,000 people, convert implicit missing 
# values to explicit missing values:we will have data for every date.

df_homicides_state_daily <- df_homicides_state                          %>% 
        mutate(mort_rate = (homicidios*100000/population))              %>% 
        complete(fecha, nesting(entidad), 
                fill = list(homicidios =0))                             %>%
        mutate(fecha = as.Date(fecha))                                  %>% 
        select(entidad, state, año, fecha, homicidios, population, mort_rate)



# 03. Check consistency of data ------------------------------------------------

sum(df_homicides$Homicidios, na.rm = T)
sum(df_homicides_state_daily$homicidios)


# 04. Save final data sets -----------------------------------------------------

# Rename df
df_homicides_state_daily_sspc_gpointerinstitucional <- df_homicides_state_daily

# Save df
save(df_homicides_state_daily_sspc_gpointerinstitucional, file = "data/gpo_interinstitucional/df_homicides_state_daily_sspc_gpointerinstitucional.RData")
