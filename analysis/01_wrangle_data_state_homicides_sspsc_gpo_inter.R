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
library(beepr)

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
        summarise(population = sum(population))                         %>% 
        group_by(año)                                                   %>% 
        bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Nacional"))) %>% 
        mutate(state = case_when(entidad=="Nacional" ~ "National", 
                                 entidad==entidad~state))

# Rename homicide data frame
df_homicides_gpo <- df_homicides                                        %>% 
        rename("entidad" = Entidad, 
                "homicidios" = Homicidios,
                "fecha" = Fecha)                                        %>% 
        filter(entidad!="Total")                                        %>% 
        mutate(entidad = as.character(entidad))                         %>% 
        group_by(fecha)                                                 %>% 
        bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Nacional"))) %>% 
        ungroup() %>% 
        mutate(id = 1:19034)

df_mis <- df_homicides_gpo %>% 
        filter(is.na(homicidios)==T)

df_compare <- df_homicides %>% 
        rename("entidad" = Entidad, 
                "homicidios" = Homicidios,
                "fecha" = Fecha) %>% 
        mutate(entidad = as.character(entidad)) %>% 
        mutate(entidad = case_when(entidad == "Total" ~ "Nacional", 
                entidad == entidad ~ entidad)) %>% 
        mutate(id = 1:19034) %>% 
        left_join(df_homicides_gpo, by = c("id", "entidad")) %>% 
        mutate(equal = (homicidios.x == homicidios.y))

df_mistakes <- df_compare %>% 
        filter(is.na(equal)==T)
        

# Check for duplicated dates: there must be as many observations per date as states (33)
df_fechas <- as.data.frame(table(df_homicides_gpo$fecha)) 

# Eliminate duplicated dates 
df_homicides_gpo_unique_dates <- df_homicides_gpo       %>% 
        group_by(entidad)                               %>% 
        distinct(fecha, .keep_all = TRUE)               %>% 
        ungroup()

# Check that filtering did actually work 
df_fechas2 <- as.data.frame(table(df_homicides_gpo_unique_dates$fecha)) 

# 02.2 Fill missing dates  -----------------------------------------------------
# Convert implicit missing values to explicit missing values:
# we will have data for every date.

# df_homicides_gpo_all <- df_homicides_gpo                        %>% 
#         complete(fecha, nesting(entidad), 
#                         fill = list(homicidios=0))              %>%
#         mutate(fecha = as.Date(fecha))                          %>% 
#         distinct(entidad, fecha, .keep_all = T)      
# beepr::beep(5)
# 
# df_post <- as.data.frame(table(df_homicideS_gpo_all$entidad))
# df_pre <- as.data.frame(table(df_homicides_gpo$entidad))

# 02.3 Add population for interinstitutional group  ----------------------------

# Join data frames
df_homicides_state <- df_homicides_gpo_unique_dates             %>% 
        mutate(año = year(as.Date(fecha)))                      %>%
        mutate(fecha = as.Date(fecha))                          %>% 
        left_join(df_pop_state, by = c("entidad", "año")) 


# 02.4 Estimate mortality rate -------------------------------------------------

# Estimate number of homicides per 100,000 people

df_homicides_state_daily <- df_homicides_state                          %>% 
        mutate(mort_rate = (homicidios*100000/population))              %>%
        group_by(state)                                                 %>% 
        unique(by = c("fecha"))                                         %>% 
        ungroup()                                                       %>% 
        select(entidad, state, año, fecha, homicidios, population, mort_rate) 


# 03. Check consistency of data ------------------------------------------------

sum(df_homicides$Homicidios, na.rm = T)
sum(df_homicides_gpo$homicidios)
sum(df_homicides_gpo$homicidios[df_homicides_gpo$entidad!="Nacional"])

sum(df_homicides_state_daily$homicidios)
sum(df_homicides_state_daily$homicidios[df_homicides_state$entidad!="Nacional"])

        # The numbers are different because the original data frame had all the 
        # dates from January 2020 duplicated.

beepr::beep(5)

# 04. Save final data sets -----------------------------------------------------

# Rename df
df_homicides_state_daily_sspc_gpointerinstitucional <- df_homicides_state_daily

# Save df
save(df_homicides_state_daily_sspc_gpointerinstitucional, file = "data/gpo_interinstitucional/df_homicides_state_daily_sspc_gpointerinstitucional.RData")
