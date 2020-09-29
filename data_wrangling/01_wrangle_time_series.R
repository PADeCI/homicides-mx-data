################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Create weekly and monthly data frames for state and county level   ##
#    Authors: Regina Isabel Medina and Mariana Fernández                      ##
#    Date: September 7th, 2020                                                ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

# Clean the workspace
rm(list = ls()) 


# 01. Load Data ----------------------------------------------------------------

# Homicide data with population at county level 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_fuentesabiertas_county_day.Rdata")

# Homicide data with population at state level 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.RData")

        # View(df_homicides_fuentesabiertas_county_day)
        # View(df_homicides_state_daily_sspc_fuentesabiertas)

# 02. Create new df for each time unit -----------------------------------------

# 02.1 Data frames at county level ---------------------------------------------
# Create time variables and rename df for simplicity 
df_dates_county <- df_homicides_fuentesabiertas_county_day %>% 
        mutate("año" = year(fecha), "year" = year(fecha),
                "mes" = month(fecha), "month" = month(fecha), 
                "semana" = isoweek(fecha), "week"= isoweek(fecha)) 
## Monthly ##
df_month_county <- df_dates_county %>%
        group_by(entidad, municipio, county, year, año, month, mes) %>% 
        summarise("homicidios" = sum(homicidios, na.rm = T), 
                  "hombre"     = sum(hombre, na.rm = T), 
                  "mujer"      = sum(mujer, na.rm = T), 
                  "no_identificado" = sum(no_identificado, na.rm = T)) 
## Weekly ##
df_week_county <- df_dates_county %>% 
        group_by(entidad, municipio, year, año, month, mes, week, semana) %>% 
        summarise("homicidios" = sum(homicidios, na.rm = T), 
                  "hombre"     = sum(hombre, na.rm = T), 
                  "mujer"      = sum(mujer, na.rm = T),
                  "no_identificado" = sum(no_identificado, na.rm = T))

## Replace 0s when NAs are needed (before data by gender was reported)
## First report on gender was done on 2019-02-01
# Male 
df_month_county$hombre[df_month_county$month == 1 & df_month_county$year == 2019] <- NA
df_week_county$hombre[df_week_county$month == 1 & df_week_county$year == 2019] <- NA

# Female 
df_month_county$mujer[df_month_county$month == 1 & df_month_county$year == 2019] <- NA
df_week_county$mujer[df_week_county$month == 1 & df_week_county$year == 2019] <- NA

# Non identified
df_month_county$no_identificado[df_month_county$month == 1 & df_month_county$year == 2019] <- NA
df_week_county$no_identificado[df_week_county$month == 1 & df_week_county$year == 2019] <- NA

# 02.2 Data frames at state level ----------------------------------------------
# Create time variables and rename df for simplicity 
df_dates_state <- df_homicides_state_daily_sspc_fuentesabiertas %>% 
        mutate("año" = year(fecha), "year" = year(fecha),
                "mes" = month(fecha), "month" = month(fecha), 
                "semana" = isoweek(fecha), "week"= isoweek(fecha)) 

## Monthly ## 
df_month_state <- df_dates_state %>% 
        group_by(entidad, state, year, año, mes, month) %>% 
        summarise("homicidios" = sum(homicidios, na.rm = T), 
                "hombre"     = sum(hombre, na.rm = T), 
                "mujer"      = sum(mujer, na.rm = T), 
                "no_identificado" = sum(no_identificado, na.rm = T)) 

## Weekly ##
df_week_state <- df_dates_state %>% 
        group_by(entidad, state, year, año, month, mes, week, semana) %>% 
        summarise("homicidios" = sum(homicidios, na.rm = T), 
                "hombre"     = sum(hombre, na.rm = T), 
                "mujer"      = sum(mujer, na.rm = T),
                "no_identificado" = sum(no_identificado, na.rm = T))
        
## Replace 0s when NAs are needed (before data by gender was reported)
## First report on gender was done on 2019-02-01

# Male
df_month_state$hombre[df_month_state$month == 1 & df_month_state$year == 2019] <- NA
df_week_state$hombre[df_week_state$month == 1 & df_week_state$year == 2019] <- NA

# Female 
df_month_state$mujer[df_month_state$month == 1 & df_month_state$year == 2019] <- NA
df_week_state$mujer[df_week_state$month == 1 & df_week_state$year == 2019] <- NA

# Non identified
df_month_state$no_identificado[df_month_state$month == 1 & df_month_state$year == 2019] <- NA
df_week_state$no_identificado[df_week_state$month == 1 & df_week_state$year == 2019] <- NA

# 03. Check consistency of data ------------------------------------------------

# Total homicides: 48,343 in all 8 data frames 
# County level
sum(df_homicides_fuentesabiertas_county_day$homicidios)
sum(df_dates_county$homicidios)
sum(df_month_county$homicidios)
sum(df_week_county$homicidios)
 
# State level
sum(df_homicides_state_daily_sspc_fuentesabiertas$homicidios)
sum(df_dates_state$homicidios)
sum(df_week_state$homicidios)
sum(df_month_state$homicidios)
       

# Men: 35,377 in all 8 data frames 
# County level 
sum(df_homicides_fuentesabiertas_county_day$hombre, na.rm = T)
sum(df_dates_county$hombre, na.rm = T)
sum(df_month_county$hombre, na.rm = T)
sum(df_week_county$hombre, na.rm = T)

# State level 
sum(df_homicides_fuentesabiertas_county_day$hombre, na.rm = T)
sum(df_dates_state$hombre, na.rm = T)
sum(df_week_state$hombre, na.rm = T)
sum(df_month_state$hombre, na.rm = T)


# Women: 4,915 in all 8 data frames  
# County level 
sum(df_homicides_fuentesabiertas_county_day$mujer, na.rm = T)
sum(df_dates_county$mujer, na.rm = T)
sum(df_month_county$mujer, na.rm = T)
sum(df_week_county$mujer, na.rm = T)

# State level 
sum(df_homicides_fuentesabiertas_county_day$mujer, na.rm = T)
sum(df_dates_state$mujer, na.rm = T)
sum(df_week_state$mujer, na.rm = T)
sum(df_month_state$mujer, na.rm = T)


# Non identified gender: 5,777 in all 8 data frames 
# County level
sum(df_homicides_fuentesabiertas_county_day$no_identificado, na.rm = T)
sum(df_dates_county$no_identificado, na.rm = T)
sum(df_month_county$no_identificado, na.rm = T)
sum(df_week_county$no_identificado, na.rm = T)

# State level
sum(df_homicides_fuentesabiertas_county_day$no_identificado, na.rm = T)
sum(df_dates_state$no_identificado, na.rm = T)
sum(df_week_state$no_identificado, na.rm = T)
sum(df_month_state$no_identificado, na.rm = T)



# 04. Save final data sets -----------------------------------------------------

## Rename data ##
df_homicides_county_daily_sspc_fuentesabiertas <- df_dates_county
df_homicides_county_weekly_sspc_fuentesabiertas <- df_week_county
df_homicides_county_monthly_sspc_fuentesabiertas <- df_month_county

df_homicides_state_daily_sspc_fuentesabiertas <- df_dates_state
df_homicides_state_weekly_sspc_fuentesabiertas <- df_week_state
df_homicides_state_monthly_sspc_fuentesabiertas <- df_month_state


## Save data ##
# County level 
save(df_homicides_county_daily_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")
save(df_homicides_county_weekly_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_weekly_sspc_fuentesabiertas.RData")
save(df_homicides_county_monthly_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_monthly_sspc_fuentesabiertas.RData")

# Sate level 
save(df_homicides_state_daily_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.RData")
save(df_homicides_state_weekly_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_weekly_sspc_fuentesabiertas.RData")
save(df_homicides_state_monthly_sspc_fuentesabiertas, file = "~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_monthly_sspc_fuentesabiertas.RData")

