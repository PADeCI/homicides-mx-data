##******************************************************************************
## Script Name: Wrangle homicide data from sspc fuentes abiertas at state-level
## Purpose:         
## 
##
## Created:             2020-09-07 
## Last update:         2020-09-27
## Authors: Mariana Consuelo Fernandez Espinosa, Regina Isabel Medina
##          
## GitHub: marianafdz465  
##
##
##******************************************************************************


# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(readr)
library(lubridate)
library(httr)
library(repmis)
library(dplyr)
library(magrittr)
library(gapminder)

# Clean the workspace
rm(list = ls()) 


# 01. Load Data ----------------------------------------------------------------

# Population data at state level
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

# Homicides at state level (from open sources)
df_homicides_daily_fuentesabiertas <- read.csv("data_raw/fuentes_abiertas/2019_2020/df_homicides_daily_2019_2020_sspc_fuentesabiertas.csv", 
        encoding = "UTF-8")


# 02. Data wrangling  ----------------------------------------------------------
# 02.1 Relabel, group and select  ----------------------------------------------

# Select columns of interest and rename

df_homicides_daily_state <- df_homicides_daily_fuentesabiertas  %>% 
        select(-Municipio)                                      %>%
        rename(entidad = "Entidad", 
               homicidios = "Homicidios",
               hombre = "Hombre",
               mujer = "Mujer", 
               no_identificado = "No.Identificado",
               fecha = "Fecha")                                 %>% 
        mutate(fecha = as.character(fecha))                     %>%  
        mutate(fecha = case_when(fecha == "08/03/19" ~ "2019-03-08",
                fecha == fecha ~ fecha))

df_homicides_daily_state$fecha <- as.Date(df_homicides_daily_state$fecha) 
df_homicides_daily_state$year <- format(as.Date(df_homicides_daily_state$fecha, format="%Y/%m/%d"),"%Y")
df_homicides_daily_state$year <- as.integer(df_homicides_daily_state$year)

# 02.2 Check for duplicated dates  ---------------------------------------------
        # # There must be as many observations per date as states (33)
        # df_fechas <- as.data.frame(table(df_homicides_daily_state$fecha)) %>% 
        #         rename(fecha = Var1, Freq_prev = Freq) 
        # 
        # # Eliminate duplicated dates 
        # df_homicides_fa_unique_dates <- df_homicides_daily_state        %>% 
        #         group_by(entidad)                                       %>% 
        #         distinct(fecha, .keep_all = TRUE)                       %>% 
        #         ungroup()
        # 
        # # Check that filtering did actually work 
        # df_fechas2 <- as.data.frame(table(df_homicides_fa_unique_dates$fecha)) %>% 
        #         rename(fecha = Var1, Freq_post= Freq) %>% 
        #         full_join(df_fechas, by = c("fecha"))
        #         


# 02.3 Summarise --------------------------------------------------------------
# Group by state
df_homicides_daily_state_summ <- df_homicides_daily_state       %>% 
        group_by(entidad, fecha)                                %>%
        summarise_all(.funs = sum, na.rm=F)


# 02.4 Estimate national levels ------------------------------------------------
df_homicides_national <- df_homicides_daily_state_summ          %>% 
        group_by(fecha)                                         %>% 
        summarise(homicidios = sum(homicidios), 
                  hombre     = sum(hombre), 
                  mujer      = sum(mujer), 
                  no_identificado = sum(no_identificado))       %>%
        mutate(entidad = "Nacional")


# 02.5 Fill omitted dates with 0s  ---------------------------------------------
df_homicides_state_daily_fuentesabiertas <-  df_homicides_daily_state %>% 
        bind_rows(df_homicides_national) %>% 
        complete(fecha, nesting(entidad),
                fill = list(homicidios=0, hombre=0, mujer=0, no_identificado=0))


# 02.6 Add population  ---------------------------------------------------------

# Vectors and relabeling that will be needed 
df_pop_state <- df_pop_state                                    %>%
        rename(state_code = "CVE_GEO")

# Filter year 2019 and 2020
df_pop_2019_2020 <- df_pop_state %>% 
        filter(year == 2019 || year == 2020)

# Create code variable for states
df_homicides_daily <- df_homicides_state_daily_fuentesabiertas  %>% 
        mutate(state_code = case_when(
                entidad == "Aguascalientes" ~ 1,
                entidad == "Baja California" ~ 2,
                entidad == "Baja California Sur" ~ 3,
                entidad == "Campeche" ~ 4,
                entidad == "Coahuila" ~ 5,
                entidad == "Colima" ~ 6,
                entidad == "Chiapas" ~ 7,      
                entidad == "Chihuahua" ~ 8,
                entidad == "Ciudad de México" ~ 9,
                entidad == "Durango" ~ 10,
                entidad == "Guanajuato" ~ 11,
                entidad == "Guerrero" ~ 12,
                entidad == "Hidalgo" ~ 13,
                entidad == "Jalisco" ~ 14,
                entidad == "Estado de México" ~ 15,
                entidad == "Michoacán" ~ 16,
                entidad == "Morelos" ~ 17,
                entidad == "Nacional" ~ 0, 
                entidad == "Nayarit" ~ 18,
                entidad == "Nuevo León"  ~ 19,
                entidad == "Oaxaca" ~ 20,
                entidad == "Puebla" ~ 21,
                entidad == "Querétaro" ~ 22,
                entidad == "Quintana Roo" ~ 23,
                entidad == "San Luis Potosí" ~ 24,
                entidad == "Sinaloa" ~ 25,
                entidad == "Sonora" ~ 26,
                entidad == "Tabasco" ~ 27,
                entidad == "Tamaulipas" ~ 28,
                entidad == "Tlaxcala" ~ 29,
                entidad == "Veracruz" ~ 30,
                entidad == "Yucatán" ~ 31, 
                entidad == "Zacatecas" ~ 32)) 

# Give date format to date variables
df_homicides_daily$fecha <- as.Date(df_homicides_daily$fecha) 
df_homicides_daily$year <- format(as.Date(df_homicides_daily$fecha, format="%Y/%m/%d"),"%Y")
df_homicides_daily$year <- as.integer(df_homicides_daily$year)

df_homicides_daily$fecha<- as.Date(df_homicides_daily$fecha , format = "%Y/%m/%d")

# Join data frames
df_homicides_daily_clean <- df_homicides_daily                          %>% 
        left_join(df_pop_2019_2020, by = c("state_code", "year"))       %>% 
        select(entidad,state,
               fecha, homicidios,
               hombre, mujer,
               no_identificado, population)

# 02.7 Estimate mortality rate -------------------------------------------------

df_final <- df_homicides_daily_clean                                    %>% 
        mutate(mort_rate = round((homicidios/population)*100000, 2))    %>% 
        group_by(state)                                                 %>% 
        unique(by = c("fecha"))                                         %>%
        ungroup()     
        

# 03. Check consistency of data ------------------------------------------------

# 03.1 Totals ------------------------------------------------------------------
# All homicides
sum(df_homicides_daily_fuentesabiertas$Homicidios)              # Original df
sum(df_final$homicidios)                                        # Final df (with national level)

# Male 
sum(df_homicides_daily_fuentesabiertas$Hombre, na.rm = T)       # Original df 
sum(df_final$hombre)                                            # Final df

# Female 
sum(df_homicides_daily_fuentesabiertas$Mujer, na.rm = T)        # Original df
sum(df_final$mujer)                                             # Final df

# Non identified
sum(df_homicides_daily_fuentesabiertas$No.Identificado, na.rm = T) # Original 
sum(df_final$no_identificado)                                   # Final df

        # There are difference in data because of the duplicated dates in August.

# 03.2 Consistency by gender ---------------------------------------------------

# Estimate the number of homicides by adding up by gender. Then compare with 
# the total number of homicides originally reported. Create a variable for 
# differences. 
df_gender <- df_final %>% 
        mutate(suma = hombre + mujer + no_identificado, 
                diff = case_when(homicidios == suma ~ 0, 
                                 homicidios == suma ~ 1)) %>% 
        group_by(entidad, state) 

# Compute the number of observations where reported and estimated total homicides 
# mismatch. 
sum(df_gender$diff, na.rm = T)

        # There are no mismatchs. 

# 03.3 Categories --------------------------------------------------------------
# Make sure NA is not a category on any variable
sum(is.na(df_final$entidad))
sum(is.na(df_final$state))
sum(is.na(df_final$population))
sum(is.na(df_final$fecha))
sum(is.na(df_final$homicidios))

        # None of the variables have a NA category

# 04. Save final data set ------------------------------------------------------
# Rename
df_homicides_state_daily_sspc_fuentesabiertas <- df_final

# Save
save(df_homicides_state_daily_sspc_fuentesabiertas, file = "data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.RData")

