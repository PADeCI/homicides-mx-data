##******************************************************************************
## Script Name: Wrangle homicide data from sspc fuentes abiertas at state-level
## Purpose:         
## 
##
## Created:             2020-10-15 
## Last update:         2020-10-22
## Authors:             Regina Isabel Medina Rosales
##          
##******************************************************************************

# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)      # Handle data
library(readr)          # Handle tibbles
library(lubridate)      # Handle dates
library(foreign)        # Import dbf files
library(repmis)         # Import web files 
library(dplyr)          # clean data
library(forcats)        # Manage categories

# Clean the workspace
rm(list = ls()) 

# 01. Load Data ----------------------------------------------------------------

# Population data at state level
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

# Deaths
df_deaths_raw <- read.dbf("data_raw/inegi/mortalidad/DEFUN19.DBF") 
        # View(df_deaths)

# 02. Clean Data ---------------------------------------------------------------
# 02.1  Create name vectors ----------------------------------------------------
# Vector of ICD-10 codes that correspond to homicide
v_homicides <- c('X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X91', 'X92', 'X93',
                 'X94', 'X95', 'X96', 'X97', 'X98', 'X99', 'Y00', 'Y01', 'Y02', 
                 'Y03', 'Y04', 'Y05', 'Y06', 'Y07', 'Y08', 'Y09')


# Vector of names of States in Spanish
v_names_states_esp <- c("Aguascalientes", "Baja California", 
                        "Baja California Sur", "Campeche", "Coahuila", "Colima", 
                        "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                        "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
                        "Estado de México", "Michoacán", "Morelos", "Nayarit", 
                        "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                        "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
                        "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
                        "Yucatán", "Zacatecas", "Desconocido")

# Vector of names of States in English
v_names_states_eng <- c("Aguascalientes", "Baja California", 
                        "Baja California Sur", "Campeche", "Coahuila", "Colima", 
                        "Chiapas", "Chihuahua", "Mexico City", "Durango", 
                        "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
                        "State of Mexico", "Michoacan", "Morelos", "Nayarit", 
                        "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro", 
                        "Quintana Roo", "San Luis Potosi", "Sinaloa", "Sonora", 
                        "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
                        "Yucatan", "Zacatecas", "Unknown")

v_months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December", "Unkown")

v_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
             "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre", "Desconocido")


# 02.2 Relabel raw variables ---------------------------------------------------
df_deaths <- df_deaths_raw              %>% 
        rename(cause  = "CAUSA_DEF", 
                sex   = "SEXO", 
                age   = "EDAD", 
                year  = "ANIO_OCUR", 
                month = "MES_OCURR",
                state = "ENT_OCURR", 
                mun   = "MUN_OCURR")    %>% 
        dplyr::select(cause, sex, age, year, month, state, mun)

# Create dataframe of homicide deaths by year, sex, and age
df_homicides <- df_deaths               %>%
        filter(str_detect(cause, paste(v_homicides, collapse = "|")),
                age != 998)

nrow(df_homicides) ##36661

# Relabel values for state and month variable 
df_homicides_labeled <- df_homicides %>% 
        mutate(id_state = state, month_num = month) %>% 
        mutate(state = case_when(state == "01" ~ v_names_states_eng[1], 
                                 state == "02" ~ v_names_states_eng[2],
                                 state == "03" ~ v_names_states_eng[3],
                                 state == "04" ~ v_names_states_eng[4],
                                 state == "05" ~ v_names_states_eng[5],
                                 state == "06" ~ v_names_states_eng[6],
                                 state == "07" ~ v_names_states_eng[7],
                                 state == "08" ~ v_names_states_eng[8],
                                 state == "09" ~ v_names_states_eng[9],
                                 state == "10" ~ v_names_states_eng[10],
                                 state == "11" ~ v_names_states_eng[11],
                                 state == "12" ~ v_names_states_eng[12],
                                 state == "13" ~ v_names_states_eng[13],
                                 state == "14" ~ v_names_states_eng[14],
                                 state == "15" ~ v_names_states_eng[15],
                                 state == "16" ~ v_names_states_eng[16],
                                 state == "17" ~ v_names_states_eng[17],
                                 state == "18" ~ v_names_states_eng[18],
                                 state == "19" ~ v_names_states_eng[19],
                                 state == "20" ~ v_names_states_eng[20],
                                 state == "21" ~ v_names_states_eng[21],
                                 state == "22" ~ v_names_states_eng[22],
                                 state == "23" ~ v_names_states_eng[23],
                                 state == "24" ~ v_names_states_eng[24],
                                 state == "25" ~ v_names_states_eng[25],
                                 state == "26" ~ v_names_states_eng[26],
                                 state == "27" ~ v_names_states_eng[27],
                                 state == "28" ~ v_names_states_eng[28],
                                 state == "29" ~ v_names_states_eng[29],
                                 state == "30" ~ v_names_states_eng[30],
                                 state == "31" ~ v_names_states_eng[31],
                                 state == "32" ~ v_names_states_eng[32],
                                 state == "99" ~ v_names_states_eng[33])) %>% 
        mutate(entidad = case_when(id_state == "01" ~ v_names_states_eng[1], 
                                id_state == "02" ~ v_names_states_esp[2],
                                id_state == "03" ~ v_names_states_esp[3],
                                id_state == "04" ~ v_names_states_esp[4],
                                id_state == "05" ~ v_names_states_esp[5],
                                id_state == "06" ~ v_names_states_esp[6],
                                id_state == "07" ~ v_names_states_esp[7],
                                id_state == "08" ~ v_names_states_esp[8],
                                id_state == "09" ~ v_names_states_esp[9],
                                id_state == "10" ~ v_names_states_esp[10],
                                id_state == "11" ~ v_names_states_esp[11],
                                id_state == "12" ~ v_names_states_esp[12],
                                id_state == "13" ~ v_names_states_esp[13],
                                id_state == "14" ~ v_names_states_esp[14],
                                id_state == "15" ~ v_names_states_esp[15],
                                id_state == "16" ~ v_names_states_esp[16],
                                id_state == "17" ~ v_names_states_esp[17],
                                id_state == "18" ~ v_names_states_esp[18],
                                id_state == "19" ~ v_names_states_esp[19],
                                id_state == "20" ~ v_names_states_esp[20],
                                id_state == "21" ~ v_names_states_esp[21],
                                id_state == "22" ~ v_names_states_esp[22],
                                id_state == "23" ~ v_names_states_esp[23],
                                id_state == "24" ~ v_names_states_esp[24],
                                id_state == "25" ~ v_names_states_esp[25],
                                id_state == "26" ~ v_names_states_esp[26],
                                id_state == "27" ~ v_names_states_esp[27],
                                id_state == "28" ~ v_names_states_esp[28],
                                id_state == "29" ~ v_names_states_esp[29],
                                id_state == "30" ~ v_names_states_esp[30],
                                id_state == "31" ~ v_names_states_esp[31],
                                id_state == "32" ~ v_names_states_esp[32],
                                id_state == "99" ~ v_names_states_esp[33])) %>% 
        mutate(month = case_when(month == "1" ~ v_months[1],
                                 month == "2" ~ v_months[2],
                                 month == "3" ~ v_months[3],
                                 month == "4" ~ v_months[4],
                                 month == "5" ~ v_months[5],
                                 month == "6" ~ v_months[6],
                                 month == "7" ~ v_months[7],
                                 month == "8" ~ v_months[8],
                                 month == "9" ~ v_months[9],
                                 month == "10" ~ v_months[10],
                                 month == "11" ~ v_months[11],
                                 month == "12" ~ v_months[12],
                                 month == "99" ~ v_months[13])) %>% 
        mutate(mes = case_when(month_num == "1" ~ v_meses[1],
                                month_num == "2" ~ v_meses[2],
                                month_num == "3" ~ v_meses[3],
                                month_num == "4" ~ v_meses[4],
                                month_num == "5" ~ v_meses[5],
                                month_num == "6" ~ v_meses[6],
                                month_num == "7" ~ v_meses[7],
                                month_num == "8" ~ v_meses[8],
                                month_num == "9" ~ v_meses[9],
                                month_num == "10" ~ v_meses[10],
                                month_num == "11" ~ v_meses[11],
                                month_num == "12" ~ v_meses[12],
                                month_num == "99" ~ v_meses[13]))


# 02.3  Summarise total homicides by state and month  --------------------------

df_homicides_state <- df_homicides_labeled                      %>% 
        group_by(state, entidad, month, mes)                    %>% 
        summarise(homicidios = n())                   

df_homicides_national <- df_homicides_state                     %>%
        group_by(month, mes)                                    %>% 
        summarise_at(c("homicidios"), sum, na.rm = T)           %>%
        mutate(state = "National", entidad = "Nacional")        %>% 
        dplyr::select(state, entidad, month, mes, homicidios)
       

df_homicides_state_pop <- df_homicides_state                    %>%
        bind_rows(df_homicides_national)                        %>% 
        mutate(year = 2019)                                     %>% 
        left_join(df_pop_state, by = c("year", "state"))        %>% 
        rename(id_state  = CVE_GEO)                             %>%  
        mutate(mort_rate = homicidios*100000/population, 
                year     = 2019, año = 2019)                    %>% 
        mutate(mes = factor(mes, levels = c(v_meses, "Total")), 
                month = factor(month, levels = c(v_months, "Total")))   %>% 
        dplyr::select(entidad, state, id_state, population, año, year, 
                        mes, month, homicidios, mort_rate)
                
# 03. Save final data set ------------------------------------------------------
# Rename
df_homicides_state_monthly_inegi_mortality <- df_homicides_state_pop

# Save 
save(df_homicides_state_monthly_inegi_mortality, file = "data/inegi/df_homicides_state_monthly_inegi_mortality.RData")

