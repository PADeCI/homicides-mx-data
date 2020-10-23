##******************************************************************************
## Script Name: Wrangle homicide data from sspc fuentes abiertas at state-level
## Purpose:         
## 
##
## Created:             2020-10-15 
## Last update:         2020-10-22
## Authors: }Regina Isabel Medina Rosales
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
library(stringr)

# Clean the workspace
rm(list = ls()) 

# 01. Load Data ----------------------------------------------------------------

# Population data at state level
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

# Homicides at state level (from INEGI by month of register)
df_ocurr_00 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_00-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_01 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_01-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_02 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_02-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_03 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_03-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_04 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_04-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_05 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_05-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_06 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_06-2019_inegi_ocurrencia.csv", encoding = "Latin-1")
df_ocurr_07 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_07-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_08 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_08-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_09 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_09-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_10 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_10-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_11 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_11-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_12 <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_12-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)
df_ocurr_NE <- read.csv("data_raw/inegi/mes_ocurrencia/df_homicides_NE-2019_inegi_ocurrencia.csv", encoding = "Latin-1", skip = 5)

# Homicides at state level (from INEGI by month of ocurrence)
df_regis_00 <- read.csv("data_raw/inegi/mes_registro/df_homicides_00-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_01 <- read.csv("data_raw/inegi/mes_registro/df_homicides_01-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_02 <- read.csv("data_raw/inegi/mes_registro/df_homicides_02-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_03 <- read.csv("data_raw/inegi/mes_registro/df_homicides_03-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_04 <- read.csv("data_raw/inegi/mes_registro/df_homicides_04-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_05 <- read.csv("data_raw/inegi/mes_registro/df_homicides_05-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_06 <- read.csv("data_raw/inegi/mes_registro/df_homicides_06-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_07 <- read.csv("data_raw/inegi/mes_registro/df_homicides_07-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_08 <- read.csv("data_raw/inegi/mes_registro/df_homicides_08-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_09 <- read.csv("data_raw/inegi/mes_registro/df_homicides_09-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_10 <- read.csv("data_raw/inegi/mes_registro/df_homicides_10-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_11 <- read.csv("data_raw/inegi/mes_registro/df_homicides_11-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)
df_regis_12 <- read.csv("data_raw/inegi/mes_registro/df_homicides_12-2019_inegi_registro.csv", encoding = "Latin-1", skip = 4)

# 02. Data wrangling  ----------------------------------------------------------
v_months <- month.name
v_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# 02.1 Population  -------------------------------------------------------------
df_pop <- df_pop_state %>% 
        rename(id_state = CVE_GEO) %>% 
        filter(year == 2019)

# 02.2 Registered month  -------------------------------------------------------
df_clean_00 <- df_regis_00      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "Total", 
                mes = "Total", 
                id_state = 0:32)

df_clean_01 <- df_regis_01      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "January", 
                mes = "Enero", 
                id_state = 0:32)

df_clean_02 <- df_regis_02      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "February", 
                mes = "Febrero", 
                id_state = 0:32)

df_clean_03 <- df_regis_03      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "March", 
                mes = "Marzo", 
                id_state = 0:32)

df_clean_04 <- df_regis_04      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "April", 
                mes = "Abril", 
                id_state = 0:32)

df_clean_05 <- df_regis_05      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "May", 
                mes = "Mayo", 
                id_state = 0:32)

df_clean_06 <- df_regis_06      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "June", 
                mes = "Junio", 
                id_state = 0:32)

df_clean_07 <- df_regis_07      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "July", 
                mes = "Julio", 
                id_state = 0:32)

df_clean_08 <- df_regis_08      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "August", 
                mes = "Agosto", 
                id_state = 0:32)

df_clean_09 <- df_regis_09      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "September", 
                mes = "Septiembre", 
                id_state = 0:32)

df_clean_10 <- df_regis_10      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "October", 
                mes = "Octubre", 
                id_state = 0:32)

df_clean_11 <- df_regis_11      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "November", 
                mes = "Noviembre", 
                id_state = 0:32)

df_clean_12 <- df_regis_12      %>% 
        rename(year=X)          %>% 
        filter(year==2019)      %>% 
        select(-"X.1")          %>% 
        gather(key = "year")    %>% 
        mutate(month = "December", 
                mes = "Diciembre", 
                id_state = 0:32)

# Joined dfs
df_register_joined_all_months <- df_clean_00                            %>% 
        bind_rows(df_clean_01, df_clean_02, df_clean_03, df_clean_04, 
                  df_clean_05, df_clean_06, df_clean_07, df_clean_08, 
                  df_clean_09, df_clean_10, df_clean_11, df_clean_12) 
        
# Create  and rename vars and values
df_register_corrected_vars <- df_register_joined_all_months             %>%
        rename(homicidios = value, 
                entidad = year)                                         %>%
        mutate(entidad = case_when(entidad == "Total" ~ "Nacional", 
                        entidad == "Coahuila.de.Zaragoza" ~ "Coahuila",
                        entidad == "Michoacán.de.Ocampo" ~ "Michoacán", 
                        entidad == "Veracruz.de.Ignacio.de.la.Llave" ~ "Veracruz", 
                        entidad == "México" ~ "Estado de México", 
                        entidad == entidad ~ entidad))  %>%
        mutate(entidad = str_replace_all(entidad, "\\.", " "), 
                homicidios = str_remove_all(homicidios, "\\,"),
                homicidios = as.numeric(homicidios))                    %>% 
        mutate(year = 2019, año = 2019, 
                homicidios = as.integer(homicidios)) 
# Pendiente: Necesito sacar los números de homicidios sin la coma

# Add population and estimate mortality rate 
df_register_pop <- df_register_corrected_vars                           %>% 
        left_join(df_pop, by = c("id_state", "year"))                   %>% 
        mutate(mort_rate = round(homicidios*100000/population, 2))      %>% 
        mutate(mes = factor(mes, levels = c(v_meses, "Total")), 
                month = factor(month, levels = c(v_months, "Total")))   %>% 
        select("entidad", "state", "population", "año", "year", "mes", "month", 
                "homicidios", "mort_rate")

# 02.3 Ocurrence month  --------------------------------------------------------

 
# 03. Check consistency of data ------------------------------------------------
# 04. Save final data set ------------------------------------------------------
# Rename 
df_homicides_state_monthly_inegi_ocurrencia <- df_register_pop

# Save
save(df_homicides_state_monthly_inegi_ocurrencia, file = "data/inegi/df_homicides_state_monthly_inegi_ocurrencia.Rdata")

