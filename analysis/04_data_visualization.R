################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Create visualizations for homicide data                            ##
#    Authors: Regina Isabel Medina Rosales                                    ##
#    Date: October 13th, 2020                                                 ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------
# Load libraries
library(dplyr)
library(ggplot2)

# Clean workspace 
rm(list=ls())

# Color blind firendly palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# 01. Load Data ----------------------------------------------------------------
# Total cases by state and month
load("./data/gpo_interinstitucional/df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata")

# Rename data frame for simplicity 
df_month <- df_homicides_state_monthly_sspc_gpointerinstitucional %>%  
        mutate(month_year = paste(month, year), 
                mes_year = paste(mes, "\n", year, sep="")) %>% 
        arrange(year, month)

df_month_national <- df_month %>%
         filter(state == "National") 

df_month_national_5m <- df_state_month_national %>% 
        filter(month == "April" || month == "May" || month == "June" || 
                        month == "July" || month == "August") %>% 
        mutate(mort_rate = homicidios*100000/population)

df_states_5m <- df_homicides_state_monthly_sspc_gpointerinstitucional %>% 
        filter(month == "April" || month == "May" || month == "June" || 
                        month == "July" || month == "August")

df_states_5m_states <- df_states_5m %>% 
        group_by(year, entidad, state) %>% 
        summarise(homicidios_mean = round(mean(homicidios), 1), 
                  homicidios_mean_rate = round(mean(mort_rate), 1))



# 02. Data filtering  ----------------------------------------------------------

# 03. Create graphs  -----------------------------------------------------------
# 03.1 National graphs --------------------------------------------------------- 
# Total by month (time series)
ggplot(df_month_national,
                aes(x = month_year, y = homicidios, fill = "#9a031e")) +
                geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette)

# Total by month in selected state (time series)
ggplot(df_month %>% filter(state == "Mexico City"),
        aes(x = month_year, y = homicidios, fill = "#9A031E")) +
        geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = "Mexico City",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette)

# Total by month in selected state (time series)
ggplot(df_month %>% filter(state == "Guanajuato"),
        aes(x = month_year, y = homicidios, fill = "#9A031E")) +
        geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = "Guanajuato",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette)

# Total by month in selected state (time series)
ggplot(df_month %>% filter(state == "Aguascalientes"),
        aes(x = month_year, y = homicidios, fill = "#9A031E")) +
        geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = "Aguascalientes",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette)


# Total by month (comparison among years)
ggplot(df_month_national_5m, 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

# Mexico City total homicides by month (comparison among year)
ggplot(df_states_5m %>% filter(state == "Mexico City"), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = "Mexico City",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

# Guanajuato total homicides by month (comparison among year)
ggplot(df_states_5m %>% filter(state == "Guanajuato"), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = "Guanajuato",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

# Guanajuato total homicides by month (comparison among year)
ggplot(df_states_5m %>% filter(state == "Aguascalientes"), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = "Aguascalientes",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)


# Rate by month (comparison among year)
ggplot(df_month_national_5m, 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between 2019 and 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)


# Average in selected states 
ggplot(df_month_national_5m, 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between 2019 and 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate",
                fill = "Year", 
                caption = "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/") +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)


# 03.2 State graphs ------------------------------------------------------------
# Total by month 
# Total by month (comparison among year)
# Total by month in selected states
# Rate by month 
# Rate by month (comparison among year)
# Rate by month in selected states










