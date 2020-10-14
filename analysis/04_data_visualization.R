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

# Paths 
input           <-  "~/GitHub/homicides-mx-data/data/gpo_interinstitucional/"
output          <-  "~/GitHub/homicides-mx-data/figs/graphs/"


# 01. Load Data ----------------------------------------------------------------
# Total cases by state and month
load(paste0(input, "df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata"))

# 02. Data wrangling  ----------------------------------------------------------
# Factors order 
v_5month <- c("April", "May", "June", "July", "August")
v_5mes <- c("Abril", "Mayo", "Junio", "Julio", "Agosto")
v_5month_2019 <- c()
v_5month_2020 <- c()

for(state in 1:length(v_5month)){
        v_5month_2019[state] <- paste0(v_5month[state], "-19") 
        v_5month_2020[state] <- paste0(v_5month[state], "-20") 
}

v_5month_year <- c(v_5month_2019, v_5month_2020)

# Rename data frame for simplicity 
df_month <- df_homicides_state_monthly_sspc_gpointerinstitucional %>%  
        mutate(month_year = as.factor(paste(month, year)), 
                mes_year = as.factor(paste(mes, year))) %>% 
        mutate(mort_rate = homicidios*100000/population)

df_month_national <- df_month %>%
        filter(state == "National") 

df_month_national_5m <- df_month_national %>% 
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

# 03. Create graphs  -----------------------------------------------------------
# 03.1 Vectors -----------------------------------------------------------------
# Color blind firendly palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
        "#D55E00", "#CC79A7")

# Source caption
v_caption_SSPC <- "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/"

# Names of states in spanish
v_entidades <- c("Aguascalientes", "Baja California", "Baja California Sur" , 
        "Campeche", "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila",
        "Colima", "Durango", "Estado de México", "Guanajuato", "Guerrero",
        "Hidalgo", "Jalisco", "Michoacán", "Morelos", "Nacional", "Nayarit",
        "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", 
        "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
        "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas") 

# Names of states in english
v_states <- c("Aguascalientes", "Baja California", "Baja California Sur", 
        "Campeche", "Chiapas", "Chihuahua", "Mexico City", "Coahuila", "Colima", 
        "Durango", "State of Mexico", "Guanajuato", "Guerrero", "Hidalgo", 
        "Jalisco", "Michoacan", "Morelos", "National", "Nayarit", "Nuevo Leon", 
        "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi", 
        "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
        "Yucatan", "Zacatecas")          

# 03.2 Total homicides by month (time series) ----------------------------------
# Trial for the national level 
ggplot(df_month_national,
                aes(x = month_year, y = homicidios)) +
                geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[7])

# Loop-generated graphs for all states
for(i in 1:length(v_states)){
ggplot(df_month %>% filter(state == v_states[i]),
        aes(x = month_year, y = homicidios, fill = "#9A031E")) +
        geom_col()  +
        labs(title = "Homicides from April 2019 to August 2020", 
                subtitle = v_states[i],
                hjust = 0, 
                x = "Month",
                y = "Number of homicides", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
                scale_fill_manual(values=cbPalette[6])

ggsave(filename = paste0(output, "homicides_time_series/g_homicides_timeseries_", 
                        v_states[i], ".png"))
}

beepr::beep(5)

# 03.2 Total homicides by month comparison among years -------------------------
# Trial 
ggplot(df_states_5m %>% filter(state=="National"), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

# Loop-generated 
for(i in 1:length(v_states)){
ggplot(df_states_5m %>% filter(state == v_states[i]), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicides comparison between 2019 and 2020", 
                subtitle = v_states[i],
                hjust = 0, 
                x = "Month",
                y = "Number of homicides",
                fill = "Year", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

ggsave(filename = paste0(output, "homicides_5month_comparison/g_homicides_timeseries_5m_", 
                v_states[i], ".png"))
}

beepr::beep(5)

# 03.3 Homicides rate by month (time series) -----------------------------------
# Trial 
ggplot(df_month %>% filter(state=="National"),
        aes(x = month_year, y = mort_rate)) +
        geom_col()  +
        labs(title = "Homicide rate from April 2019 to August 2020", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate\n(number of homicides per 100,000 people)", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[7])

# Loop-generated
for(i in 1:length(v_states)){
ggplot(df_month %>% filter(state==v_states[i]),
        aes(x = month_year, y = mort_rate)) +
        geom_col()  +
        labs(title = "Homicide rate from April 2019 to August 2020", 
                subtitle = v_states[i],
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate\n(number of homicides per 100,000 people)", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[2])
        
ggsave(filename = paste0(output, "mort_rate_time_series/g_mortrate_timeseries_", 
                v_states[i], ".png"))
        
}
beepr::beep(5)

# 03.4 Homicides rate by month comparison among years --------------------------
# Trial 
ggplot(df_states_5m %>% filter(state==v_states[i]), 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between 2019 and 2020", 
                subtitle = v_states[i],
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate\n(number of homicides per 100,000 people)",
                fill = "Year", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette[6:7])



# Loop-generated
for(i in 1:length(v_states)){
ggplot(df_states_5m %>% filter(state==v_states[i]), 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between 2019 and 2020", 
                subtitle = v_states[i],
                hjust = 0, 
                x = "Month",
                y = "Monthly homicide rate\n(number of homicides per 100,000 people)",
                fill = "Year", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette[6:7])

ggsave(filename = paste0(output, "mort_rate_5m_comparison/g_mortrate_timeseries_5m_", 
        v_states[i], ".png"))
}
beepr::beep(5)

# 03.2 State graphs ------------------------------------------------------------
# Total by month 
# Total by month (comparison among year)
# Total by month in selected states
# Rate by month 
# Rate by month (comparison among year)
# Rate by month in selected states










