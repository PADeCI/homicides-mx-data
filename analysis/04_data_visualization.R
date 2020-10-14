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

# Color blind firendly palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7")


# 01. Load Data ----------------------------------------------------------------
# Total cases by state and month
load(paste0(input, "df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata"))

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
# 03.1 Vectors -----------------------------------------------------------------
v_caption_SSPC <- "Source: Daily reports of the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/"

v_entidades <- c("Aguascalientes", "Baja California", "Baja California Sur" , 
        "Campeche", "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila",
        "Colima", "Durango", "Estado de México", "Guanajuato", "Guerrero",
        "Hidalgo", "Jalisco", "Michoacán", "Morelos", "Nacional", "Nayarit",
        "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", 
        "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
        "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas") 

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
        scale_fill_manual(values=cbPalette)

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
        scale_fill_manual("", values=c("#5F0F40"))

ggsave(filename = paste0(output, "homicides_time_series/g_homicides_timeseries_", 
                        v_states[i], ".png"))
}

beepr::beep(5)

# 03.2 Total homicides by month comparison among years -------------------------
# Trial 
ggplot(df_states_5m, 
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

ggsave(filename = paste0(output, "homicides_5month_comparison/g_homicides_timeseries_", 
                v_states[i], ".png"))
}

beepr::beep(5)

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










