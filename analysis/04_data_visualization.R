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
library(tidyr)          # For reshaping wide to long
library(stringr)        # For string manipulation
library(forcats)        # For category handling


# Clean workspace 
rm(list=ls())

# Paths 
input_gpo       <- "~/GitHub/homicides-mx-data/data/gpo_interinstitucional/"
input_fa        <- "~/GitHub/homicides-mx-data/data/fuentes_abiertas/"
output          <- "~/GitHub/homicides-mx-data/figs/graphs/"

# Paste paths functions 
        # paste_inp <- function(path){paste0(input, path)} # General form 
paste_inp_gpo <- function(path){paste0(input_gpo, path)}
paste_inp_fa <- function(path){paste0(input_fa, path)} 
paste_out <- function(path){paste0(output, path)}


# 01. Load Data ----------------------------------------------------------------
# Total cases by state and month from Gpo. Inter.(gpo)
load(paste_inp_gpo("df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata"))

# Total cases by state and month from Fuentes Abiertas (fa)
load(paste_inp_fa("df_homicides_state_monthly_sspc_fuentesabiertas.RData"))

# 02. Data wrangling  ----------------------------------------------------------
# 02.1 Gpo. Inter. data --------------------------------------------------------
# Factors order 
v_meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
v_5month <- month.name[4:8]
v_5mes <- c("Abril", "Mayo", "Junio", "Julio", "Agosto")


for(state in 1:length(v_5month)){
        v_5month_2019[state] <- paste0(v_5month[state], "-19") 
        v_5month_2020[state] <- paste0(v_5month[state], "-20") 
}

v_5month_year <- c(v_5month_2019, v_5month_2020)

# Rename data frame for simplicity 
df_month <- df_homicides_state_monthly_sspc_gpointerinstitucional %>%  
        mutate(month = fct_relevel(month, month.name),
                mes = fct_relevel(mes, v_meses)) %>% 
        mutate(month_year = as.factor(paste0(str_sub(month, 1, 3), "-",
                                             str_sub(year, 3, 4))), 
                mes_year = as.factor(paste0(str_sub(mes, 1, 3), "-",
                                            str_sub(year, 3, 4)))) %>% 
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

# 02.2 Fuentes abiertas' data ---------------------------------------------------
df_month_fa <- df_homicides_state_monthly_sspc_fuentesabiertas %>% 
        mutate(month = fct_relevel(month, month.name),
                mes = fct_relevel(mes, v_meses)) %>% 
        mutate(month_year = as.factor(paste0(str_sub(month, 1, 3), "-",
                str_sub(year, 3, 4))), 
                mes_year = as.factor(paste0(str_sub(mes, 1, 3), "-",
                        str_sub(year, 3, 4)))) %>% 
        mutate(mort_rate = homicidios*100000/population)
        
df_month_fa_long <- df_month_fa %>% 
        rename(Male = hombre, 
                Female = mujer, 
                Non_identified  = no_identificado) %>% 
        gather(sex, homicides, Male:Non_identified, factor_key = TRUE) %>% 
        mutate(sex = as.character(sex), 
               sex = case_when(sex == "Non_identified" ~ "Non identified", 
                                sex==sex~sex), 
                sex = as.factor(sex), 
                sex  = fct_reorder(sex, c("Male", "Female", "Non identified"))) 
        
        

# 03. Create graphs (Gpo. Inter.)  ---------------------------------------------
# 03.1 Vectors -----------------------------------------------------------------
# Color blind firendly palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
        "#D55E00", "#CC79A7")

# Source caption
v_caption_SSPC <- "Source: Own elaboration with data from the SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/"

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
# Title and subtitles vectors 
v_title <- "Total homicides by month (as reported by Gpo. Inter)" 
v_xlab  <- "Month"
v_ylab  <- "Number of homicides"

# Manual trial for the national level 
ggplot(df_month_national,
                aes(x = month_year, y = homicidios)) +
                geom_col()  +
        labs(title = v_title, 
                subtitle = "National level",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[7])

# Loop-generated graphs graphs for all states
for(i in 1:length(v_states)){
ggplot(df_month %>% filter(state == v_states[i]),
        aes(x = month_year, y = homicidios, fill = "#9A031E")) +
        geom_col()  +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
                scale_fill_manual(values=cbPalette[1])

ggsave(filename = paste0(output, "gpo_homicides_time_series/g_homicides_timeseries_", 
                        v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output, "gpo_homicides_time_series/g_homicides_timeseries_",
        v_states[i], ".pdf"), width = 6, height = 4)

}

beepr::beep(2)

# 03.3 Total homicides by month comparison among years -------------------------

# Title and subtitle vectors
v_title <- "Homicide comparison by months (as reported by Gpo. Inter)"
v_xlab  <- "Month"
v_ylab <- "Number of homicides"
v_filllab <- "Year"

# Manual trial 
ggplot(df_states_5m %>% filter(state=="National"), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = v_title, 
                subtitle = "National level",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab,
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

# Loop-generated graphs 
for(i in 1:length(v_states)){
ggplot(df_states_5m %>% filter(state == v_states[i]), 
        aes(x = month, y = homicidios, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab,
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

ggsave(filename = paste0(output, "gpo_homicides_5month_comparison/g_homicides_timeseries_5m_", 
                v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output, "gpo_homicides_5month_comparison/g_homicides_timeseries_5m_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 03.4 Homicides rate by month (time series) -----------------------------------
# Title and subtitle vectors
v_title <- "Homicide rate by month (as reported by Gpo. Inter)"
v_xlab <- "Month"
v_ylab <- "Monthly homicide rate\n(number of homicides per 100,000 people)"

# Manual trial 
ggplot(df_month %>% filter(state=="National"),
        aes(x = month_year, y = mort_rate)) +
        geom_col()  +
        labs(title = v_title, 
                subtitle = "National level",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[7])

# Loop-generated graphs
for(i in 1:length(v_states)){
ggplot(df_month %>% filter(state==v_states[i]),
        aes(x = month_year, y = mort_rate)) +
        geom_col()  +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        guides(fill = "none") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[7])
        
ggsave(filename = paste0(output, "gpo_mort_rate_time_series/g_mortrate_timeseries_", 
                v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output, "gpo_mort_rate_time_series/g_mortrate_timeseries_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 03.5 Homicides rate by month comparison among years --------------------------

# Title and subtitle vectors 
v_title <- "Homicide rate comparison by month (as reported by Gpo. Inter)"
v_xlab <- "Month"        
v_ylab <- "Monthly homicide rate\n(number of homicides per 100,000 people)" 
v_filllab <- "Year"

# Manual trial 
ggplot(df_states_5m %>% filter(state==v_states[i]), 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab,
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette[6:7])



# Loop-generated graphs
for(i in 1:length(v_states)){
ggplot(df_states_5m %>% filter(state==v_states[i]), 
        aes(x = month, y = mort_rate, fill = year)) +
        geom_col(position = "dodge") +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab,
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values=cbPalette)

ggsave(filename = paste0(output, "gpo_mort_rate_5m_comparison/g_mortrate_timeseries_5m_", 
        v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output, "gpo_mort_rate_5m_comparison/g_mortrate_timeseries_5m_",
        v_states[i], ".pdf"), width = 6, height = 4)
}
beepr::beep(2)


# 04. Create graphs (Fuentes abiertas)  ----------------------------------------
# 04.1 Vectors -----------------------------------------------------------------
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
        "Jalisco", "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", 
        "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi", 
        "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
        "Yucatan", "Zacatecas")          


# 04.2 Total homicides by month (time series) -----------------------------------
# Title and subtitle vectors 
v_title <- "Total homicides by month (as reported by open sources)"
v_xlab <- "Month"
v_ylab <- "Number of homicides"

# Manual trial
ggplot(df_month_fa %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=homicidios)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[3])

# Loop-generated graphs
for(i in 1:length(v_states)){
ggplot(df_month_fa %>% filter(state == v_states[i]), 
        aes(x = month_year, y=homicidios)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[3])

ggsave(filename = paste0(output, 
        "fa_homicides_time_series/g_homicies_time_series_total_", 
        v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output,
        "fa_homicides_time_series/g_homicies_time_series_total_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)




# 04.3 Male homicides by month (time series) -----------------------------------
# Title and subtitle vectors 
v_title <- "Male homicides by month (as reported by open sources)"
v_xlab <- "Month"
v_ylab <- "Number of homicides"

# Manual trial
ggplot(df_month_fa %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=hombre)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[1])

# Loop-generated graphs
for(i in 1:length(v_states)){
        ggplot(df_month_fa %>% filter(state == v_states[i]), 
                aes(x = month_year, y=hombre)) +
                geom_col() +
                labs(title = v_title, 
                        subtitle = v_states[i],
                        hjust = 0, 
                        x = v_xlab,
                        y = v_ylab, 
                        caption = v_caption_SSPC) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 30)) +
                guides(fill = "none") +
                scale_fill_manual(values = cbPalette[7])
        
ggsave(filename = paste0(output, 
        "fa_homicides_time_series/g_homicies_time_series_male_", 
        v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output,
        "fa_homicides_time_series/g_homicies_time_series_male_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 04.4 Female homicides by month (time series) -----------------------------------
# Title and subtitle vectors 
v_title <- "Female homicides by month (as reported by open sources)"
v_xlab <- "Month"
v_ylab <- "Number of homicides"

# Manual trial
ggplot(df_month_fa %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=mujer)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[5])

# Loop-generated graphs
for(i in 1:length(v_states)){
        ggplot(df_month_fa %>% filter(state == v_states[i]), 
                aes(x = month_year, y=mujer)) +
                geom_col() +
                labs(title = v_title, 
                        subtitle = v_states[i],
                        hjust = 0, 
                        x = v_xlab,
                        y = v_ylab, 
                        caption = v_caption_SSPC) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 30)) +
                guides(fill = "none") +
                scale_fill_manual(values=cbPalette[5])
        
ggsave(filename = paste0(output, 
        "fa_homicides_time_series/g_homicies_time_series_female_", 
        v_states[i], ".png"), width = 6, height = 4)

ggsave(filename = paste0(output,
        "fa_homicides_time_series/g_homicies_time_series_female_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 04.5 Non-identified homicides by month (time series) -----------------------------------
# Title and subtitle vectors 
v_title <- "Non-identified gender homicides by month (as reported by open sources)"
v_xlab <- "Month"
v_ylab <- "Number of homicides"

# Manual trial
ggplot(df_month_fa %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=no_identificado)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[5])

# Loop-generated graphs
for(i in 1:length(v_states)){
        ggplot(df_month_fa %>% filter(state == v_states[i]), 
                aes(x = month_year, y=no_identificado)) +
                geom_col() +
                labs(title = v_title, 
                        subtitle = v_states[i],
                        hjust = 0, 
                        x = v_xlab,
                        y = v_ylab, 
                        caption = v_caption_SSPC) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 30)) +
                guides(fill = "none") +
                scale_fill_manual(values=cbPalette[5])
        
ggsave(filename = paste0(output, 
        "fa_homicides_time_series/g_homicies_time_series_noniden_", 
        v_states[i], ".png"), width = 6, height = 4)

ggsave(filename = paste0(output,
        "fa_homicides_time_series/g_homicies_time_series_noniden_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 04.6 Monthly homicides by gender, 2019-2020  ---------------------------
# Title and subtitle vectors 
v_title <- "Monthly homicides by gender, 2019-2020"
v_xlab <- "Month"
v_ylab <- "Number of homicides"
v_filllab <- "Sex"

df_month_fa_long <- df_month_fa_long %>% 
        mutate(sex = fct_reorder(sex, c("Non identified", "Male", "Female")))

# Manual trial
# Stacked bars
ggplot(df_month_fa_long %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=homicides, fill = sex)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_fill_manual(values=cbPalette[4:2])

# Adjacent bars
ggplot(df_month_fa_long %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=homicides, fill = sex)) +
        geom_col(position="dodge") +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_fill_manual(values=cbPalette[4:2])

# Loop-generated graphs
for(i in 1:length(v_states)){
ggplot(df_month_fa_long %>% filter(state == v_states[i]), 
        aes(x = month_year, y=homicides, fill = sex)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = v_states[i],
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                fill = v_filllab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        scale_fill_manual(values=cbPalette[4:2])

        
ggsave(filename = paste0(output, 
        "fa_homicides_time_series/g_homicies_time_series_all_genders_", 
        v_states[i], ".png"), width = 6, height = 4)

ggsave(filename = paste0(output,
        "fa_homicides_time_series/g_homicies_time_series_all_genders_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 4.07 Mortality rate (total) --------------------------------------------------
# Title and subtitle vectors 
v_title <- "Mortality rate of total homicides (as reported by open sources)"
v_xlab <- "Month"
v_ylab <- "Monthly homicide rate\n(number of homicides per 100,000 people)"

# Manual trial
ggplot(df_month_fa %>% filter(state == "Mexico City"), 
        aes(x = month_year, y=mort_rate)) +
        geom_col() +
        labs(title = v_title, 
                subtitle = "Mexico City",
                hjust = 0, 
                x = v_xlab,
                y = v_ylab, 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) +
        guides(fill = "none") +
        scale_fill_manual(values=cbPalette[3])

# Loop-generated graphs
for(i in 1:length(v_states)){
        ggplot(df_month_fa %>% filter(state == v_states[i]), 
                aes(x = month_year, y=mort_rate)) +
                geom_col() +
                labs(title = v_title, 
                        subtitle = v_states[i],
                        hjust = 0, 
                        x = v_xlab,
                        y = v_ylab, 
                        caption = v_caption_SSPC) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 30)) +
                guides(fill = "none") +
                scale_fill_manual(values=cbPalette[3])
        
ggsave(filename = paste0(output, 
        "fa_mort_rate_time_series/g_mort_rate_time_series_total_", 
        v_states[i], ".png"), width = 6, height = 4)
ggsave(filename = paste0(output,
        "fa_mort_rate_time_series/g_mort_rate_time_series_total_",
        v_states[i], ".pdf"), width = 6, height = 4)
}

beepr::beep(2)

# 4.08 Mortality rate (male) ---------------------------------------------------

# 4.09 Mortality rate (female) -------------------------------------------------
# 4.10 Mortality rate (non-identified) -----------------------------------------
# 4.11 Mortality rate (all genders) --------------------------------------------

# End of script  -------------------------------------------------------------
beepr::beep(3)

