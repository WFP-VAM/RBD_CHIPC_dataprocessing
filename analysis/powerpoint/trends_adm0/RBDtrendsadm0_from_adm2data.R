library(readxl)
library(tidyverse)
library(plotly)
library(extrafont)
library(flextable)
library(officer)
library(rvg)
library(here)
library(roperators)

#how to add template?
#change size / layouts of graphics so that fits better
#shorten code 

#create theme
#make x and y axis blank, put legend in bottom
theme_vamgraphs <- function(){ 
  font <- "Open Sans"   #assign font family up front
  theme_minimal() %+replace%    #replace elements we want to change
    theme(
      plot.title = element_text(family = "Open Sans SemiBold", color = "black", hjust = 0.5, size = 18, margin=margin(0,0,5,0)),
      plot.subtitle = element_text(family = "Open Sans SemiBold", color = "black", hjust = 0.5, size = 12, margin=margin(0,0,30,0)),
      strip.text = element_text(family = "Open Sans SemiBold", color = "black",  size = 10, margin=margin(0,0,30,0)),
      strip.text.x = element_text(family = "Open Sans SemiBold", color = "black",  size = 10, margin=margin(0,0,30,0)),
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      axis.text.x = element_text(family = "Open Sans", color = "black", size = 8, angle = 90),
      axis.title.x = element_blank(),
      axis.text.y =  element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = "Open Sans SemiBold", color = "black", size = 8),
      panel.spacing = unit(1, "cm"),
      panel.margin = unit(2, "lines"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())
}

setwd("C:/RBD_CHIPC_dataprocessing/analysis/powerpoint/trends_adm0")


#import CH/IPC country level data
data <- read_excel("C:/RBD_CHIPC_dataprocessing/data/processed/cadre_harmonise_caf_ipc.xlsx")
#only select data since 2019 and only the relevant exercise/reference period 
data_filtered <- data %>% filter(reference_year >= 2019) %>% filter(usethisperiod == "Y")
#use filter to only select proper exercise/reference periods (i.e. dont use the projections from Sep-Dec when Jan-May results are available the same year)
data_filtered <- data_filtered %>% filter(usethisperiod == "Y")
#take out new states in Nigeria analysed in 2021 but not before



#abbreviate name of CAR so it fits graphics
data_filtered <- data_filtered %>% mutate(adm0_name = case_when(
  adm0_name == "Central African Republic" ~ "C.A.R.",
  TRUE ~ adm0_name))
#change reference time period labels to be more informative and not franglais
data_filtered <- data_filtered %>% mutate(reference_label = case_when(
  reference_label == "Jun-Aug" ~ "Projected: June-August",
  reference_label == "Sep-Dec" ~ "Current: September-December",
  reference_label == "Jan-May" ~ "Current: January-May"
))

#make reference year a factor
data_filtered <- data_filtered %>% mutate(reference_year = as.factor(reference_year))
#creates list of sahel+NGA/CMR/CAR and coastal countries
sahelplus <- c("Burkina Faso","C.A.R.","Cameroon","Chad","Mali","Mauritania","Niger","Nigeria","Senegal")
coastal <- c("Benin","Cabo Verde","Cote d'Ivoire","Gambia","Guinea", "Ghana","Guinea-Bissau","Liberia","Sierra Leone","Togo")

data_filtered$reference_label <- fct_relevel(data_filtered$reference_label,  "Current: September-December", "Projected: June-August",  "Current: January-May")

#phase5
phase5_rbd <- data_filtered %>% mutate_at(vars(phase1:phase35), ~replace_na(., 0)) %>% group_by(reference_label, reference_year) %>% 
  summarise(phase5 = round(sum(phase5),0)) %>% ungroup()

#create % food insecure (phase3-5) out of analyzed population total
data_filtered <- data_filtered %>% mutate_at(vars(phase1:phase35), ~replace_na(., 0)) %>% mutate(perc35 = round((phase35 / population)*100,1),
                                          phase1million = phase1 / 1E6,
                                          phase2million = phase2 / 1E6,
                                          phase3million = phase3 / 1E6,
                                          phase4million = phase4 / 1E6,
                                          phase5million = phase5 / 1E6,
                                          phase35million = phase35 / 1E6) 

#summarize phases for RBD 
sum_phase_rbd <- data_filtered %>% group_by(reference_label, reference_year) %>% 
summarise(phase1 = sum(phase1million), phase2 = sum(phase2million), phase3 = sum(phase3million), phase4 = sum(phase4million), phase5 = sum(phase5million), phase35 = sum(phase35million)) %>% ungroup()
sum_phase_rbd$reference_label <- fct_relevel(sum_phase_rbd$reference_label,  "Current: September-December", "Projected: June-August",  "Current: January-May")
sum_phase_rbd_perc <- sum_phase_rbd %>% mutate_at(vars(phase1:phase35), ~round(., 1)) %>% mutate(Row = 1:n()) %>%  group_by(reference_label) %>% 
  mutate(percentage_change = round((phase35/lag(phase35) * 100) -100,0)) %>% select(-Row)

#summarize phases for Sahel 
sum_phase_sahel <- data_filtered %>% filter(adm0_name %in% c("Burkina Faso","Chad","Mali","Mauritania","Niger")) %>% group_by(reference_label, reference_year) %>% 
  summarise(phase1 = sum(phase1million), phase2 = sum(phase2million), phase3 = sum(phase3million), phase4 = sum(phase4million), phase5 = sum(phase5million), phase35 = sum(phase35million)) %>% ungroup()
sum_phase_sahel_perc <- sum_phase_sahel %>% mutate_at(vars(phase1:phase35), ~round(., 1)) %>% mutate(Row = 1:n()) %>%  group_by(reference_label) %>% 
  mutate(percentage_change = round((phase35/lag(phase35) * 100) -100,0)) %>% select(-Row)

#summarize phases for Central Sahel 
sum_phase_centralsahel <- data_filtered %>% filter(adm0_name %in% c("Burkina Faso","Mali","Niger")) %>% group_by(reference_label, reference_year) %>% 
  summarise(phase1 = sum(phase1million), phase2 = sum(phase2million), phase3 = sum(phase3million), phase4 = sum(phase4million), phase5 = sum(phase5million), phase35 = sum(phase35million)) %>% ungroup()
sum_phase_centralsahel_perc <- sum_phase_centralsahel %>% mutate_at(vars(phase1:phase35), ~round(., 1)) %>% mutate(Row = 1:n()) %>%  group_by(reference_label) %>% 
  mutate(percentage_change = round((phase35/lag(phase35) * 100) -100,0)) %>% select(-Row)

#summarize phase for Lac Chad Basin
sum_phase_lcb <- data_filtered %>% filter(adm1_name %in% c("Extreme-Nord","Lac","Diffa","Adamawa","Borno","Yobe")) %>% group_by(reference_label, reference_year) %>% 
  summarise(phase1 = sum(phase1million), phase2 = sum(phase2million), phase3 = sum(phase3million), phase4 = sum(phase4million), phase5 = sum(phase5million), phase35 = sum(phase35million)) %>% ungroup()
sum_phase_lcb_perc <- sum_phase_lcb %>% mutate_at(vars(phase1:phase35), ~round(., 1)) %>% mutate(Row = 1:n()) %>%  group_by(reference_label) %>% 
  mutate(percentage_change = round((phase35/lag(phase35) * 100) -100,0)) %>% select(-Row)




#pivot phases 2 - 4
phase2to4_rbd <- sum_phase_rbd %>% select(-phase35, -phase1, -phase5) %>%  pivot_longer(!reference_label:reference_year, names_to = "phase", values_to = "value")  
phase2to4_rbd_perc <- phase2to4_rbd %>% mutate(Row = 1:n()) %>%  group_by(reference_label, phase) %>% 
  mutate(percentage_change = (value/lag(value) * 100) -100) %>% select(-Row)
#create another variable not showing less than 0.1
phase2to4_rbd <- phase2to4_rbd %>% mutate(value_filtered = replace(value, value<0.1, NA)) %>% mutate(value_filtered = round(value_filtered,1))


#summarized phases for each country in RBD
data_filtered_country <- data_filtered  %>% group_by(reference_label, reference_year, adm0_name) %>% 
  summarise(phase1 = sum(phase1million), phase2 = sum(phase2million), phase3 = sum(phase3million), phase4 = sum(phase4million), phase5 = sum(phase5million), phase35 = sum(phase35million)) %>% ungroup()
data_filtered_country_perc <- data_filtered_country %>% mutate(Row = 1:n()) %>%  group_by(adm0_name, reference_label) %>% 
  mutate(percentage_change = round((phase35/lag(phase35) * 100) -100,0)) %>% select(-Row) 

#pivot longer
data_filtered_country_pivot <- data_filtered_country %>% select(-phase35,-phase1) %>% pivot_longer(!reference_label:adm0_name, names_to = "phase", values_to = "value")



#CH color code
CH_colors = c("phase1" = "#c6ffc7", "phase2" = "#ffe718", "phase3" = "#e88400", "phase4" = "#e02d00", "phase5" = "#5e0803")
CH_colors2to4 = c("phase2" = "#ffe718", "phase3" = "#e88400", "phase4" = "#e02d00")

#Entire region
#number phase3-5 by season
plot_rbd_35 <- sum_phase_rbd_perc %>%  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ reference_label) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase 3 - 5) by Season", subtitle = "Includes all RBD Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_rbd_35  <- plot_rbd_35  +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = paste0(phase35,"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(percentage_change,"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#Sahel 5 
plot_sahel_35 <- sum_phase_sahel_perc %>%  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ reference_label) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase 3 - 5) by Season", subtitle = "Sahel 5 Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_sahel_35  <- plot_sahel_35  +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = paste0(phase35,"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(percentage_change,"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#Central Sahel 
plot_centralsahel_35 <- sum_phase_centralsahel_perc %>%  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ reference_label) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase 3 - 5) by Season", subtitle = "Central Sahel Countries (BKA, MLI, NER)", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_centralsahel_35  <- plot_centralsahel_35  +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = paste0(phase35,"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(percentage_change,"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#LCB
plot_lcb_35 <- sum_phase_lcb_perc %>%  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ reference_label) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase 3 - 5) by Season", subtitle = "Lake Chad Basin (Far North, Adamawa, Borno, Yobe, Lac and Diffa)", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_lcb_35  <- plot_lcb_35  +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = paste0(phase35,"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(percentage_change,"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 


#number by phase 
#projected
plot_rbd_proj <- phase2to4_rbd %>% filter(reference_label == "Projected: June-August") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Year", subtitle = "June - August, 2019 - 2022: Includes all RBD Countries", caption = "") 
plot_rbd_proj  <- plot_rbd_proj +ylab("number of people (millions)")  +geom_text(aes(label = paste0(round(value_filtered,1),"m"), angle = 0), size = 3) 
#janmay
plot_rbd_janmay <- phase2to4_rbd %>% filter(reference_label == "Current: January-May") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity")  +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Year", subtitle = "January - May, 2019 - 2021: Includes all RBD Countries", caption = "") 
plot_rbd_janmay  <- plot_rbd_janmay +ylab("number of people (millions)")    +geom_text(aes(label = paste0(round(value_filtered,1),"m"), angle = 0), size = 3) 
#sepdec
plot_rbd_sepdec <- phase2to4_rbd %>% filter(reference_label == "Current: September-December") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Year", subtitle = "September-December, 2019 - 2021: Includes all RBD Countries", caption = "") 
plot_rbd_sepdec  <- plot_rbd_sepdec +ylab("number of people (millions)")    +geom_text(aes(label = paste0(round(value_filtered,1),"m"), angle = 0), size = 3) 
#number by phase - faceted by phase
#projected
plot_rbd_proj_fac <- phase2to4_rbd_perc %>% filter(reference_label == "Projected: June-August") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ phase) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Phase and Year", subtitle = "June - August, 2019 - 2022: Including all RBD Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_rbd_proj_fac  <- plot_rbd_proj_fac +ylab("number of people (millions)")+geom_text(aes(label = paste0(round(value,1),"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#janmay
plot_rbd_janmay_fac <- phase2to4_rbd_perc %>% filter(reference_label == "Current: January-May") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ phase) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Phase and Year", subtitle = "January - May, 2019 - 2021: Including all RBD Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_rbd_janmay_fac  <- plot_rbd_janmay_fac +ylab("number of people (millions)")+geom_text(aes(label = paste0(round(value,1),"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#sepdec
plot_rbd_sepdec_fac <- phase2to4_rbd_perc %>% filter(reference_label == "Current: September-December") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ phase) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Phase 2 - 4 Populations by Phase and Year", subtitle = "September-December, 2019 - 2021: Including all RBD Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year")
plot_rbd_sepdec_fac  <- plot_rbd_sepdec_fac +ylab("number of people (millions)")+geom_text(aes(label = paste0(round(value,1),"m"), angle = 0, vjust = 1.5), size = 3) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 3) 
#phase5
#Entire region
#number phase3-5 by season
plot_rbd_phase5 <- phase5_rbd %>%  ggplot(aes(x = reference_year, y = phase5)) +geom_col(position = "dodge",fill = "#5e0803", width = .75) +facet_grid(. ~ reference_label) +theme_vamgraphs() +labs(title = "Trends of the Number of Phase 5 by Year and Season", subtitle = "Includes all RBD Countries", caption = "") 
plot_rbd_phase5  <- plot_rbd_phase5  +ylab("number of people in phase 5") +geom_text(aes(label = phase5))




#Sahel
#number phase3-5 projected
plot_sahel_proj <- data_filtered_country_perc  %>% filter(adm0_name %in% sahelplus & reference_label == "Projected: June-August")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "June - August, 2019 - 2022: Sahel 6 & CMR/CAF/NGA", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_sahel_proj <- plot_sahel_proj  +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,1), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0, 18)) 
#number phase3-5 current - Jan-May
plot_sahel_janmay <- data_filtered_country_perc %>% filter(adm0_name %in% sahelplus & reference_label == "Current: January-May")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "January - May, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_sahel_janmay <- plot_sahel_janmay   +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,1), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0, 18)) 
#number phase3-5 current - Sep-Dec
plot_sahel_sepdec <- data_filtered_country_perc %>% filter(adm0_name %in% sahelplus & reference_label == "Current: September-December")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "September - December, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "percentage shown is percent change in phase 3 - 5 from previous year") 
plot_sahel_sepdec <- plot_sahel_sepdec   +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,1), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0, 18)) 


# 
# #STOPPED HERE - NEED TO MAKE IT 
# 
#number of pop by phase - simple
plot_sahel_phase_proj <- data_filtered_country_pivot %>% filter(adm0_name %in% sahelplus & reference_label == "Projected: June-August") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Population by Phase and Year and Country", subtitle = "June - August, 2019 - 2022: Sahel 6 & CMR/CAF/NGA", caption = "")
plot_sahel_phase_proj  <- plot_sahel_phase_proj +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)
plot_sahel_phase_janmay <- data_filtered_country_pivot %>% filter(adm0_name %in% sahelplus & reference_label == "Current: January-May") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Population by Phase and Year and Country", subtitle = "January - May, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "")
plot_sahel_phase_janmay   <- plot_sahel_phase_janmay  +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)
plot_sahel_phase_sepdec <- data_filtered_country_pivot %>% filter(adm0_name %in% sahelplus & reference_label == "Current: September-December") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors2to4) +theme_vamgraphs() +labs(title = "Trends of Population by Phase and Year and Country", subtitle = "September - December, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "")
plot_sahel_phase_sepdec  <- plot_sahel_phase_sepdec +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)


#Coastal
#number phase3-5 projected
plot_coastal_proj <- data_filtered_country_perc %>% filter(adm0_name %in% coastal & reference_label == "Projected: June-August")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "June - August, 2017 - 2022: Coastal Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year")
plot_coastal_proj <- plot_coastal_proj   +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,2), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0, 2))
#number phase3-5 current - Jan-May
plot_coastal_janmay <- data_filtered_country_perc%>% filter(adm0_name %in% coastal & reference_label == "Current: January-May")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "January - May, 2017 - 2021: Coastal Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year")
plot_coastal_janmay <- plot_coastal_janmay   +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,2), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0,2))
#number phase3-5 current - Sep-Dec
plot_coastal_sepdec <- data_filtered_country_perc %>% filter(adm0_name %in% coastal & reference_label == "Current: September-December")  %>%
  ggplot(aes(x = reference_year, y = phase35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends of the Number of Food Insecure (phase3 to phase5) by Country and Year", subtitle = "September - December, 2017 - 2021: Coastal Countries", caption = "percentage shown is percent change in phase 3 - 5 from previous year")
plot_coastal_sepdec <- plot_coastal_sepdec   +ylab("number of people (millions) in phase3-5") +geom_text(aes(label = round(phase35,2), angle = 0, vjust = 1.5), size = 2.5) +geom_text(aes(label = paste0(round(percentage_change),"%"), angle = 45, hjust = -.1, vjust = .2), size = 2.5) +scale_y_continuous(limits = c(0,2))

# 

#number of pop by phase - simple
plot_coastal_phase_proj <- data_filtered_country_pivot %>% filter(adm0_name %in% coastal & reference_label == "Projected: June-August") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors) +theme_vamgraphs() +labs(title = "Trends of Estimated Population by Phase and Year and Country", subtitle = "June - August, 2019 - 2022: Coastal countries", caption = "")
plot_coastal_phase_proj  <- plot_coastal_phase_proj +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)
plot_coastal_phase_janmay <- data_filtered_country_pivot %>% filter(adm0_name %in% coastal & reference_label == "Current: January-May") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors) +theme_vamgraphs() +labs(title = "Trends of Estimated Population by Phase and Year and Country", subtitle = "January - May, 2019 - 2021: Coastal countries", caption = "")
plot_coastal_phase_janmay   <- plot_coastal_phase_janmay  +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)
plot_coastal_phase_sepdec <- data_filtered_country_pivot %>% filter(adm0_name %in% coastal & reference_label == "Current: September-December") %>% ggplot(aes(x = reference_year, y = value, fill = phase)) +geom_bar(stat = "identity") +facet_grid(. ~ adm0_name) +scale_fill_manual(values = CH_colors) +theme_vamgraphs() +labs(title = "Trends of Estimated Population by Phase and Year and Country", subtitle = "September - December, 2019 - 2021: Coastal countries", caption = "")
plot_coastal_phase_sepdec  <- plot_coastal_phase_sepdec +ylab("number of people (millions)") #+geom_text(aes(label = round(value,1), angle = 0, vjust = 1.5), size = 3)

# # #Sahel
data_filtered_perc <- data_filtered_country_perc %>% mutate(analyzedpop = phase1 +phase2 +phase3 +phase4 +phase5) %>% mutate(percphase35 = ((phase35/analyzedpop) *100)) %>% mutate(percphase35 = round(percphase35,1))


    


view(data_filtered_perc)

# #% phase3-5 projected
plot_sahel_proj_perc <- data_filtered_perc %>% filter(adm0_name %in% sahelplus & reference_label == "Projected: June-August")  %>%
ggplot(aes(x = reference_year, y = percphase35)) +geom_col(position = "dodge",fill = "#69b3a2", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5) of Total Population", subtitle = "June - August, 2019 - 2022: Sahel 6 & CMR/CAF/NGA", caption = "") 
plot_sahel_proj_perc <- plot_sahel_proj_perc  +ylab("percent of people in phase3-5 out of total analyzed population") +geom_text(aes(label = paste0(round(percphase35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 60)) 
# #% phase3-5 current - Jan-May
# plot_sahel_janmay_perc <- data_filtered %>% filter(adm0_name %in% sahelplus & reference_label == "Current: January-May")  %>%
# ggplot(aes(x = reference_year, y = perc35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5)", subtitle = "January - May, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "") 
# plot_sahel_janmay_perc <- plot_sahel_janmay_perc  +ylab("percent of people in phase3-5") +geom_text(aes(label = paste0(round(perc35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 60)) 
# #% phase3-5 current - Sep-Dec
plot_sahel_sepdec_perc <- data_filtered_perc %>% filter(adm0_name %in% sahelplus & reference_label == "Current: September-December")  %>%
ggplot(aes(x = reference_year, y = percphase35)) +geom_col(position = "dodge",fill = "#69b3a2", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5) of Total Population", subtitle = "September - December, 2019 - 2021: Sahel 6 & CMR/CAF/NGA", caption = "") 
plot_sahel_sepdec_perc <- plot_sahel_sepdec_perc +ylab("percent of people in phase3-5 out of total analyzed population") +geom_text(aes(label = paste0(round(percphase35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 60)) 
# 
# # #Coastal 
# # # % phase3-5 projected
plot_coastal_proj_perc <- data_filtered_perc %>% filter(adm0_name %in% coastal & reference_label == "Projected: June-August")  %>%
ggplot(aes(x = reference_year, y = percphase35)) +geom_col(position = "dodge",fill = "#69b3a2", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5)", subtitle = "June - August, 2019 - 2022: Coastal countries", caption = "") 
plot_coastal_proj_perc  <- plot_coastal_proj_perc   +ylab("percent of people in phase3-5 out of total analyzed population") +geom_text(aes(label = paste0(round(percphase35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 30)) 
#  # % phase3-5 current - Jan-May
# plot_coastal_janmay_perc <- data_filtered %>% filter(adm0_name %in% coastal & reference_label == "Current: January-May")  %>%
# ggplot(aes(x = reference_year, y = perc35)) +geom_col(position = "dodge",fill = "#4E84C4", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5)", subtitle = "January - May, 2019 - 2021: Coastal countries", caption = "") 
# plot_coastal_janmay_perc <- plot_coastal_janmay_perc +ylab("percent of people in phase3-5") +geom_text(aes(label = paste0(round(perc35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 30)) 
# # # % phase3-5 current - Sep-Dec
plot_coastal_sepdec_perc <- data_filtered_perc %>% filter(adm0_name %in% coastal & reference_label == "Current: September-December")  %>%
ggplot(aes(x = reference_year, y = percphase35)) +geom_col(position = "dodge",fill = "#69b3a2", width = .75) +facet_grid(. ~ adm0_name) +theme_vamgraphs() +labs(title = "Trends in the Percentage of Food Insecure (phase3 to phase5) of Total Population", subtitle = "September - December, 2019 - 2021: Coastal countries", caption = "") 
plot_coastal_sepdec_perc <- plot_coastal_sepdec_perc  +ylab("percent of people in phase3-5 out of total analyzed population") +geom_text(aes(label = paste0(round(percphase35),"%"), angle = 90, vjust = .5, hjust = -.1), size = 3) +scale_y_continuous(limits = c(0, 30)) 


#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
listofplots <- list(plot_rbd_35,  
                    plot_sahel_35, plot_centralsahel_35, plot_lcb_35,
                    plot_rbd_sepdec, plot_rbd_proj, #plot_rbd_janmay,
                    plot_rbd_sepdec_fac, plot_rbd_proj_fac, #plot_rbd_janmay_fac,
                    plot_rbd_phase5,
                    plot_sahel_sepdec,  plot_sahel_proj, #plot_sahel_janmay,
                    plot_coastal_sepdec,  plot_coastal_proj, #plot_coastal_janmay,
                    plot_sahel_sepdec_perc,  plot_sahel_proj_perc,  #plot_sahel_janmay_perc,
                    plot_coastal_sepdec_perc,  plot_coastal_proj_perc,  #plot_coastal_janmay_perc,
                    plot_sahel_phase_sepdec,  plot_sahel_phase_proj,  #plot_sahel_phase_janmay,
                    plot_coastal_phase_sepdec, plot_coastal_phase_proj) #plot_coastal_phase_janmay)
#create_dml, converts the ggplot objects to dml objects.
create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}
#Apply this function to the list of ggplot objects to create a list of dml objects with the same dimension.
plots_dml <- purrr::map(listofplots, create_dml)
# function to export plot to PowerPoint ----
create_pptx <- function(plot, path, left = 0.5, top = 0.5, width = 9, height = 7){
  # if file does not yet exist, create new PowerPoint ----
  if (!file.exists(path)) {
    out <- officer::read_pptx()
  }
  # if file exist, append slides to exisiting file ----
  else {
    out <- officer::read_pptx(path)
  }
  out %>% 
    officer::add_slide() %>% 
    officer::ph_with(plot, location = officer::ph_location(
      width = width, height = height, left = left, top = top)) %>% 
    base::print(target = path)
}
##now fire away!
purrr::map(
  # dml plots to export ----
  plots_dml, 
  # exporting function ----
  create_pptx, 
  # additional fixed arguments in create_pptx ----
  path = "CH_country_trends_Nov2021.pptx"
)






