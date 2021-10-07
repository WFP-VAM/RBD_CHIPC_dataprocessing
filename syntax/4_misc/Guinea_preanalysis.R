library(tidyverse)
library(sf)
library(googlesheets4)
library(rgdal)
library(janitor)
library(rmapshaper)
library(readxl)
library(mapview)
library(readxl)
library(extrafont)
library(flextable)
library(officer)
library(rvg)
library(here)


#create theme
#make x and y axis blank, put legend in bottom
theme_vamgraphs <- function(){ 
  font <- "Open Sans"   #assign font family up front
  theme_minimal() %+replace%    #replace elements we want to change
    theme(
      plot.title = element_text(family = "Open Sans SemiBold", color = "black", size = 30, margin=margin(0,0,30,0)),
      strip.text = element_text(family = "Open Sans SemiBold", color = "black", size = 18, margin=margin(0,0,30,0)),
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      axis.text.x = element_text(family = "Open Sans", color = "black", size = 10, angle = 90),
      axis.text.y =  element_text(family = "Open Sans", color = "black", size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = "Open Sans SemiBold", color = "black", size = 8),
      panel.spacing = unit(1, "cm"),
      panel.margin = unit(2, "lines"))
}

#add CH data and select Guunea March 2021
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc.xlsx", guess_max = 21474836) %>% 
  filter(adm0_name == "Guinea" & exercise_year == 2021)
#split current and projected values
#current
cadre_harmonise_caf_ipc_mar2021current <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "current") %>% 
  rename(currentmar2021_population = population, currentmar2021_phase_class = phase_class, currentmar2021_phase1 = phase1, currentmar2021_phase2 = phase2, currentmar2021_phase3 = phase3, currentmar2021_phase4 = phase4, currentmar2021_phase5 = phase5, currentmar2021_phase35 = phase35, currentmar2021_foodconsumption_phase = foodconsumption_phase, currentmar2021_livelihoods_phase = livelihoods_phase, currentmar2021_nutrition_phase = nutrition_phase, currentmar2021_mortality_phase = mortality_phase) 
#projected
cadre_harmonise_caf_ipc_mar2021projected <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "projected") %>% 
  select(adm0_name,adm1_name,adm2_name, adm2_pcod2, projectedmar2021_population = population, projectedmar2021_phase_class = phase_class, projectedmar2021_phase1 = phase1, projectedmar2021_phase2 = phase2, projectedmar2021_phase3 = phase3, projectedmar2021_phase4 = phase4, projectedmar2021_phase5 = phase5, projectedmar2021_phase35 = phase35, projectedmar2021_foodconsumption_phase = foodconsumption_phase, projectedmar2021_livelihoods_phase = livelihoods_phase, projectedmar2021_nutrition_phase = nutrition_phase, projectedmar2021_mortality_phase = mortality_phase)
#join current and projected
cadre_harmonise_caf_ipc_mar2021_gin <- right_join(cadre_harmonise_caf_ipc_mar2021current, cadre_harmonise_caf_ipc_mar2021projected, by = c("adm0_name","adm1_name","adm2_name"))
cadre_harmonise_caf_ipc_mar2021_gin <- cadre_harmonise_caf_ipc_mar2021_gin %>% mutate(adm2_pcod2 = case_when(!is.na(adm2_pcod2.x) ~ adm2_pcod2.x, 
                                                                                                     TRUE ~ adm2_pcod2.y)) %>% select(-adm2_pcod2.x, adm2_pcod2.y)
#create calculated final_phase
cadre_harmonise_caf_ipc_mar2021_gin <- cadre_harmonise_caf_ipc_mar2021_gin %>% mutate(currentmar2021_phase_class_calc = ((3 * currentmar2021_foodconsumption_phase + 2 * currentmar2021_livelihoods_phase)/5), projectedmar2021_phase_class_calc = ((3 * projectedmar2021_foodconsumption_phase + 2 * projectedmar2021_livelihoods_phase)/5))
#round to whole phase
cadre_harmonise_caf_ipc_mar2021_gin <- cadre_harmonise_caf_ipc_mar2021_gin %>% mutate(currentmar2021_phase_class_calc = round(currentmar2021_phase_class_calc,0), projectedmar2021_phase_class_calc = round(projectedmar2021_phase_class_calc,0))
#flags difference between assigned phase and calculated phase
cadre_harmonise_caf_ipc_mar2021_gin <- cadre_harmonise_caf_ipc_mar2021_gin %>% 
  mutate(currentmar2021_phase_class_diff =  currentmar2021_phase_class - currentmar2021_phase_class_calc,
         projectedmar2021_phase_class_diff = projectedmar2021_phase_class - projectedmar2021_phase_class_calc) 
cadre_harmonise_caf_ipc_mar2021_gin <- cadre_harmonise_caf_ipc_mar2021_gin %>%  mutate(currentmar2021_phase_class_diff = case_when(
  currentmar2021_phase_class_diff == 0 ~ "Same", TRUE ~ "Different"), 
  projectedmar2021_phase_class_diff = case_when(
    projectedmar2021_phase_class_diff == 0 ~ "Same", TRUE ~ "Different"))

#adm0 
wca_shp0gin <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") %>% filter(admin0Name %in% c("Guinea"))
#adm2
wca_shp2gin <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") %>% filter(admin0Name %in% c("Guinea"))


#join adm2 shape with adm2 data
wca_adm2_ch_gin <- inner_join(wca_shp2gin, cadre_harmonise_caf_ipc_mar2021_gin, by = c("admin2Pcod" = "adm2_pcod2"))

#mapping
#CH color codes
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
mapcurrentmar2021_phase_class <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(currentmar2021_phase_class))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing") +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 current phasing - assigned") +geom_sf(data=wca_shp0gin, fill=NA)
mapcurrentmar2021_phase_class_calc <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(currentmar2021_phase_class_calc))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing") +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 current phasing - calculated") +geom_sf(data=wca_shp0gin, fill=NA)
mapprojectedmar2021_phase_class <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(projectedmar2021_phase_class))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing") +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 projected phasing - assigned") +geom_sf(data=wca_shp0gin, fill=NA)
mapprojectedmar2021_phase_class_calc <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(projectedmar2021_phase_class_calc))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing") +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 projected phasing - calculated") +geom_sf(data=wca_shp0gin, fill=NA)
#fc
mapcurrentmar2021_foodconsumption_phase <- wca_adm2_ch_gin %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(currentmar2021_foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 (current) food consumption outcome phasing") +geom_sf(data=wca_shp0gin, fill=NA)
mapprojectedmar2021_foodconsumption_phase <- wca_adm2_ch_gin %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(projectedmar2021_foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "June - August 2021 (projected) food consumption outcome phasing") +geom_sf(data=wca_shp0gin, fill=NA)
#livelihoods
mapcurrentmar2021_livelihoods_phase <- wca_adm2_ch_gin %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(currentmar2021_livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 (current) livelihood outcome phasing") +geom_sf(data=wca_shp0gin, fill=NA)
mapprojectedmar2021_livelihoods_phase <- wca_adm2_ch_gin %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(projectedmar2021_livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "June - August 2021 (projected) livelihood outcome phasing") +geom_sf(data=wca_shp0gin, fill=NA)
#differencea
mapcurrentmar2021_phase_class_diff <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(currentmar2021_phase_class_diff))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +ggtitle(label= "March - May 2021 current phasing - difference between assigned and calculated phasing") +geom_sf(data=wca_shp0gin, fill=NA)
mapprojectedmar2021_phase_class_diff <- wca_adm2_ch_gin %>% ggplot()  +geom_sf(data = wca_adm2_ch_gin, mapping = aes(fill = as.factor(projectedmar2021_phase_class_diff))) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +ggtitle(label= "June - August 2021 (projected) - difference between assigned and calculated phasing") +geom_sf(data=wca_shp0gin, fill=NA)

#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
listofplots <- list(mapcurrentmar2021_phase_class, mapcurrentmar2021_phase_class_calc, mapcurrentmar2021_phase_class_diff,
                    mapprojectedmar2021_phase_class, mapprojectedmar2021_phase_class_calc, mapprojectedmar2021_phase_class_diff,
                    mapcurrentmar2021_foodconsumption_phase, mapprojectedmar2021_foodconsumption_phase,
                    mapcurrentmar2021_livelihoods_phase, mapprojectedmar2021_livelihoods_phase)

#create_dml, converts the ggplot objects to dml objects.
create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}
#Apply this function to the list of ggplot objects to create a list of dml objects with the same dimension.
plots_dml <- purrr::map(listofplots, create_dml)
# function to export plot to PowerPoint ----
create_pptx <- function(plot, path, left = 0.5, top = 1, width = 9, height = 4.95){
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
  path = "C:\\RBD_CHIPC_dataprocessing\\analysis\\misc\\Guinea_CHmar2021preanalysis.pptx"
  )

cadre_harmonise_caf_ipc_mar2021_gin %>% write.csv("C:\\RBD_CHIPC_dataprocessing\\analysis\\misc\\cadre_harmonise_caf_ipc_mar2021_gin.csv")







