library(readxl)
library(tidyverse)
library(plotly)
library(extrafont)
library(flextable)
library(rmapshaper)

setwd("C:/RBD_CHIPC_dataprocessing")

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

library(tidyverse)
library(sf)
library(googlesheets4)
library(rgdal)
library(janitor)
library(rmapshaper)
library(readxl)
library(mapview)


#add CH data
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc.xlsx", guess_max = 21474836)
#split into current and projected and then join together - think this could be done smarter with pivot_wider
#current
cadre_harmonise_caf_ipc_mar2021current <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "current") %>% 
  rename(currentmar2021_population = population, currentmar2021_phase_class = phase_class, currentmar2021_phase1 = phase1, currentmar2021_phase2 = phase2, currentmar2021_phase3 = phase3, currentmar2021_phase4 = phase4, currentmar2021_phase5 = phase5, currentmar2021_phase35 = phase35, currentmar2021_foodconsumption_phase = foodconsumption_phase, currentmar2021_livelihoods_phase = livelihoods_phase, currentmar2021_nutrition_phase = nutrition_phase, currentmar2021_mortality_phase = mortality_phase) 
#projected
cadre_harmonise_caf_ipc_mar2021projected <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "projected") %>% 
  select(adm0_name,adm1_name,adm2_name, adm2_pcod2, projectedmar2021_population = population, projectedmar2021_phase_class = phase_class, projectedmar2021_phase1 = phase1, projectedmar2021_phase2 = phase2, projectedmar2021_phase3 = phase3, projectedmar2021_phase4 = phase4, projectedmar2021_phase5 = phase5, projectedmar2021_phase35 = phase35, projectedmar2021_foodconsumption_phase = foodconsumption_phase, projectedmar2021_livelihoods_phase = livelihoods_phase, projectedmar2021_nutrition_phase = nutrition_phase, projectedmar2021_mortality_phase = mortality_phase)
#join current and projected
cadre_harmonise_caf_ipc_mar2021 <- right_join(cadre_harmonise_caf_ipc_mar2021current, cadre_harmonise_caf_ipc_mar2021projected, by = c("adm0_name","adm1_name","adm2_name"))
cadre_harmonise_caf_ipc_mar2021 <- cadre_harmonise_caf_ipc_mar2021 %>% mutate(adm2_pcod2 = case_when(!is.na(adm2_pcod2.x) ~ adm2_pcod2.x, 
                                                                                                     TRUE ~ adm2_pcod2.y)) %>% select(-adm2_pcod2.x, adm2_pcod2.y)
#select countries or in ghana the adm1 areas where analysis was only completed at adm1 level and select / rename variables
cadre_harmonise_caf_ipc_mar2021adm1 <- cadre_harmonise_caf_ipc_mar2021 %>% filter(is.na(adm2_name)) %>% 
  select(adm1_pcod2, currentmar2021_population, currentmar2021_phase_class, currentmar2021_phase1, currentmar2021_phase2, currentmar2021_phase3, currentmar2021_phase4, currentmar2021_phase5, currentmar2021_phase35, currentmar2021_foodconsumption_phase, currentmar2021_livelihoods_phase, currentmar2021_nutrition_phase, currentmar2021_mortality_phase,
         projectedmar2021_population, projectedmar2021_phase_class, projectedmar2021_phase1, projectedmar2021_phase2, projectedmar2021_phase3, projectedmar2021_phase4,projectedmar2021_phase5,projectedmar2021_phase35, projectedmar2021_foodconsumption_phase, projectedmar2021_livelihoods_phase, projectedmar2021_nutrition_phase, projectedmar2021_mortality_phase)
#select countries/areas where analysis was completed at adm2 level and select / rename variables - except Niger                                        
cadre_harmonise_caf_ipc_mar2021adm2 <- cadre_harmonise_caf_ipc_mar2021  %>% filter(!is.na(adm2_name) & adm0_name != "Niger" & adm2_name != "Koro_accessible" & adm2_name != "Koro_limitedaccess")  %>% 
  select(adm2_pcod2, currentmar2021_population, currentmar2021_phase_class, currentmar2021_phase1, currentmar2021_phase2, currentmar2021_phase3, currentmar2021_phase4, currentmar2021_phase5, currentmar2021_phase35, currentmar2021_foodconsumption_phase, currentmar2021_livelihoods_phase, currentmar2021_nutrition_phase, currentmar2021_mortality_phase,
         projectedmar2021_population, projectedmar2021_phase_class, projectedmar2021_phase1, projectedmar2021_phase2, projectedmar2021_phase3, projectedmar2021_phase4,projectedmar2021_phase5,projectedmar2021_phase35, projectedmar2021_foodconsumption_phase, projectedmar2021_livelihoods_phase, projectedmar2021_nutrition_phase, projectedmar2021_mortality_phase)
#select data for Niger which has special adm2 areas - accessible vs non-accessible
cadre_harmonise_caf_ipc_mar2021adm2niger <- cadre_harmonise_caf_ipc_mar2021  %>% filter(adm0_name %in% c("Niger")) %>% 
  select(adm2_pcod2, currentmar2021_population, currentmar2021_phase_class, currentmar2021_phase1, currentmar2021_phase2, currentmar2021_phase3, currentmar2021_phase4, currentmar2021_phase5, currentmar2021_phase35, currentmar2021_foodconsumption_phase, currentmar2021_livelihoods_phase, currentmar2021_nutrition_phase, currentmar2021_mortality_phase,
         projectedmar2021_population, projectedmar2021_phase_class, projectedmar2021_phase1, projectedmar2021_phase2, projectedmar2021_phase3, projectedmar2021_phase4,projectedmar2021_phase5,projectedmar2021_phase35, projectedmar2021_foodconsumption_phase, projectedmar2021_livelihoods_phase, projectedmar2021_nutrition_phase, projectedmar2021_mortality_phase)
#also need to create special shape for Koro, Mopti
cadre_harmonise_caf_ipc_mar2021mli_adm2_koro <- cadre_harmonise_caf_ipc_mar2021  %>% filter(adm2_name %in% c("Koro_accessible","Koro_limitedaccess")) %>% 
  select(adm2_pcod2, currentmar2021_population, currentmar2021_phase_class, currentmar2021_phase1, currentmar2021_phase2, currentmar2021_phase3, currentmar2021_phase4, currentmar2021_phase5, currentmar2021_phase35, currentmar2021_foodconsumption_phase, currentmar2021_livelihoods_phase, currentmar2021_nutrition_phase, currentmar2021_mortality_phase,
         projectedmar2021_population, projectedmar2021_phase_class, projectedmar2021_phase1, projectedmar2021_phase2, projectedmar2021_phase3, projectedmar2021_phase4,projectedmar2021_phase5,projectedmar2021_phase35, projectedmar2021_foodconsumption_phase, projectedmar2021_livelihoods_phase, projectedmar2021_nutrition_phase, projectedmar2021_mortality_phase)
#
#import shapefiles at adm1, adm2 - 
#adm0 
wca_shp0all <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") %>% 
  filter(admin0Name %in% c("Benin","Burkina Faso","Cameroon","Chad","Côte d'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo"))
#adm1
wca_shp1all <- read_sf("data\\geo\\adm1\\wca_admbnda_adm1_ocha_18022021.shp") 
#adm2
wca_shp2all <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") 
#niger
niger_shp <- read_sf("data\\geo\\special_areas\\ner_ch_march2021.gpkg") 
#Koro, Mopti in Mali
mli_adm2_koro_shp <- read_sf("data\\geo\\special_areas\\mli_adm2_koro.gpkg") 




#join adm1 shape with adm1 data 
wca_adm1_ch <- inner_join(wca_shp1all, cadre_harmonise_caf_ipc_mar2021adm1, by = c("admin1Pcod" = "adm1_pcod2"))
#join adm2 shape with adm2 data
wca_adm2_ch <- inner_join(wca_shp2all, cadre_harmonise_caf_ipc_mar2021adm2, by = c("admin2Pcod" = "adm2_pcod2"))

#puts together adm1 and adm2
wca_ch <- bind_rows(wca_adm1_ch, wca_adm2_ch)
#join niger data with niger shape
wca_adm2_niger <- left_join(niger_shp, cadre_harmonise_caf_ipc_mar2021adm2niger, by = "adm2_pcod2")
#join mali,koro with mali,koro shape
wca_adm2_koro <- left_join(mli_adm2_koro_shp, cadre_harmonise_caf_ipc_mar2021mli_adm2_koro, by = "adm2_pcod2")

#remove some variables to make same before binding
wca_ch <- wca_ch %>% select(admin0Name:projectedmar2021_mortality_phase)
wca_adm2_niger <- wca_adm2_niger %>% select(geom:projectedmar2021_mortality_phase) %>% rename(geometry = geom)
wca_ch <- bind_rows(wca_ch, wca_adm2_niger)
wca_adm2_koro  <- wca_adm2_koro %>% select(geom:projectedmar2021_mortality_phase) %>% rename(geometry = geom)
wca_ch <- bind_rows(wca_ch, wca_adm2_koro)

#test mapping
#CH color codes
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
wca_ch <- wca_ch  %>% filter(admin0Name %in% c("Burkina Faso","Cameroon","Chad","Gambia","Mali","Mauritania","Nigeria","Senegal",NA))

#fix and reduce size
wca_ch  <- st_make_valid(wca_ch )
wca_ch  <- rmapshaper::ms_simplify(wca_ch , keep_shapes = TRUE) # simplify polygons



mapcurrentmar2021_phase_class <- wca_ch %>%  ggplot()  +geom_sf(data = wca_ch, mapping = aes(fill = as.factor(currentmar2021_phase_class)), color = NA) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing") +scale_fill_manual(values = CH_colors) +ggtitle(label= "March - May 2021 (current) final area phasing") +geom_sf(data=wca_shp0all, fill=NA)
mapprojectedmar2021_phase_class <- wca_ch %>% ggplot() +geom_sf(mapping = aes(fill = as.factor(projectedmar2021_phase_class)), color = NA) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "June - August 2021 (projected) final area phasing") +geom_sf(data=wca_shp0all, fill=NA)





ggsave("mapcurrentmar2021_phase_class.eps",mapcurrentmar2021_phase_class)
ggsave("mapprojectedmar2021_phase_class.eps",mapprojectedmar2021_phase_class)

#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
listofplots <- list(mapcurrentmar2021_phase_class, mapprojectedmar2021_phase_class)

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
  path = "mapCH_OCHA.pptx"
)







