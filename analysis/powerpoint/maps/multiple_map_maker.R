
library(tidyverse)
library(sf)
library(readxl)
library(tidyverse)
library(extrafont)

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



library(sf)
library(rmapshaper)
library(readxl)
library(mapview)

#add CH shapefiles
wca_CHIPC_nov2021_current <- read_sf("C:\\RBD_CHIPC_dataprocessing\\data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2021_current_nov2021_simple.geojson")
wca_CHIPC_nov2021_projected <- read_sf("C:\\RBD_CHIPC_dataprocessing\\data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2021_projected_jun2022_simple.geojson")
#add boundarry of countries
wca_shp0all <- read_sf("C:\\RBD_CHIPC_dataprocessing\\data\\geo\\finalized_CHgeofiles\\wca_shp0all.geojson") %>% mutate(admin0Name = case_when(
  admin0Name == "Côte d'Ivoire" ~ "Cote d'Ivoire", 
  admin0Name == "Guinea Bissau" ~ "Guinea-Bissau",
  TRUE ~ admin0Name))

#recode NA as missing
wca_CHIPC_nov2021_current <- wca_CHIPC_nov2021_current %>% mutate(across(foodconsumption_phase:mortality_phase, ~replace_na(.x, "NA"))
)

wca_CHIPC_nov2021_projected <- wca_CHIPC_nov2021_projected %>% mutate(across(foodconsumption_phase:mortality_phase, ~replace_na(.x, "NA"))
)




#plot parameters
#CH color codes
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803", "NA" = "#ffffff")

# current_period <- c("October - December 2021 (current): ")
# projected_period <- c("June - September 2022 (projected): ")
# final_phase <- c("final phasing")
# fc_phase <- c("food consumption phasing")
# lh_phase <- c("livelihood phasing")
# nut_phase <- c("nutrition phasing")
# mort_phase <- c("mortality phasing")


#trying to purrr
#how to add thicker country line

countries <- unique(wca_CHIPC_nov2021_current$adm0_name)

map_ch_adm2 <- function(x, country, list, type = c("current", "projected")) {
  type <- match.arg(type)
  maps <- purrr::map(countries, ~ filter(list[[type]], adm0_name == (.x)) %>%
                       ggplot() + 
                       geom_sf(aes(fill = as.factor({{x}})), linetype = 2, size = 0.175) +
                       geom_sf(data = filter(wca_shp0all, admin0Name == .x), fill = NA) +
                       scale_fill_manual(values = CH_colors) +
                       theme_void() +
                       coord_sf(datum = NA) +
                       guides(fill = guide_legend(override.aes = list(linetype = 0))) +
                       labs(fill="phasing", title = glue::glue("{type}: {quo_name(enquo(x))}, {.x}"))) 
}

data <- list(current = wca_CHIPC_nov2021_current,
             projected = wca_CHIPC_nov2021_projected)


mapss <- map(c("current", "projected"), 
             ~ map_ch_adm2(x = phase_class, country = countries, list = data, type = .x))

maps <- unlist(mapss, recursive = FALSE)



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

#export 2 powerpoint
#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
#listofplots <- mget(ls(pattern = "map$"))

#create_dml, converts the ggplot objects to dml objects.
create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}
#Apply this function to the list of ggplot objects to create a list of dml objects with the same dimension.
plots_dml <- purrr::map(maps, create_dml)
##now fire away!
purrr::map(
  # dml plots to export ----
  plots_dml, 
  # exporting function ----
  create_pptx,   
  # additional fixed arguments in create_pptx ----
  path = "analysis\\powerpoint\\maps\\country_mapsNov2021_deez.pptx"
)

