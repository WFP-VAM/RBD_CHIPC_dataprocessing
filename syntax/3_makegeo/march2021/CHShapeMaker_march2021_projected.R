library(tidyverse)
library(sf)
library(googlesheets4)
library(rgdal)
library(janitor)
library(rmapshaper)
library(readxl)
library(mapview)
library(roperators)

#add CH data
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc_test.xlsx", guess_max = 21474836)
#split into current  different administrative levels
#projected
cadre_harmonise_caf_ipc_filtered <- cadre_harmonise_caf_ipc %>% 
  filter(exercise_year == 2021 & exercise_label == "Jan-May" & chtype == "projected") 
#areas analyzed at the adm0.5 level
#areas analyzed at the adm0.5 level
cadre_harmonise_caf_ipc_adm0_5 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(is.na(adm1_name) & is.na(adm1_5_name) & is.na(adm2_name) & is.na(adm2_5_name) & is.na(adm3_name))  
#areas analyzed at the adm1 level
cadre_harmonise_caf_ipc_adm1 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(is.na(adm1_5_name) & is.na(adm2_name) & is.na(adm2_5_name) & is.na(adm3_name))  
#areas analyzed at the adm1.5 level
cadre_harmonise_caf_ipc_adm1_5 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(!is.na(adm1_5_name) & is.na(adm2_name) & is.na(adm2_5_name) & is.na(adm3_name))  
#areas analyzed at the adm2 level
cadre_harmonise_caf_ipc_adm2 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(!is.na(adm2_name) & is.na(adm2_5_name) & is.na(adm3_name))  
#areas analyzed at the adm2.5 level
cadre_harmonise_caf_ipc_adm2_5 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(!is.na(adm2_5_name) & is.na(adm3_name))  
#areas analyzed at the adm3 level
cadre_harmonise_caf_ipc_adm3 <- cadre_harmonise_caf_ipc_filtered %>% 
  filter(!is.na(adm3_name))   

#create a vector of countries in West/Central Africa CH/IPC process (include all countries even if no data)
wca <- c("Benin","Burkina Faso","Cameroon","Central African Republic","Chad","Côte d'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo")

#import shapefiles at adm1, adm2, adm3 (all files taken from HDX website)
#adm0 
wca_shp0_filtered <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") %>% filter(admin0Name %in% wca)
#adm1
wca_shp1_filtered <- read_sf("data\\geo\\adm1\\wca_admbnda_adm1_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod) %>% filter(adm0_name %in% wca)
#adm2 and get rid of special areas in Mali / Niger / Liberia / Mauritania so they can be created/added seperately
wca_shp2_filtered <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod) %>% filter(adm0_name %in% wca)
#adm2.5
mli_adm3 <- read_sf("data/geo/adm3/MLI/mli_admbnda_adm3_1m_dnct_20190802.shp") 
mli_adm3 <- mli_adm3 %>% dplyr::select(adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod, adm3_name = admin3Name, adm3_pcod2 = admin3Pcod, Shape_Leng, Shape_Area, geometry) %>% mutate(adm0_name = "Mali")
#adm3 - mali
ner_adm3 <- read_sf("data/geo/adm3/NER/NER_adm03_feb2018.shp") 
ner_adm3 <- ner_adm3 %>% mutate(adm0_name = "Niger") %>% dplyr::select(adm0_name, adm0_pcod2 = ISO2, adm1_name = adm_01, adm1_pcod2 = rowcacode1, adm2_name = adm_02, adm2_pcod2 = rowcacode2, adm3_pcod2 = rowcacode3, adm3_name = adm_03, Shape_Leng, Shape_Area, geometry)
#CAR
car_adm3 <- read_sf("data/geo/adm3/CAF/caf_admbnda_adm3_200k_sigcaf_reach_itos_v2.shp") %>% select(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod, adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod, adm3_name = admin3Name, adm3_pcod2 = admin3Pcod, Shape_Leng, Shape_Area, geometry)



#0.5 areas
cadre_harmonise_caf_ipc_adm0_5 %>% count(adm0_name)

#adm1 areas
cadre_harmonise_caf_ipc_adm1 %>% count(adm0_name)
listofadm1curr <- pull(cadre_harmonise_caf_ipc_adm1["adm1_pcod2"])
wca_adm1_filtered <- wca_shp1_filtered   %>% filter(adm1_pcod2 %in% listofadm1curr)

#adm 1.5 areas - Mauritania & Liberia
cadre_harmonise_caf_ipc_adm1_5 %>% count(adm1_5_name)
#Liberia
lbr_1_5 <- wca_shp2_filtered %>% filter(adm1_name == "Montserrado") %>% mutate(adm1_5_pcod2 = case_when(
  adm2_name %in% c("Greater Monrovia") ~ "LBR011_u",
  adm2_name %in% c("Careysburg","Commonwealth1","St. Paul River","Todee") ~ "LBR011_r"))
#create shape area 
lbr_1_5$Shape_Area <- st_area(lbr_1_5)    
#combine together at adm2 level
lbr_1_5 <- lbr_1_5 %>%  
  group_by(adm0_name,  adm0_pcod2, adm1_name, adm1_pcod2, adm1_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) 
###Mauritania - nouakchott and chami/noudibhou
mrt_1_5 <- wca_shp2_filtered %>% filter(adm1_name %in% c("Nouakchott")) %>%
  mutate(adm1_5_pcod2 = case_when(
    adm2_name %in% c("Arafat","El Mina","Riad") ~ "MRT010_sud",
    adm2_name %in% c("Ksar","Sebkha","TevraghZeina") ~ "MRT010_ouest",
    adm2_name %in% c("Dar Naim","Teyarett","Toujounine") ~ "MRT010_nord"))
    #create shape area 
mrt_1_5$Shape_Area <- st_area(mrt_1_5) 
#combine together at adm2 level
mrt_1_5 <- mrt_1_5  %>%  
  group_by(adm0_name,  adm0_pcod2, adm1_name, adm1_pcod2, adm1_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) 
#bind
wca_1_5_filtered <- bind_rows(lbr_1_5, mrt_1_5) 
wca_1_5_filtered$Shape_Area <- as.numeric(wca_1_5_filtered$Shape_Area)

#adm2 areas 
cadre_harmonise_caf_ipc_adm2 %>% count(adm0_name)
listofadm2curr <- pull(cadre_harmonise_caf_ipc_adm2["adm2_pcod2"])
wca_adm2_filtered <- wca_shp2_filtered %>% filter(adm2_pcod2 %in% listofadm2curr)


#2.5 - Mali & Niger - why no 
cadre_harmonise_caf_ipc_adm2_5 %>% count(adm0_name)
##Mali - creating special adm2 codes using accesible vs limited acess adm3 areas and Bamako communes - Mali
mli_2_5 <- mli_adm3 %>% filter(adm2_name %in% c("Koro")) %>%  mutate(adm2_5_pcod2 = case_when(
  adm3_name  %in% c("Koro","Barapireli","Diougani","Dougouténé I","Dougouténé II","Koporo Pen","Koporokendie Na","Pel Maoude","Youdiou") ~ "ML0505_a",
  adm3_name %in% c("Bamba","Bondo","Diankabou","Dinangourou","Kassa","Madougou","Yoro") ~ "ML0505_al"))
#combine together at adm2 level
mli_2_5  <- mli_2_5  %>%  
  group_by(adm0_name, adm0_pcod2, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, adm2_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() 
#Niger - creating special adm2 codes using accesible vs limited acess adm3 areas Niger
ner_adm2_5 <- ner_adm3 %>%  filter(adm2_name %in% c("Dogondoutchi", "Guidan Roumdji","Madarounfa","Tahoua","Filingué","Ouallam","Say","Téra","Tillabéri")) %>% 
  mutate(adm2_5_pcod2 = case_when(
    adm3_name %in% c("Matankari", "Dogondoutchi", "Dan-Kassari", "Kiéché") ~ "NE0303_a", #Dogondoutchi - same as Nov 2020
    adm3_name %in% c("Soucoucoutane", "Dogonkiria") ~ "NE0303_al",
    adm3_name %in% c("Chadakori", "Saé Saboua") ~ "NE0405_a", #Guidan Roumdji - changed from Nov 2020
    adm3_name %in% c("Guidan Roumdji", "Guidan Sori", "Tibiri") ~ "NE0405_al",
    adm3_name %in% c("Dan-Issa", "Djiratawa") ~ "NE0406_a", #Madarounfa - changed from Nov 2020
    adm3_name %in% c("Gabi","Safo", "Madarounfa","Sarkin Yamma") ~ "NE0406_al",
    adm3_name %in% c("Kalfou", "Barmou", "Bambeye", "Affala") ~ "NE0509_a", #Tahoua changed from March 2019
    adm3_name %in% c("Tebaram","Takanamat") ~ "NE0509_al",
    adm3_name %in% c("Filingué","Kourfeye Centre") ~ "NE0606_a", #Filingue- first time split into accessible non accessible
    adm3_name %in% c("Imanan", "Tondikandia") ~ "NE0606_al",
    adm3_name %in% c("Simiri") ~ "NE0609_a", #Ouallam
    adm3_name %in% c("Dingazi", "Ouallam","Tondikiwindi") ~ "NE0609_al",
    adm3_name %in% c("Say","Ouro Guéladjo") ~ "NE0610_a", #Say
    adm3_name %in% c("Tamou") ~ "NE0610_al",
    adm3_name %in% c("Kokorou","Méhana","Téra") ~ "NE0611_a", #Tera - changed since nov2020
    adm3_name %in% c("Diagourou","Gorouol") ~"NE0611_al",
    adm3_name %in% c("Anzourou") ~ "NE0612_al", #Tillaberi - same as before
    adm3_name %in% c("Dessa", "Sinder", "Sakoïra", "Bibiyergou", "Tillabéri", "Kourteye") ~ "NE0612_a"))
#combine together at adm2 level
ner_adm2_5  <- ner_adm2_5 %>%  
  group_by(adm0_name, adm0_pcod2, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, adm2_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() 
#bind
wca_2_5_filtered <- bind_rows(mli_2_5, ner_adm2_5)

#admin3 - Mali & CAR
cadre_harmonise_caf_ipc_adm3 %>% count(adm0_name)
mli_adm3 <- mli_adm3 %>% filter(adm2_name == "Bamako") 
#fix adm2 pcod for Bangui
car_adm3 <- car_adm3 %>% filter(adm2_name == "Bangui") %>% mutate(adm2_pcod2 = "CF7101")

#add together all current 
wca_CH <- bind_rows(wca_adm1_filtered, wca_1_5_filtered, wca_adm2_filtered, wca_2_5_filtered, mli_adm3, car_adm3) 
wca_CH <- wca_CH %>% dplyr::select(-date, OBJECTID, -adm0_name, -adm1_name, -adm2_name, -adm3_name)
wca_CH_nga <- wca_CH %>% filter(adm0_pcod2 == "NG")
wca_CH_nonga <- wca_CH %>% filter(adm0_pcod2 != "NG")
# join things
wca_final_CH <- inner_join(wca_CH_nonga, cadre_harmonise_caf_ipc_filtered,  by = c("adm0_pcod2","adm1_pcod2","adm1_5_pcod2","adm2_pcod2","adm2_5_pcod2","adm3_pcod2"))

#Nigeria didnt join so do this one specifically and differently
wca_CH_nga <- wca_CH %>% filter(adm0_pcod2 == "NG") %>% dplyr::select(-adm0_pcod2,-adm1_5_pcod2,-adm2_5_pcod2,-adm3_pcod2)
wca_final_CH_nga <- inner_join(wca_CH_nga, cadre_harmonise_caf_ipc_filtered,  by = c("adm1_pcod2","adm2_pcod2"))

#Niger didnt join so do this also differently
wca_CH_ner <- ner_adm2_5 %>% filter(adm0_pcod2 == "NE" & !is.na(adm2_5_pcod2)) %>% dplyr::select(-adm0_pcod2,-adm1_pcod2,-adm2_pcod2, -adm0_name,-adm1_name, -adm2_name)
wca_final_CH_ner <- inner_join(wca_CH_ner, cadre_harmonise_caf_ipc_filtered,  by = c("adm2_5_pcod2"))


wca_final_CH_FINAL <- bind_rows(wca_final_CH,wca_final_CH_nga, wca_final_CH_ner)


wca_final_CH_FINAL <- wca_final_CH_FINAL %>% dplyr::select(adm0_name, adm0_pcod3, adm0_pcod2, 
                                                                     adm0_5_name,	adm0_5_pcod2,
                                                                     adm1_name, adm1_pcod2,
                                                                     adm1_5_pcod2, adm1_5_pcod2, 
                                                                     adm2_name, adm2_pcod2,
                                                                     adm2_5_name, adm2_5_pcod2,
                                                                     adm3_name, adm3_pcod2,
                                                                     everything(),
                                                                     Shape_Leng, Shape_Area, Shape_Area,
                                                                     -adm0_gaulcode, -OBJECTID, -usethisperiod)
#create map this period
wca_final_CH_FINAL <- wca_final_FINAL %>% mutate(mapthis = case_when(
  adm2_5_name %in% c("Daura-Idps","Funtua-Idps","Katsina-Idps")  ~ "no",
  adm1_5_name == "Sokoto-Idps" ~ "no",
  TRUE ~ "yes"))

#create end and start date
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = "2021-06-01", end_date = "2021-09-30") 
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))



##add map this 
#testmap
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
mapprojentmar2021_phase_classtest2 <- ggplot()  +geom_sf(data = wca_final_CH_FINAL, mapping = aes(fill = as.factor(phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) final area phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#foodconsumption
mapprojentmar2021_foodconsumption_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="food consumption outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) food consumption outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#livelihoods
mapprojentmar2021_livelihoods_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="livelihoods outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) livelihood outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#nutrition
mapprojentmar2021_nutrition_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(nutrition_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="nutrition outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) nutrition outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#mortality
mapprojentmar2021_mortality_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(mortality_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) mortality outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#FCS
mapprojentmar2021_fcs_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(FCG_finalphase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (projent) mortality outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)


#fix and reduce size
wca_CHIPC_mar2021_proj_jun2021 <- st_make_valid(wca_final_CH_FINAL)
wca_CHIPC_mar2021_proj_jun2021_simple <- rmapshaper::ms_simplify(wca_CHIPC_mar2021_proj_jun2021, keep_shapes = TRUE) # simplify polygons
### Save it as .geojson file
#st_write(wca_CHIPC_mar2021_proj_jun2021, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2021_projected_jun2021.geojson", driver="GeoJSON", append = T) 
st_write(wca_CHIPC_mar2021_proj_jun2021_simple, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2021_projected_jun2021_simple.geojson", driver="GeoJSON", append = T) 



