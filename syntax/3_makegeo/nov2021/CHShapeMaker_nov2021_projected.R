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
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc.xlsx", guess_max = 21474836)
#split into current  different administrative levels
#current
cadre_harmonise_caf_ipc_filtered <- cadre_harmonise_caf_ipc %>% filter(exercise_year == 2021 & exercise_label == "Sep-Dec" & chtype == "projected") 
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
#but take out Sierra Leone because have to use new admin areas
wca <- c("Burkina Faso","Benin","Cameroon","Central African Republic","Chad","Côte d'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Togo")

#import shapefiles at adm1, adm2, adm3 (all files taken from HDX website)
#adm0 
wca_shp0_filtered <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") %>% filter(admin0Name %in% wca)
#adm1
wca_shp1_filtered <- read_sf("data\\geo\\adm1\\wca_admbnda_adm1_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod) %>% filter(adm0_name %in% wca)
#adm1.5
wca_shp1_5_filtered <- read_sf("data\\geo\\adm1_5\\nga_admbnda_senDist_inec_osgof_20190417.shp") %>% 
rename(adm0_name = ADM0_EN, adm0_pcod2 = ADM0_PCODE, adm1_name = ADM1_EN, adm1_pcod2 = ADM1_PCODE, adm1_5_name = SD_EN, adm1_5_pcod2 = SD_PCODE) 

#adm2 and get rid of special areas in Mali / Niger so they can be created/added seperately
wca_shp2_filtered <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod) %>% filter(adm0_name %in% wca)
#sierra leone adm2
wca_sle_filtered <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_29062021.shp")%>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod) %>% filter(adm0_name == "Sierra Leone")

#adm2.5
mli_adm3 <- read_sf("data/geo/adm3/MLI/mli_admbnda_adm3_1m_dnct_20190802.shp") 
mli_adm3 <- mli_adm3 %>% dplyr::select(adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod, adm3_name = admin3Name, adm3_pcod2 = admin3Pcod, Shape_Leng, Shape_Area, geometry) %>% mutate(adm0_name = "Mali")
#adm3 - niger
ner_adm3 <- read_sf("data/geo/adm3/NER/NER_adm03_feb2018.shp") 
ner_adm3 <- ner_adm3 %>% mutate(adm0_name = "Niger") %>% dplyr::select(adm0_name, adm0_pcod2 = ISO2, adm1_name = adm_01, adm1_pcod2 = rowcacode1, adm2_name = adm_02, adm2_pcod2 = rowcacode2, adm3_pcod2 = rowcacode3, adm3_name = adm_03, Shape_Leng, Shape_Area, geometry)


#0.5 areas
cadre_harmonise_caf_ipc_adm0_5 %>% count(adm0_name)

#adm1 areas
cadre_harmonise_caf_ipc_adm1 %>% count(adm0_name)
listofadm1curr <- pull(cadre_harmonise_caf_ipc_adm1["adm1_pcod2"])
wca_adm1_filtered <- wca_shp1_filtered   %>% filter(adm1_pcod2 %in% listofadm1curr)

#adm 1.5 areas - Nigeria 
cadre_harmonise_caf_ipc_adm1_5 %>% count(adm1_5_name)
#Nigeria - Abia and Sokoto IDPs
wca_1_5_filtered <- wca_shp1_5_filtered %>% filter(adm1_name == "Abia") %>% select(adm1_5_pcod2,Shape_Area,geometry)




#adm2 areas - 
#alll without Sierra Leone
cadre_harmonise_caf_ipc_adm2 %>% count(adm0_name)
listofadm2curr <- pull(cadre_harmonise_caf_ipc_adm2["adm2_pcod2"])
wca_adm2_filtered <- wca_shp2_filtered %>% filter(adm2_pcod2 %in% listofadm2curr)
#add Sierra Leone
wca_adm2_filtered <- bind_rows(wca_adm2_filtered, wca_sle_filtered)


#2.5 - 
#Agadez, Diffa, Dosso, Tillaberi 
#Guidan Roumdji, Madarounfa, Ouallam, Tera, Tillaberi 
#Niger - creating special adm2 codes using accesible vs limited acess adm3 areas Niger and ville areas
ner_adm2_5 <- ner_adm3 %>%  filter(adm2_name %in% c("Tchirozerine","Dosso","Diffa","Guidan Roumdji","Madarounfa","Ouallam","Téra","Tillabéri")) %>%mutate(adm2_5_pcod2 = case_when(
    #innaccessible areas / 
    adm3_name %in% c("Chadakori", "Saé Saboua") ~ "NE0405_a", #Guidan Roumdji - same as March 2021
    adm3_name %in% c("Guidan Roumdji", "Guidan Sori", "Tibiri") ~ "NE0405_al",
    adm3_name %in% c("Dan-Issa", "Djiratawa") ~ "NE0406_a", #Madarounfa - same as March 2021
    adm3_name %in% c("Gabi","Safo", "Madarounfa","Sarkin Yamma") ~ "NE0406_al",
    adm3_name %in% c("Simiri","Ouallam") ~ "NE0609_a", #Ouallam - changed from  March 2021
    adm3_name %in% c("Dingazi","Tondikiwindi") ~ "NE0609_al",
    adm3_name %in% c("Kokorou","Méhana","Téra") ~ "NE0611_a", #Tera - same as March 2021
    adm3_name %in% c("Diagourou","Gorouol") ~"NE0611_al",
    adm3_name %in% c("Anzourou") ~ "NE0612_al", #Tillaberi - same as March 2021 - but excludes Tillaberi commune
    adm3_name %in% c("Dessa", "Sinder", "Sakoïra", "Bibiyergou", "Kourteye") ~ "NE0612_a",
    #department areas
    adm3_name %in% c("Tchirozerine", "Dabaga", "Tabelot") ~ "NE0106_dep",
    adm3_name %in% c("Chetimari","Gueskérou") ~ "NE0202_dep",
    adm3_name %in% c("Farey","Garankédey","Gollé","Goroubankassam","Karguibangou","Mokko","Sambéra","Tessa","Tombokoirey I","Tombokoirey II") ~ "NE0304_dep"))
#combine together at adm2 level
ner_adm2_5  <- ner_adm2_5 %>%  
  group_by(adm0_name, adm0_pcod2, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, adm2_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() %>% filter(!is.na(adm2_5_pcod2))


#admin3 - Mali & Niger
cadre_harmonise_caf_ipc_adm3 %>% count(adm0_name)
#niger - villes
ner_adm3_villes <- ner_adm3 %>% filter(adm3_name %in% c("Agadez","Diffa", "Dosso", "Tillabéri")) %>%
  mutate(adm3_pcod2 = case_when(
    adm3_name == "Agadez" ~ "NE010601",
    adm3_name == "Diffa" ~ "NE002002",
    adm3_name == "Dosso" ~ "NE003004",
    adm3_name == "Tillabéri" ~ "NE006012"))
#mali
mli_adm3 <- mli_adm3 %>% filter(adm2_name == "Bamako") 
#together
wca_adm3 <- bind_rows(ner_adm3_villes, mli_adm3)

#add together all 
wca_CH <- bind_rows(wca_adm1_filtered, wca_adm2_filtered, ner_adm2_5) 
wca_CH <- wca_CH %>% dplyr::select(-date, OBJECTID, -adm0_name, -adm1_name, -adm2_name)
wca_CH_nga <- wca_CH %>% filter(adm0_pcod2 == "NG")
wca_CH_nonga <- wca_CH %>% filter(adm0_pcod2 != "NG")
# join things
wca_final_CH <- inner_join(wca_CH_nonga, cadre_harmonise_caf_ipc_filtered,  by = c("adm0_pcod2","adm1_pcod2","adm2_pcod2","adm2_5_pcod2"))

#Nigeria didnt join so do this one specifically and differently
#Nigeria adm2
wca_CH_nga <- wca_CH %>% filter(adm0_pcod2 == "NG") %>% dplyr::select(-adm0_pcod2,-adm2_5_pcod2)
wca_final_CH_nga <- inner_join(wca_CH_nga, cadre_harmonise_caf_ipc_filtered,  by = c("adm1_pcod2","adm2_pcod2"))
#Nigeria adm1_5 abia
wca_1_5_filtered_joined <- inner_join(wca_1_5_filtered, cadre_harmonise_caf_ipc_filtered,  by = c("adm1_5_pcod2"))
#bind
wca_CH_nga <- bind_rows(wca_final_CH_nga, wca_1_5_filtered_joined)

#Niger didnt join so do this also differently
wca_CH_ner <- ner_adm2_5 %>% filter(adm0_pcod2 == "NE" & !is.na(adm2_5_pcod2)) %>% dplyr::select(-adm0_pcod2,-adm1_pcod2,-adm2_pcod2, -adm0_name,-adm1_name, -adm2_name)
wca_final_CH_ner <- inner_join(wca_CH_ner, cadre_harmonise_caf_ipc_filtered,  by = c("adm2_5_pcod2"))

#adm3
wca_adm3 <- wca_adm3 %>% dplyr::select(adm3_pcod2, Shape_Leng, Shape_Area, geometry)
wca_adm3_final <- inner_join(wca_adm3, cadre_harmonise_caf_ipc_filtered,  by = c("adm3_pcod2"))

wca_final_CH_FINAL <- bind_rows(wca_final_CH,wca_CH_nga, wca_final_CH_ner,wca_adm3_final)


wca_final_CH_FINAL <- wca_final_CH_FINAL %>% dplyr::select(adm0_name, adm0_pcod3, adm0_pcod2, 
                                                                     adm0_5_name,	adm0_5_pcod2,
                                                                     adm1_name, adm1_pcod2,
                                                                     adm1_5_pcod2, adm1_5_pcod2, 
                                                                     adm2_name, adm2_pcod2,
                                                                     adm2_5_name, adm2_5_pcod2,
                                                                     adm3_name, adm3_pcod2,
                                                                     everything(),
                                                                     Shape_Leng, Shape_Area, Shape_Area,
                                                                     -OBJECTID, -usethisperiod)


#create end and start date
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = "2022-06-01", end_date = "2022-09-30") 
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))


##add map this 

#testmap
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
map_phase_classtest2 <- ggplot()  +geom_sf(data = wca_final_CH_FINAL, mapping = aes(fill = as.factor(phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) final area phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#foodconsumption
map_foodconsumption_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(foodconsumption_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="food consumption outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) food consumption outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#livelihoods
map_livelihoods_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(livelihoods_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="livelihoods outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) livelihood outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#nutrition
map_nutrition_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(nutrition_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="nutrition outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) nutrition outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#mortality
map_mortality_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(mortality_phase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) mortality outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)
#FCS
map_fcs_phase <- wca_final_CH_FINAL %>% ggplot() +geom_sf(aes(fill = as.factor(FCG_finalphase))) +theme_void() +coord_sf(datum=NA) +labs(fill="mortality outcome phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "March 2021 (current) mortality outcome phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)


#fix and reduce size
wca_CHIPC_nov2021_projected_jun2022 <- st_make_valid(wca_final_CH_FINAL)
wca_CHIPC_nov2021_projected_jun2022_simple <- rmapshaper::ms_simplify(wca_final_CH_FINAL, keep_shapes = TRUE) # simplify polygons


#I hate how this is a manual step, but I'm in a hurry so do like this for now
withoutgeo <- cadre_harmonise_caf_ipc_filtered %>% filter(adm1_5_name == "Sokoto-Idps" | adm2_name == "Taoudenit")

wca_CHIPC_nov2021_projected_jun2022_simple <- bind_rows(wca_CHIPC_nov2021_projected_jun2022_simple, withoutgeo)

wca_CHIPC_nov2021_projected_jun2022_simple <- wca_CHIPC_nov2021_projected_jun2022_simple %>% select(-usethisperiod)


### Save it as .gpkg file
st_write(wca_CHIPC_nov2021_projected_jun2022_simple , "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2021_projected_jun2022_simple.geojson", driver="GeoJSON", append = T) 
#st_write(wca_CHIPC_nov2021_projected_jun2021_simple , "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2021_projected_jun2021_simple.gpkg", driver="GPKG", append = T) 

