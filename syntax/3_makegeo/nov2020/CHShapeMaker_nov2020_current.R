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
cadre_harmonise_caf_ipc_filtered <- cadre_harmonise_caf_ipc %>% 
  filter(exercise_year == 2020 & exercise_label == "Sep-Dec" & chtype == "current") 
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
ner_adm3 <- read_sf("data/geo/adm3/NER/NER_adm03_feb2018.shp") 
ner_adm3 <- ner_adm3 %>% mutate(adm0_name = "Niger") %>% dplyr::select(adm0_name, adm0_pcod2 = ISO2, adm1_name = adm_01, adm1_pcod2 = rowcacode1, adm2_name = adm_02, adm2_pcod2 = rowcacode2, adm3_pcod2 = rowcacode3, adm3_name = adm_03, Shape_Leng, Shape_Area, geometry)
#adm3 - mali
mli_adm3 <- read_sf("data/geo/adm3/MLI/mli_admbnda_adm3_1m_dnct_20190802.shp") 
mli_adm3 <- mli_adm3 %>% dplyr::select(adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod, adm3_name = admin3Name, adm3_pcod2 = admin3Pcod, Shape_Leng, Shape_Area, geometry) %>% mutate(adm0_name = "Mali")



#0.5 areas
cadre_harmonise_caf_ipc_adm0_5 %>% count(adm0_name)

#adm1 areas
cadre_harmonise_caf_ipc_adm1 %>% count(adm0_name)
listofadm1curr <- pull(cadre_harmonise_caf_ipc_adm1["adm1_pcod2"])
wca_adm1_filtered <- wca_shp1_filtered   %>% filter(adm1_pcod2 %in% listofadm1curr)

#adm 1.5 areas - Mauritania 
cadre_harmonise_caf_ipc_adm1_5 %>% count(adm1_5_name)
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
mrt_1_5$Shape_Area <- as.numeric(mrt_1_5$Shape_Area)

#adm2 areas 
cadre_harmonise_caf_ipc_adm2 %>% count(adm0_name)
listofadm2curr <- pull(cadre_harmonise_caf_ipc_adm2["adm2_pcod2"])
wca_adm2_filtered <- wca_shp2_filtered %>% filter(adm2_pcod2 %in% listofadm2curr)


#2.5 - Niger 
cadre_harmonise_caf_ipc_adm2_5 %>% count(adm0_name)
#Niger - creating special adm2 codes using accesible vs limited acess adm3 areas Niger
ner_adm3$adm3_name <- iconv(str_to_title(ner_adm3$adm3_name), from = "UTF-8", to = "ASCII//TRANSLIT")
ner_adm3$adm2_name <- iconv(str_to_title(ner_adm3$adm2_name), from = "UTF-8", to = "ASCII//TRANSLIT")
ner_adm2_5 <- ner_adm3 %>%  filter(adm2_name %in% c("Dogondoutchi", "Guidan Roumdji","Madarounfa","Ouallam","Tera","Tillaberi")) 
ner_adm2_5 <- ner_adm2_5 %>%  mutate(adm2_5_pcod2 = case_when(
  adm3_name %in% c("Soucoucoutane", "Dogonkiria") ~ "NE0303_al",
  adm3_name %in% c("Matankari", "Dogondoutchi", "Dan-Kassari", "Kieche") ~ "NE0303_a",
  adm3_name %in% c("Guidan Roumdji", "Guidan Sori") ~ "NE0405_al",
  adm3_name %in% c("Tibiri", "Chadakori", "Sae Saboua") ~ "NE0405_a",
  adm3_name %in% c("Gabi","Safo", "Madarounfa") ~ "NE0406_al",
  adm3_name %in% c("Sarkin Yamma", "Dan-Issa", "Djiratawa") ~ "NE0406_a",
  adm3_name %in% c("Tondikiwindi") ~ "NE0609_al",
  adm3_name %in% c("Simiri", "Ouallam", "Dingazi") ~ "NE0609_a", 
  adm3_name %in% c("Anzourou") ~ "NE0612_al",
  adm3_name %in% c("Dessa", "Sinder", "Sakoira", "Bibiyergou", "Tillaberi", "Kourteye") ~ "NE0612_a", 
  adm3_name %in% c("Gorouol") ~ "NE0611_al", 
  adm3_name %in% c("Kokorou","Tera","Mehana","Diagourou") ~ "NE0611_a"))  
#combine together at adm2 level
ner_adm2_5  <- ner_adm2_5 %>%  
  group_by(adm0_name, adm0_pcod2, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, adm2_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() 

#admin3 - Mali 
cadre_harmonise_caf_ipc_adm3 %>% count(adm0_name)
mli_adm3 <- mli_adm3 %>% filter(adm2_name == "Bamako") 


#add together all current 
wca_CH <- bind_rows(wca_adm1_filtered, mrt_1_5, wca_adm2_filtered, ner_adm2_5, mli_adm3) 
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
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(mapthis = "yes")


##add map this 
#testmap
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
#finalphases
map <- ggplot()  +geom_sf(data = wca_final_CH_FINAL, mapping = aes(fill = as.factor(phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +ggtitle(label= "novch 2021 (projent) final area phasing") +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)


#fix and reduce size
wca_CHIPC_nov2020_cur_nov2020 <- st_make_valid(wca_final_CH_FINAL)
wca_CHIPC_nov2020_cur_nov2020_simple <- rmapshaper::ms_simplify(wca_CHIPC_nov2020_cur_nov2020, keep_shapes = TRUE) # simplify polygons
### Save it as .gpkg file
st_write(wca_CHIPC_nov2020_cur_nov2020, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2020_current_nov2020.gpkg", driver="GPKG", append = T) 
st_write(wca_CHIPC_nov2020_cur_nov2020_simple, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_nov2020_current_nov2020_simple.gpkg", driver="GPKG", append = T) 




