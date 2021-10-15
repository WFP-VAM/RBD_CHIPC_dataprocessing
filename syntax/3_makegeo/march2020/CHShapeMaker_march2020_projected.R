library(tidyverse)
library(sf)
library(googlesheets4)
library(rgdal)
library(janitor)
library(rmapshaper)
library(readxl)
library(mapview)
library(roperators)
library(mapedit)

#add CH data
cadre_harmonise_caf_ipc <- read_excel("data/processed/cadre_harmonise_caf_ipc.xlsx", guess_max = 21474836)
#split into current  different administrative levels
#Make sure to select the right exercise period and projection 
cadre_harmonise_caf_ipc_filtered <- cadre_harmonise_caf_ipc %>% 
  filter(exercise_year == 2020 & reference_label == "Jun-Aug" & usethisperiod == "Y") 

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
wca <- c("Benin","Burkina Faso","Cabo Verde","Cameroon","Central African Republic","Chad","Côte d'Ivoire","Gambia","Ghana","Guinea","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo")

#import shapefiles at adm1, adm2, adm3 (all files taken from HDX website)
#adm0 
wca_shp0_filtered <- read_sf("data\\geo\\adm0\\wca_admbnda_adm0_ocha_18022021.shp") %>% filter(admin0Name %in% wca)
#adm1
wca_shp1_filtered <- read_sf("data\\geo\\adm1\\wca_admbnda_adm1_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod) %>% filter(adm0_name %in% wca)
#adm1_5
wca_NGA_sen <- read_sf("data\\geo\\special_areas\\nga_admbnda_senDist_inec_osgof_20190417.shp") %>% dplyr::select(adm0_name = ADM0_EN,adm0_pcod2 = ADM0_PCODE,adm1_name = ADM1_EN,adm1_pcod2 = ADM1_PCODE,adm1_5_name = SD_EN,adm1_5_pcod2 = SD_PCODE,Shape_Leng,Shape_Area,geometry)
#adm2 
wca_shp2_filtered <- read_sf("data\\geo\\adm2\\wca_admbnda_adm2_ocha_18022021.shp") %>% rename(adm0_name = admin0Name, adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod) %>% filter(adm0_name %in% wca)
#adm2.5 - Niger
ner_adm3 <- read_sf("data/geo/adm3/NER/NER_adm03_feb2018.shp") 
ner_adm3 <- ner_adm3 %>% mutate(adm0_name = "Niger") %>% dplyr::select(adm0_name, adm0_pcod2 = ISO2, adm1_name = adm_01, adm1_pcod2 = rowcacode1, adm2_name = adm_02, adm2_pcod2 = rowcacode2, adm3_pcod2 = rowcacode3, adm3_name = adm_03, Shape_Leng, Shape_Area, geometry)
#adm3 - mali
mli_adm3 <- read_sf("data/geo/adm3/MLI/mli_admbnda_adm3_1m_dnct_20190802.shp") 
mli_adm3 <- mli_adm3 %>% dplyr::select(adm0_pcod2 = admin0Pcod , adm1_name = admin1Name, adm1_pcod2 = admin1Pcod, adm2_name = admin2Name, adm2_pcod2 = admin2Pcod, adm3_name = admin3Name, adm3_pcod2 = admin3Pcod, Shape_Leng, Shape_Area, geometry) %>% mutate(adm0_name = "Mali")


#check thorough and recode areas by admin level

#0.5 areas
cadre_harmonise_caf_ipc_adm0_5 %>% count(adm0_name)

#adm1 areas
cadre_harmonise_caf_ipc_adm1 %>% count(adm0_name)
listofadm1curr <- pull(cadre_harmonise_caf_ipc_adm1["adm1_pcod2"])
wca_adm1_filtered <- wca_shp1_filtered   %>% filter(adm1_pcod2 %in% listofadm1curr)

#adm1.5 areas
cadre_harmonise_caf_ipc_adm1_5 %>% count(adm1_5_name)
#Nigeria
wca_NGA_sen_filtered <- wca_NGA_sen %>% filter(adm1_name == "Kano")
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
mrt_1_5$Shape_Area <- as.numeric(mrt_1_5$Shape_Area)




#adm2 areas
cadre_harmonise_caf_ipc_adm2 %>% count(adm0_name)
listofadm2curr <- pull(cadre_harmonise_caf_ipc_adm2["adm2_pcod2"])
wca_adm2_filtered <- wca_shp2_filtered %>% filter(adm2_pcod2 %in% listofadm2curr)

#2.5 - Niger
cadre_harmonise_caf_ipc_adm2_5 %>% count(adm0_name)
#Niger - creating special adm2 codes using accesible vs limited acess adm3 areas Niger
ner_adm3$adm2_name <- iconv(str_to_title(ner_adm3$adm2_name), from = "UTF-8", to = "ASCII//TRANSLIT")
ner_adm3$adm3_name <- iconv(str_to_title(ner_adm3$adm3_name), from = "UTF-8", to = "ASCII//TRANSLIT")
ner_adm2_5 <- ner_adm3 %>%  filter(adm2_name %in% c("Dogondoutchi", "Guidan Roumdji","Madarounfa","Ouallam","Tahoua","Tera","Tillaberi")) 
ner_adm2_5 <- ner_adm2_5 %>%  mutate(adm2_5_pcod2 = case_when(
  adm3_name %in% c("Chetimari","Gueskerou") ~ "NE0202_al",
  adm3_name == "Diffa" ~ "NE0202_a",
  adm3_name %in% c("Soucoucoutane", "Dogonkiria") ~ "NE0303_al",
  adm3_name %in% c("Matankari", "Dogondoutchi", "Dan-Kassari", "Kieche") ~ "NE0303_a",
  adm3_name %in% c("Tibiri","Guidan Sori") ~ "NE0405_al",
  adm3_name %in% c("Guidan Roumdji", "Chadakori", "Sae Saboua") ~ "NE0405_a",
  adm3_name == "Gabi" ~ "NE0406_al",
  adm3_name %in% c("Sarkin Yamma", "Safo", "Madarounfa", "Dan-Issa", "Djiratawa") ~ "NE0406_a",
  adm3_name == "Tebaram" ~ "NE0509_al",
  adm3_name %in% c("Bambeye",  "Takanamat",  "Affala",  "Kalfou",  "Barmou") ~ "NE0509_a",
  adm3_name == "Tondikiwindi" ~ "NE0609_al",
  adm3_name %in% c("Simiri", "Ouallam", "Dingazi") ~ "NE0609_a",
  adm3_name == "Anzourou" ~ "NE0612_al",
  adm3_name %in% c("Dessa", "Sinder", "Sakoira", "Bibiyergou", "Tillaberi", "Kourteye") ~ "NE0612_a",
  adm3_name == "Gorouol" ~ "NE0611_al",
  adm3_name %in% c("Kokorou","Tera","Mehana","Diagourou") ~ "NE0611_a"))
# #create special area for Nguimi ville - NEED TO FIX THIS WITH A CLEAR HEAD
# ner_adm2_5_nguimi <- ner_adm3 %>%  filter(adm2_name == "N'guigmi") %>% 
#   group_by(adm2_name, adm2_pcod2) %>%
#   summarise(Shape_Area = sum(Shape_Area)) %>%
#   ungroup()
# ### custom create Nguimi ville and Nguimi AL
# tb <- filter(ner_adm3, adm3_name == "N'guigmi") %>%
#   slice(1) %>%
#   as_tibble() %>%
#   select(-geometry)
# #make nguimi ville
# nguigmi_coord <- c(13.110833, 14.252778)
# nguigmi_ville <- nguigmi_coord %>%
#   st_point() %>%
#   st_sfc(crs = 4326) %>%
#   st_sf(geometry = ., tb)
# ### Create a buffer ~ 5km around the area
# nguigmi_ville <- st_buffer(nguigmi_ville, dist = 5/120) %>% mutate(adm2_5name = "NE0206_a")
# nguigmi_adm2 <- filter(ner_adm2_5_nguimi, adm2_name == "N'Guigmi")
# #plot(st_geometry(nguigmi_adm2))
# plot(st_geometry(nguigmi_ville), add = TRUE)
# 
# nguigmi_limitedaccess <- st_difference(nguigmi_adm2, nguigmi_ville) %>%
#   select(-ends_with(".1"), -NOMDEP) %>% mutate(rowcacode2 = "NE0206_al")
# 
# plot(st_geometry(nguigmi_limitedaccess), col = "steelblue")
# mapview(nguimi_limitedaccess)
# 
# nguigmi_adm2_new <- rbind(nguigmi_ville, nguigmi_limitedaccess) %>% select(adm_02, Shape_Area, rowcacode2)
# mapview(nguigmi_adm2_new)
# 
# NER_CH_Mar2020 <- rbind(ner_adm2_new, nguigmi_adm2_new) %>% rename(adm2_pcod3 = rowcacode2)
# 
#combine together at adm2 level
ner_adm2_5  <- ner_adm2_5 %>%  
  group_by(adm0_name, adm0_pcod2, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, adm2_5_pcod2) %>%
  summarise(Shape_Area = sum(Shape_Area)) %>%
  ungroup() 

#admin3 - Mali 
cadre_harmonise_caf_ipc_adm3 %>% count(adm0_name)
mli_adm3 <- mli_adm3 %>% filter(adm2_name == "Bamako") 


#add together all layers and split Nigeria/Niger because it gets tricky
wca_CH <- bind_rows(wca_adm1_filtered, mrt_1_5, wca_adm2_filtered, ner_adm2_5, mli_adm3) 
wca_CH_nonga <- wca_CH %>% filter(adm0_pcod2 != "NG") %>%  dplyr::select(-date, OBJECTID, -adm0_name, -adm1_name, -adm2_name, -adm3_name)
# join things
wca_final_CH <- inner_join(wca_CH_nonga, cadre_harmonise_caf_ipc_filtered,  by = c("adm0_pcod2","adm1_pcod2","adm2_pcod2","adm2_5_pcod2","adm3_pcod2"))

#Nigeria didnt join easily so do this one specifically and differently
wca_CH_nga <- wca_CH %>% filter(adm0_pcod2 == "NG" & adm1_name != "Kano") %>% dplyr::select(-date, OBJECTID, -adm0_name, -adm1_name, -adm2_name, -adm3_name, adm0_pcod2, adm1_pcod2)
wca_final_CH_nga <- inner_join(wca_CH_nga, cadre_harmonise_caf_ipc_filtered,  by = c("adm1_pcod2","adm2_pcod2"))

#Kano seperately because adm1_pcod2 are in correct in GIS file
wca_NGA_sen_filtered <- wca_NGA_sen_filtered %>% dplyr::select(adm1_5_pcod2,Shape_Leng, Shape_Area,geometry)
wca_final_CH_kano <- inner_join(wca_NGA_sen_filtered, cadre_harmonise_caf_ipc_filtered,  by = c("adm1_5_pcod2"))


#Niger didnt join so do this also differently
wca_CH_ner <- ner_adm2_5 %>% filter(adm0_pcod2 == "NE" & !is.na(adm2_5_pcod2)) %>% dplyr::select(-adm0_pcod2,-adm1_pcod2,-adm2_pcod2, -adm0_name,-adm1_name, -adm2_name)
wca_final_CH_ner <- inner_join(wca_CH_ner, cadre_harmonise_caf_ipc_filtered,  by = c("adm2_5_pcod2"))

#put together and reorder/remove variables
wca_final_CH_FINAL <- bind_rows(wca_final_CH, wca_final_CH_nga, wca_final_CH_kano, wca_final_CH_ner) %>% dplyr::select(adm0_name, adm0_pcod3, adm0_pcod2, 
                                                                     adm0_5_name,	adm0_5_pcod2,
                                                                     adm1_name, adm1_pcod2,
                                                                     adm1_5_pcod2, adm1_5_pcod2, 
                                                                     adm2_name, adm2_pcod2,
                                                                     adm2_5_name, adm2_5_pcod2,
                                                                     adm3_name, adm3_pcod2,
                                                                     everything(),
                                                                     Shape_Leng, Shape_Area, Shape_Area,
                                                                     -adm0_gaulcode, -OBJECTID, -usethisperiod)

#create end and start date
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = "2020-06-01", end_date = "2020-09-30") 
wca_final_CH_FINAL <- wca_final_CH_FINAL %>% mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))




#visualize map to make sure work was done right -
CH_colors = c("1" = "#c6ffc7", "2" = "#ffe718", "3" = "#e88400", "4" = "#e02d00", "5" = "#5e0803")
map <- ggplot()  +geom_sf(data = wca_final_CH_FINAL, mapping = aes(fill = as.factor(phase_class), color = NULL)) +theme_void() +coord_sf(datum=NA) +labs(fill="phasing")  +scale_fill_manual(values = CH_colors) +geom_sf(data=wca_shp0_filtered,  lwd=1, fill=NA)


#fix, reduce size and rename
wca_CHIPC_mar2020_projected_jun2020 <- st_make_valid(wca_final_CH_FINAL)
wca_CHIPC_mar2020_projected_jun2020_simple <- rmapshaper::ms_simplify(wca_CHIPC_mar2020_projected_jun2020, keep_shapes = TRUE) # simplify polygons
### Save it as .gpkg file
#st_write(wca_CHIPC_mar2020_projected_jun2020, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2020_projected_jun2020.geojson", driver="GeoJSON", append = T) 
st_write(wca_CHIPC_mar2020_projected_jun2020_simple, "data\\geo\\finalized_CHgeofiles\\wca_CHIPC_mar2020_projected_jun2020_simple.geojson", driver="GeoJSON", append = T) 




