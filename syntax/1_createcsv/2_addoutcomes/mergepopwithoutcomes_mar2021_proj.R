library(readxl)
library(tidyverse)
library(readr)
library(stringi)

#open CH outcome data - without Nigeria 
Outcome_without_Nigeria <- read_excel("data/raw/CH_2021/outcomes/Outcome_with_new_data_of_Guinea.xlsx")
#replace 0 with NA
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate_at(vars(CA_curr:Mort_proj), ~replace(., . == 0, NA)) 
#select only current outcome indicators
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% select(-CA_curr,-EME_Curr,-Nutrition_Curr,-Mort_Curr) %>% 
  rename(adm0_name = ADMIN0Name, adm1_name = ADMIN1Name, adm2_name = ADMIN2Name, foodconsumption_phase = CA_proj, livelihoods_phase = EME_proj, nutrition_phase = Nutrition_proj, mortality_phase = Mort_proj)
#remove characters and proper case
Outcome_without_Nigeria <- Outcome_without_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Outcome_without_Nigeria <- Outcome_without_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#There are some countries areas where analysis was only done at amd1 level so remove the adm2 duplication for these countries
Outcome_without_Nigeria_adm1 <- Outcome_without_Nigeria  %>% filter(adm0_name %in% c("Bissau","Cote  D'ivoire","Gambia","Ghana","Liberia")) %>% mutate(adm2_name = if_else(adm2_name == adm1_name,"",adm2_name)) %>%  mutate(adm2_name = na_if(adm2_name, ""))


#join one by one

#Burkina - 
bfa_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_mar2021_proj.csv")
#join projrent
sux1 <- anti_join(bfa_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
bfa_mar2021_proj <- left_join(bfa_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
bfa_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_mar2021_proj.csv")


#Cote Divoire
civ_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/civ/civ_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(civ_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
Outcome_without_Nigeria_adm1 <- Outcome_without_Nigeria_adm1 %>% mutate(adm0_name = recode(adm0_name,"Cote  D'ivoire" = "Cote D'ivoire"))
#join
civ_mar2021_proj <- left_join(civ_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#write
civ_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/civ/civ_mar2021_proj.csv")

#Guinea_bissau
gnb_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gnb/gnb_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(gnb_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
Outcome_without_Nigeria_adm1 <- Outcome_without_Nigeria_adm1 %>% mutate(adm0_name = recode(adm0_name,"Bissau" = "Guinea-Bissau"))
#join
gnb_mar2021_proj <- left_join(gnb_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#write
gnb_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gnb/gnb_mar2021_proj.csv")

#Cameroon
cmr_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/cmr/cmr_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(cmr_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
cmr_mar2021_proj <- left_join(cmr_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
cmr_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/cmr/cmr_mar2021_proj.csv")

#ghana
gha_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(gha_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#join
gha_mar2021_proj <- left_join(gha_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#write
gha_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_mar2021_proj.csv")

#Gambia
gmb_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(gmb_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#join
gmb_mar2021_proj <- left_join(gmb_mar2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name","adm2_name"))
#write
gmb_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_mar2021_proj.csv")

#Guinea
gin_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gin/gin_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(gin_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
gin_mar2021_proj <- left_join(gin_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
gin_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gin/gin_mar2021_proj.csv")

#Liberia 
lbr_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/lbr/lbr_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(lbr_mar2021_proj, Outcome_without_Nigeria_adm1 , by = c("adm0_name","adm1_name","adm2_name"))
#join
lbr_mar2021_proj <- left_join(lbr_mar2021_proj, Outcome_without_Nigeria_adm1 , by = c("adm0_name","adm1_name","adm2_name"))
#write
lbr_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/lbr/lbr_mar2021_proj.csv")


#Mali 
mli_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mli/mli_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(mli_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
mli_mar2021_proj <- left_join(mli_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
mli_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mli/mli_mar2021_proj.csv")


#MRT
mrt_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(mrt_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
mrt_mar2021_proj <- left_join(mrt_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
mrt_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_mar2021_proj.csv")


#Niger
ner_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(ner_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate(adm2_name = case_when(
  adm2_name == "Guidan Roumdji_accessible" ~ "Guidan Roumdji_accessiblemarch2021",
  adm2_name == "Guidan Roumdji_limitedaccess" ~ "Guidan Roumdji_limitedaccessmarch2021",
  adm2_name == "Madarounfa_accessible" ~ "Madarounfa_accessiblemarch2021",
  adm2_name == "Madarounfa_limitedaccess" ~ "Madarounfa_limitedaccessmarch2021",
  adm2_name == "Tahoua_accessible" ~ "Tahoua_accessiblemarch2021",
  adm2_name == "Tahoua_limitedaccess" ~ "Tahoua_limitedaccessmarch2021",
  adm2_name == "Ouallam_accessible" ~ "Ouallam_accessiblemarch2021",
  adm2_name == "Ouallam_limitedaccess" ~ "Ouallam_limitedaccessmarch2021",
  adm2_name == "Tera_accessible" ~ "Tera_accessiblemarch2021",
  adm2_name == "Tera_limitedaccess" ~ "Tera_limitedaccessmarch2021",
  TRUE ~ adm2_name))
sux1 <- anti_join(ner_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
ner_mar2021_proj <- left_join(ner_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
ner_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_mar2021_proj.csv")

#Sierra Leone
sle_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(sle_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
sle_mar2021_proj <- left_join(sle_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
sle_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_mar2021_proj.csv")

#Senegal
sen_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(sen_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
sen_mar2021_proj <- left_join(sen_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
sen_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_mar2021_proj.csv")

#Chad
tcd_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tcd/tcd_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(tcd_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
tcd_mar2021_proj <- left_join(tcd_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
tcd_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tcd/tcd_mar2021_proj.csv")

#Togo
tgo_mar2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(tgo_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
tgo_mar2021_proj <- left_join(tgo_mar2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
tgo_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_mar2021_proj.csv")



#open CH outcome data - with Nigeria 
Outcome_Nigeria <- read_excel("data/raw/CH_2021/outcomes/Outcome_Nigeria_Last_version.xlsx")
#replace 0 with NA
Outcome_Nigeria <- Outcome_Nigeria %>% mutate_at(vars(CA_proj:Mort_proj), ~replace(., . == 0, NA)) 
#select only projrent outcome indicators
Outcome_Nigeria <- Outcome_Nigeria  %>% select(-CA_curr, -EME_Curr, -Nutrition_Curr, -Mort_Curr) %>% 
  rename(adm0_name = ADMIN0Name, adm1_name = ADMIN1Name, adm2_name = ADMIN2Name, foodconsumption_phase = CA_proj, livelihoods_phase = EME_proj, nutrition_phase = Nutrition_proj, mortality_phase = Mort_proj)
#remove characters and proper case
Outcome_Nigeria <- Outcome_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Outcome_Nigeria <- Outcome_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))

#Nigeria - skip for now
nga_mar2021_proj <- read_csv("data/csv/nga/nga_mar2021_proj.csv", lazy = FALSE)
#join projrent
sux1 <- anti_join(nga_mar2021_proj, Outcome_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
sux2 <- anti_join( Outcome_Nigeria, nga_mar2021_proj, by = c("adm0_name","adm1_name","adm2_name"))
#recode for Borno, some IDP areas (Yobe areas cannot be joined because they are split into inaccessible vs accessible)
Outcome_Nigeria <- Outcome_Nigeria %>% mutate(adm2_name = recode(adm2_name,"Askirauba" = "Askirauba (Total)",
                                                                            "Bama" = "Bama (Total)",
                                                                            "Damboa" = "Damboa (Total)",
                                                                            "Dikwa" = "Dikwa (Total)",
                                                                            "Gubio" = "Gubio (Total)",
                                                                            "Gwoza" = "Gwoza (Total)",
                                                                            "Konduga" = "Konduga (Total)",
                                                                            "Mafa" = "Mafa (Total)",
                                                                            "Magumeri" = "Magumeri (Total)",
                                                                            "Mobbar" = "Mobbar (Total)",
                                                                            "Monguno" = "Monguno (Total)",
                                                                            "Nganzai" = "Nganzai (Total)"))
#fix adm1/adm2_name names
Outcome_Nigeria <- Outcome_Nigeria %>% mutate(adm1_name = case_when(
  adm2_name == "Amac" ~ "Zone A",
  adm2_name == "Bwari" ~ "Zone A",
  adm2_name == "Gwagwalada" ~ "Zone B",
  adm2_name == "Kuje" ~ "Zone B",
  adm2_name == "Kwali" ~ "Zone B",
  adm2_name == "Abaji" ~ "Zone C",
  adm2_name %in% c("Auyo","Biriniwa","Guri","Hadejia","Kafin Hausa","Kaugama","Kiri Kasamma","Malam Madori") ~ "Jigawa_zone 1",
  adm2_name %in% c("Birni Kudu") ~ "Jigawa_zone 2",
  adm2_name %in% c("Buji","Babura","Gagarawa","Garki","Gumel","Gwiwa","Kazaure","Maigatari","Roni","Sule-Tankarkar","Taura","Yankwashi","Ringim") ~ "Jigawa_zone 3",
  adm2_name== "Dutse" ~ "Jigawa_zone 4",
  adm2_name == "Gwaram" ~ "Jigawa_zone 5",
  adm2_name == "Jahun" ~ "Jigawa_zone 6",
  adm2_name == "Kiyawa" ~ "Jigawa_zone 7",
  adm2_name == "Miga" ~ "Jigawa_zone 8",
  adm2_name %in% c("Dala","D/Kudu","Fagge","G/Mallam","Gezawa","Gwale","Kumbotso","Kura","Madobi","Municipal","Nassarawa","Tarauni","Ungogo","Warawa","Minjibir") ~ "Kano Central (Zone 1)",
  adm2_name %in% c("Bagwai","Bichi","Danbatta","D/Tofa","Gabasawa","Gwarzo","Kabo","Kunchi","Makoda","R/Gado","Shanono","Tofa","Tsanyawa") ~ "Kano North",
  adm2_name %in% c("Ajingi", "Albasu", "Bebeji", "Bunkure", "Doguwa", "Garko", "Gaya", "Karaye", "Kibiya", "Kiru", "Rano", "Rogo", "Sumaila", "Takai", "T/Wada","Wudil") ~ "Kano South (Zone 3)",
  adm2_name %in% c("Gusau Total","Bungudu","Tsafe","Maru") ~ "Central Zone",
  adm2_name %in% c("Kaura Namoda Total","Shinkafi","Zurmi","Birnin Magaji") ~ "Northern Zone",
  adm2_name %in% c("Maradun","Anka","Bakura","Gummi","Bukkuyum","Talata Mafara Total") ~ "Western Zone",
  adm2_name %in% c("Ardo Kola","Jalingo","Karim Lamido","Lau","Yorro","Zing") ~ "Northern Zone",
  adm2_name %in% c("Bali","Gashaka","Gassol","Kurmi","Sardauna") ~ "Central Zone",
  adm2_name %in% c("Donga","Ibi","Takum","Ussa","Wukari") ~ "Southern Zone",
  TRUE ~ adm1_name))
#Sokoto Idps
Outcome_Nigeria <- Outcome_Nigeria %>% mutate(adm2_name = recode(adm2_name,"Sokoto Idps" = "Idps"))
#join
nga_mar2021_proj <- left_join(nga_mar2021_proj, Outcome_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#apply the katsina IDP classification to all three IDPS groups in Katsina
nga_mar2021_proj <- nga_mar2021_proj %>% mutate(
  foodconsumption_phase = case_when(adm2_name %in% c("Katsina -IDPs","Daura-IDPs","Funtua-IDPs") ~ 3, TRUE ~ foodconsumption_phase),
  livelihoods_phase = case_when(adm2_name %in% c("Katsina -IDPs","Daura-IDPs","Funtua-IDPs") ~ 4, TRUE ~ livelihoods_phase),
  nutrition_phase = case_when(adm2_name %in% c("Katsina -IDPs","Daura-IDPs","Funtua-IDPs") ~ 3, TRUE ~ nutrition_phase))
#write
nga_mar2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/nga/nga_mar2021_proj.csv")






