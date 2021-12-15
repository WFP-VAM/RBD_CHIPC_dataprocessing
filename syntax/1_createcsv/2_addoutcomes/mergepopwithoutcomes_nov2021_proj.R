library(readxl)
library(tidyverse)
library(readr)
library(stringi)

#open CH outcome data - without Nigeria 
Outcome_without_Nigeria <- read_excel("data\\raw\\CH_2021\\nov 2021\\Outcome_without_Nigeria_Nov2021.xlsx")
#replace 0 with NA
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate_at(vars(CA_curr:Mort_proj), ~replace(., . == 0, NA)) 
#select only current outcome indicators
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% select(-CA_curr, -EME_Curr, -Nutrition_Curr, -Mort_Curr) %>% 
  rename(adm0_name = ADMIN0Name, adm1_name = ADMIN1Name, adm2_name = ADMIN2Name, foodconsumption_phase = CA_proj, livelihoods_phase = EME_proj, nutrition_phase = Nutrition_proj, mortality_phase = Mort_proj)
#remove characters and proper case
Outcome_without_Nigeria <- Outcome_without_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Outcome_without_Nigeria <- Outcome_without_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#There are some countries areas where analysis was only done at amd1 level so remove the adm2 duplication for these countries
Outcome_without_Nigeria_adm1 <- Outcome_without_Nigeria  %>% filter(adm0_name %in% c("Guinee Bissau","Cote D'ivoire","Gambia","Togo")) %>% select(-adm2_name)
Outcome_without_Nigeria_adm1 <- Outcome_without_Nigeria_adm1 %>% mutate(adm0_name = recode(adm0_name,"Guinee Bissau" = "Guinea-Bissau"))
 
#change names of Countries
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate(adm0_name = case_when(
  adm0_name == "Burkina" ~ "Burkina Faso", 
  adm0_name == "Cameroun" ~ "Cameroon",
  adm0_name == "Guinee" ~ "Guinea",
  adm0_name == "Mauritanie" ~ "Mauritania",
  adm0_name == "Tchad" ~ "Chad",
  TRUE ~ adm0_name))


#join one by one
#Benin
ben_nov2021_proj <- read_csv("data/csv/ben/ben_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
ben_nov2021_proj <- ben_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(ben_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
ben_nov2021_proj <- left_join(ben_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
ben_nov2021_proj %>% write_csv("data/csv/ben/ben_nov2021_proj.csv")

#Burkina - 
bfa_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
bfa_nov2021_proj <- bfa_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(bfa_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
bfa_nov2021_proj <- left_join(bfa_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
bfa_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/BFA/bfa_nov2021_proj.csv")

#Cameroon
cmr_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/cmr/cmr_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
cmr_nov2021_proj <- cmr_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(cmr_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
cmr_nov2021_proj <- left_join(cmr_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
cmr_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/cmr/cmr_nov2021_proj.csv")

#Cote Divoire
civ_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/civ/civ_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
civ_nov2021_proj <- civ_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(civ_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#join
civ_nov2021_proj <- left_join(civ_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#write
civ_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/civ/civ_nov2021_proj.csv")

#Gambia
gmb_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
gmb_nov2021_proj <- gmb_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(gmb_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#join
gmb_nov2021_proj <- left_join(gmb_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#write
gmb_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gmb/gmb_nov2021_proj.csv")

#Guinea_bissau
gnb_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gnb/gnb_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
gnb_nov2021_proj <- gnb_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(gnb_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#join
gnb_nov2021_proj <- left_join(gnb_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#write
gnb_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gnb/gnb_nov2021_proj.csv")


#ghana
gha_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
gha_nov2021_proj <- gha_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(gha_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
gha_nov2021_proj <- left_join(gha_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
gha_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gha/gha_nov2021_proj.csv")


#Guinea
gin_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gin/gin_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
gin_nov2021_proj <- gin_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(gin_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
gin_nov2021_proj <- left_join(gin_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
gin_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/gin/gin_nov2021_proj.csv")

#Mali 
mli_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mli/mli_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
mli_nov2021_proj <- mli_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(mli_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
sux2 <- anti_join(Outcome_without_Nigeria, mli_nov2021_proj, by = c("adm0_name","adm1_name","adm2_name"))
#fix Koro
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate(adm2_name = case_when(
  adm2_name == "Koro_accessible" ~ "Koro_accessible_nov2021", TRUE ~ adm2_name))
sux1 <- anti_join(mli_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
mli_nov2021_proj <- left_join(mli_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
mli_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mli/mli_nov2021_proj.csv")


#MRT
mrt_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
mrt_nov2021_proj <- mrt_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(mrt_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
mrt_nov2021_proj <- left_join(mrt_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
mrt_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/mrt/mrt_nov2021_proj.csv")


#Niger
ner_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
ner_nov2021_proj <- ner_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(ner_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#fix some names
Outcome_without_Nigeria <- Outcome_without_Nigeria %>% mutate(adm2_name = case_when(
  adm2_name == "Guidan Roumdji_inaccessible" ~ "Guidan Roumdji_inaccessible_nov2021",
  adm2_name == "Guidan Roumdji" ~ "Guidan Roumdji_accessible_nov2021",
  adm2_name == "Madarounfa_inaccessible" ~ "Madarounfa_inaccessible_nov2021",
  adm2_name == "Madarounfa" ~ "Madarounfa_accessible_nov2021",
  adm2_name == "Ouallam_accessible" ~ "Ouallam_accessible_nov2021",
  adm2_name == "Ouallam_inaccessible" ~ "Ouallam_inaccessible_nov2021",
  adm2_name == "Tera_accessible" ~ "Tera_accessible_nov2021",
  adm2_name == "Tera_inaccessible" ~ "Tera_inaccessible_nov2021",
  adm2_name == "Tillaberi_inaccessible" ~ "Tillaberi_inaccessible_nov2021",
  adm2_name == "Tillaberi" ~ "Tillaberi_accessible_nov2021",
  adm2_name == "Tchirozerine" ~ "Tchirozerine Department",
  adm2_name == "Dosso" ~ "Dosso Department",
  TRUE ~ adm2_name))
sux1 <- anti_join(ner_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
ner_nov2021_proj <- left_join(ner_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
ner_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/ner/ner_nov2021_proj.csv")

#Sierra Leone
sle_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
sle_nov2021_proj <- sle_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(sle_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
sle_nov2021_proj <- left_join(sle_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
sle_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sle/sle_nov2021_proj.csv")

#Senegal
sen_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
sen_nov2021_proj <- sen_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(sen_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
sen_nov2021_proj <- left_join(sen_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
sen_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/sen/sen_nov2021_proj.csv")

#Chad
tcd_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tcd/tcd_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
tcd_nov2021_proj <- tcd_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(tcd_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#join
tcd_nov2021_proj <- left_join(tcd_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name","adm2_name"))
#write
tcd_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tcd/tcd_nov2021_proj.csv")

#Togo
tgo_nov2021_proj <- read_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_nov2021_proj.csv", lazy = FALSE) %>% select(-foodconsumption_phase:-mortality_phase)
tgo_nov2021_proj <- tgo_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))
#join projrent
sux1 <- anti_join(tgo_nov2021_proj, Outcome_without_Nigeria, by = c("adm0_name","adm1_name"))
#join
tgo_nov2021_proj <- left_join(tgo_nov2021_proj, Outcome_without_Nigeria_adm1, by = c("adm0_name","adm1_name"))
#write
tgo_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/tgo/tgo_nov2021_proj.csv")

#open CH outcome data - with Nigeria 
#three different kinds of joins
Outcome_Nigeria <- read_excel("data\\raw\\CH_2021\\nov 2021\\Outcome_Nigeria_Nov2021.xlsx") 
#replace 0 with NA
Outcome_Nigeria <- Outcome_Nigeria %>% mutate_at(vars(CA_curr:Mort_proj), ~replace(., . == 0, NA)) 
#select only current outcome indicators
Outcome_Nigeria <- Outcome_Nigeria  %>% select(-CA_curr, -EME_Curr, -Nutrition_Curr, -Mort_Curr) %>% 
  rename(adm0_name = ADMIN0Name, adm1_name = ADMIN1Name, adm2_name = ADMIN2Name, foodconsumption_phase = CA_proj, livelihoods_phase = EME_proj, nutrition_phase = Nutrition_proj, mortality_phase = Mort_proj)
#remove characters and proper case
Outcome_Nigeria <- Outcome_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Outcome_Nigeria <- Outcome_Nigeria %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))

#import csv
nga_nov2021_proj <- read_csv("data/csv/nga/nga_nov2021_proj.csv", lazy = FALSE) 
nga_nov2021_proj <- nga_nov2021_proj %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))

#join1
Outcome_Nigeria_join1 <- Outcome_Nigeria %>% filter(adm1_name %in% c("Adamawa","Borno","Yobe","Abia","Bauchi"))
nga_nov2021_proj_join1 <- nga_nov2021_proj %>% filter(adm1_name %in% c("Adamawa","Borno","Yobe","Abia","Bauchi"))
#check join
sux1 <- anti_join(nga_nov2021_proj_join1, Outcome_Nigeria_join1, by = c("adm0_name","adm1_name","adm2_name"))
sux2 <- anti_join(Outcome_Nigeria_join1, nga_nov2021_proj_join1, by = c("adm0_name","adm1_name","adm2_name"))
#recode for Borno, some IDP areas (Yobe areas cannot be joined because they are split into inaccessible vs accessible)
Outcome_Nigeria_join1 <- Outcome_Nigeria_join1 %>% mutate(adm2_name = recode(adm2_name,
                                                                             "Bama" = "Bama (Total)",
                                                                             "Dikwa" = "Dikwa (Total)",
                                                                             "Gubio" = "Gubio (Total)",
                                                                             "Gwoza" = "Gwoza (Total)",
                                                                             "Kaga" = "Kaga (Total)",
                                                                             "Konduga" = "Konduga (Total)",
                                                                             "Magumeri" = "Magumeri (Total)",
                                                                             "Monguno" = "Monguno (Total)",
                                                                             "Ngala" = "Ngala (Total)",
                                                                             "Nganzai" = "Nganzai (Total)",
                                                                             "Kukawa" = "Kukawa (Inaccessible)"))
#join 
nga_nov2021_proj_join1 <- left_join(nga_nov2021_proj_join1, Outcome_Nigeria_join1, by = c("adm0_name","adm1_name","adm2_name"))


#join2
Outcome_Nigeria_join2 <- Outcome_Nigeria %>% filter(adm1_name %in% c("Zamfara","Cross River","Edo","Niger"))
nga_nov2021_proj_join2 <- nga_nov2021_proj %>% filter(adm1_name %in% c("South","East","North","Cross River/C","Cross River/N","Cross River/S","Edo/N","Edo/C","Edo/S","Western Zone","Northern Zone","Central Zone"))
#recode
Outcome_Nigeria_join2 <- Outcome_Nigeria_join2 %>% mutate(adm1_name = case_when(
  adm2_name == "Agaie" ~ "South",
  adm2_name == "Chanchaga" ~ "East",
  adm2_name == "Kontagora" ~ "North",
  adm2_name == "Ikom" ~ "Cross River/C",
  adm2_name == "Ogoja" ~ "Cross River/N",
  adm2_name == "Calabar-Municipal" ~ "Cross River/S",
  adm2_name == "Akoko Edo" ~ "Edo/N",
  adm2_name == "Esan West" ~ "Edo/C",
  adm2_name == "Oredo" ~ "Edo/S",
  adm2_name == "Bakura" ~ "Western Zone",
  adm2_name == "Zamfara, North" ~ "Northern Zone",
  adm2_name == "Bungudu" ~ "Central Zone")) %>%
  select(adm1_name, foodconsumption_phase:mortality_phase)
#check join
sux1 <- anti_join(nga_nov2021_proj_join2, Outcome_Nigeria_join2, by = c("adm1_name"))
sux2 <- anti_join(Outcome_Nigeria_join2, nga_nov2021_proj_join2, by = c("adm1_name"))
#join projrent
nga_nov2021_proj_join2 <- left_join(nga_nov2021_proj_join2, Outcome_Nigeria_join2, by = c("adm1_name"))

#join3
Outcome_Nigeria_join3 <- Outcome_Nigeria %>% filter(adm1_name %in% c("Benue", "Enugu", "Federal Capital Territory", "Jigawa", "Kano", "Kaduna", "Katsina", "Kebbi", "Gombe", "Lagos", "Plateau", "Sokoto", "Taraba")) %>% select(adm1_name, adm2_name,foodconsumption_phase:mortality_phase)                                                                  
nga_nov2021_proj_join3 <- nga_nov2021_proj %>% filter(adm1_name %in% c("Benue", "Enugu", "Federal Capital Territory", "Jigawa", "Kano", "Kaduna", "Katsina", "Kebbi", "Gombe", "Lagos", "Plateau", "Sokoto", "Taraba")) 

nga_nov2021_proj_join3 <- nga_nov2021_proj_join3 %>% mutate(adm2_key = case_when(
  #benue
  adm2_name %in% c("Katsina-Ala","Konshisha","Kwande","Logo","Ukum","Ushongo","Vandeikya") ~ "Northeast (Zone A)",
  adm2_name %in% c("Buruku","Gboko","Guma","Gwer East","Gwer West","Makurdi","Tarka") ~ "Northwest (Zone B)",
  adm2_name %in% c("Ado","Agatu","Apa","Obi","Ogbadibo","Ohimini","Oju","Okpokwu","Oturkpo") ~ "South (Zone C)",
  #enugu
  adm2_name %in% c("Igbo Etiti","Igbo Eze North","Igbo Eze South","Nsukka", "Udenu","Uzo Uwani") ~ "Nsukka",
  adm2_name %in% c("Aninri","Awgu","Ezeagu","Oji River","Udi") ~ "Ezeagu",
  adm2_name %in% c("Enugu North","Enugu South","Enugu East","Isi Uzo","Nkanu East","Nkanu West") ~ "Enugu East",
  #FCT
  adm2_name %in% c("Abuja Municipal","Bwari") ~ "Abuja Municipal",
  adm2_name %in% c("Abaji","Gwagwalada","Kuje","Kwali") ~ "Gwagwalada",
  #gombe
  adm2_name %in% c("Akko","Yamaltu Deba") ~ "Akko",
  adm2_name %in% c("Balanga","Billiri","Kaltungo","Shongom") ~ "Balanga",
  adm2_name %in% c("Dukku","Funakaye","Gombe","Kwami","Nafada") ~ "Dukku",
  #jigawa
  adm2_name %in% c("Birni Kudu","Buji","Dutse","Gwaram","Jahun","Kiyawa","Miga") ~ "South_west (Zone 3)",
  adm2_name %in% c("Auyo","Biriniwa","Guri","Hadejia","Kafin Hausa","Kaugama","Kiri Kasamma","Malam Madori") ~ "North_east (Zone 1)",
  adm2_name %in% c("Babura","Gagarawa","Garki","Gumel","Gwiwa","Kazaure","Maigatari","Roni","Sule-Tankarkar","Taura","Yankwashi","Ringim") ~ "North_west (Zone 2)",
  #kano
  adm2_name %in% c("Dala","D/Kudu","Fagge","G/Mallam","Gezawa","Gwale","Municipal","Kumbotso","Kura","Madobi","Minjibir","Nassarawa","Tarauni","Ungogo","Warawa") ~ "Central",
  adm2_name %in% c("Bagwai","Bichi","Danbatta","D/Tofa","Gabasawa","Gwarzo","Kabo","Karaye","Kunchi","Makoda","R/Gado","Shanono","Tofa","Tsanyawa") ~ "North",
  adm2_name %in% c("Ajingi","Albasu","Bebeji","Bunkure","Doguwa","Garko","Gaya","Kibiya","Kiru","Rano","Rogo","Sumaila","Takai","T/Wada","Wudil") ~ "South",
  #kaduna
  adm2_name %in% c("Birnin Gwari","Chikun","Giwa","Igabi","Kaduna North","Kaduna South","Kajuru") ~ "Kaduna Central Senatorial Zone",
  adm2_name %in% c("Jaba","Jema'a","Kachia","Kagarko","Kaura","Kauru","Sanga","Zango Kataf") ~ "Kaduna South Senatorial Zone",
  adm2_name %in% c("Ikara","Kubau","Kudan","Lere","Makarfi","Sabon Gari","Soba","Zaria") ~ "Kaduna North Senatorial Zone",
  #katsina
  adm2_name %in% c("Baure","Bindawa","Daura","Dutsi","Ingawa","Kankia","Kusada","Mai'adua","Mani","Mashi","Sandamu","Zango") ~ "Daura",
  adm2_name %in% c("Bakori","Dandume","Danja","Faskari","Funtua","Kafur","Kankara","Malumfashi","Matazu","Musawa","Sabuwa") ~ "Funtua",
  adm2_name %in% c("Batagarawa","Batsari","Charanchi","Dan Musa","Dutsin-Ma","Jibia","Kaita","Katsina","Kurfi","Rimi","Safana") ~ "Katsina",
  #kebbi
  adm2_name %in% c("Arewa","Argungu","Augie","Bagudo","Dandi","Jega","Suru") ~ "Kebbi North",
  adm2_name %in% c("Aliero","Birnin Kebbi","Bunza","Gwandu","Kalgo","Koko/Besse","Maiyama") ~ "Kebbi Central",
  adm2_name %in% c("Fakai","Ngaski","Sakaba","Shanga","Danko/Wasagu","Yauri","Zuru") ~ "Kebbi South",
  #Lagos
  adm2_name %in% c("Agege","Ajeromi_ifelodun","Alimosho","Amuwo_odofin","Badagry","Ifako_ijaiye","Ikeja","Mushin","Ojo","Oshodi_osolo") ~ "Lagos_west",
  adm2_name %in% c("Epe","Ibeju_lekki","Ikorodu","Kosofe","Shomolu") ~ "Lagos_east",
  adm2_name %in% c("Apapa","Eti_osa","Lagos_island","Lagos_mainland","Surulere") ~ "Lagos_central",
  #Plateau
  adm2_name %in% c("Barkin Ladi","Bassa","Jos East","Jos North","Jos South","Riyom") ~ "Northern Zone",
  adm2_name %in% c("Bokkos","Kanam","Kanke","Mangu","Pankshin") ~ "Central Zone",
  adm2_name %in% c("Langtang North","Langtang South","Mikang","Qu'an Pan","Shendam","Wase") ~ "Southern Zone",
  #Sokoto 
  adm2_name %in% c("Binji","Gudu","Kware","Silame","Sokoto North","Sokoto South","Tangaza","Wamakko") ~ "Central",
  adm2_name %in% c("Gada","Goronyo","Gwadabawa","Illela","Isa","Rabah","Sabon Birni","Wurno") ~ "East",
  adm2_name %in% c("Bodinga","Dange/Shuni","Kebbe","Shagari","Tambuwal","Tureta","Yabo") ~ "South",
  #Taraba
  adm2_name %in% c("Bali","Gashaka","Gassol","Kurmi","Sardauna") ~ "Central",
  adm2_name %in% c("Donga","Ibi","Takum","Ussa","Wukari") ~ "Ibi",
  adm2_name %in% c("Ardo Kola","Jalingo","Karim Lamido","Lau","Yorro","Zing") ~ "Lau"))
#check join
sux1 <- anti_join(nga_nov2021_proj_join3, Outcome_Nigeria_join3, by = c("adm1_name", "adm2_key" = "adm2_name"))
sux2 <- anti_join(Outcome_Nigeria_join3, nga_nov2021_proj_join3, by = c("adm1_name", "adm2_name" = "adm2_key")) 
#join projrent
nga_nov2021_proj_join3 <- left_join(nga_nov2021_proj_join3, Outcome_Nigeria_join3, by = c("adm1_name", "adm2_key" = "adm2_name")) %>% select(-adm2_key)



#put it all together
nga_nov2021_proj <- bind_rows(nga_nov2021_proj_join1,nga_nov2021_proj_join2,nga_nov2021_proj_join3)
#write
nga_nov2021_proj %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/nga/nga_nov2021_proj.csv")