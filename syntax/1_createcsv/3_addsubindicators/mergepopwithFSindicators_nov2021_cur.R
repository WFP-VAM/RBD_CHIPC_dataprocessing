library(readxl)
library(tidyverse)
library(readr)
library(stringi)

#import excel file containing consolidated 
Indicateurs <- read_excel("data/raw/CH_2021/nov 2021/Compilation Globale Matrice  Nov2021.xlsx") %>% 
  select(adm0_name = ADMIN0Name,	adm1_name = ADMIN1Name, adm2_name = ADMIN2Name,
         FCG_Poor, FCG_Borderline,FCG_Acceptable,	FCG_finalphase,
         HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,
         HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	HHS_finalphase,
         LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,	LhHCSCat_finalphase,
         rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase)



#round all %to one decimal point
Indicateurs <- Indicateurs %>% mutate(across(c(FCG_Poor, FCG_Borderline,FCG_Acceptable,
                                               HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,
                                               HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	
                                               LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,
                                               rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3), round,1))

#many countries did not calculate final phase and a few calculations are incorrect so re-calculate the whole thing
#rCSI
Indicateurs <- Indicateurs %>%  mutate(rcsi23 = rCSI_Phase2 + rCSI_Phase3,
           rCSI_finalphase =
           case_when(
             rCSI_Phase3 >= 20 ~ 3,
             rCSI_Phase2 >= 20 | rcsi23 >= 20 ~ 2,
             rCSI_Phase1 >= 20 ~ 1,
             TRUE ~  NA_real_)) %>% select(-rcsi23)
#FCS
#Apply the Cadre Harmonise rules for phasing the Food Consumption Groups
Indicateurs <- Indicateurs %>% mutate(PoorBorderline = FCG_Poor + FCG_Borderline, FCG_finalphase = case_when(
  !is.na(FCG_finalphase) ~ FCG_finalphase,
  FCG_Poor < 5 ~ 1,  
  FCG_Poor >= 20 ~ 4,
  between(FCG_Poor,5,10) ~ 2, 
  between(FCG_Poor,10,20) & PoorBorderline < 30 ~ 2, 
  between(FCG_Poor,10,20) & PoorBorderline >= 30 ~ 3,
  TRUE ~  NA_real_)) %>% select(-PoorBorderline)
#HDDS
Indicateurs <- Indicateurs %>% mutate(
  phase2345 = HDDS_Phase2 +HDDS_Phase3 +HDDS_Phase4 +HDDS_Phase5,
  phase345 = HDDS_Phase3 +HDDS_Phase4 +HDDS_Phase5,  
  phase45 = HDDS_Phase4 +HDDS_Phase5, 
  HDDS_finalphase = case_when(
     HDDS_Phase5 >= 20 ~ 5,  
     HDDS_Phase4 >= 20 | phase45 >= 20 ~ 4, 
     HDDS_Phase3 >= 20 | phase345 >= 20 ~ 3, 
     HDDS_Phase2 >= 20 | phase2345 >= 20 ~ 2,  
     HDDS_Phase1 >= 20 ~ 1,
     TRUE ~  NA_real_)) %>% select(-phase2345,-phase345,-phase45)
#LHCS
Indicateurs <- Indicateurs %>% mutate(stresscrisisemergency = LhHCSCat_StressStrategies +LhHCSCat_CrisisStategies +LhHCSCat_EmergencyStrategies,
       crisisemergency = LhHCSCat_CrisisStategies +LhHCSCat_EmergencyStrategies,
       LhHCSCat_finalphase = case_when(
         LhHCSCat_EmergencyStrategies >= 20 ~ 4,
         crisisemergency >= 20 | LhHCSCat_CrisisStategies > 20 ~ 3,
         crisisemergency >= 20 & LhHCSCat_EmergencyStrategies < 20 ~ 3,
         LhHCSCat_NoStrategies < 80 & LhHCSCat_CrisisStategies < 20 ~ 2,
         LhHCSCat_NoStrategies >= 80 ~ 1,
         TRUE ~  NA_real_)) %>% select(-stresscrisisemergency, -crisisemergency)
#HHS
Indicateurs <- Indicateurs %>% mutate(phase2345 = `HHS_Phase2` + `HHS_Phase3` + `HHS_Phase4` + `HHS_Phase5`,
       phase345 = `HHS_Phase3` + `HHS_Phase4` + `HHS_Phase5`,
       phase45 = `HHS_Phase4` + `HHS_Phase5`,
       HHS_finalphase = case_when(
         HHS_Phase5 >= 20 ~ 5,
         HHS_Phase4 >= 20 | phase45 >= 20 ~ 4,
         HHS_Phase3 >= 20 | phase345 >= 20 ~ 3,
         HHS_Phase2 >= 20 | phase2345 >= 20 ~ 2,
         HHS_Phase1 >= 20 ~ 1,
         TRUE ~  NA_real_)) %>% select(-phase2345, -phase345, -phase45)




#remove characters and proper case
Indicateurs <- Indicateurs %>%   mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Indicateurs <- Indicateurs %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#There are some countries areas where analysis was only done at amd1 level so remove the adm2 duplication for these countries
Indicateurs_adm1 <-  Indicateurs %>% filter(adm0_name %in% c("Guinea Bissau","Cote D'ivoire","Gambia","Liberia","Togo")) %>% 
  select(-adm2_name) %>% 
  mutate(adm0_name = case_when(adm0_name == "Guinea Bissau" ~ "Guinea-Bissau",TRUE ~ adm0_name))




#pullup and match country by country

#Benin - 
ben_nov2021_cur <- read_csv("data/csv/ben/ben_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(ben_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
ben_nov2021_cur <- left_join(ben_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
ben_nov2021_cur %>% write_csv("data/csv/ben/ben_nov2021_cur.csv")

#Burkina - 
bfa_nov2021_cur <- read_csv("data/csv/BFA/bfa_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(bfa_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
bfa_nov2021_cur <- left_join(bfa_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
bfa_nov2021_cur %>% write_csv("data/csv/BFA/bfa_nov2021_cur.csv")
 
#Cameroon 
cmr_nov2021_cur <- read_csv("data/csv/cmr/cmr_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(cmr_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
cmr_nov2021_cur <- left_join(cmr_nov2021_cur,  Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
# #write
cmr_nov2021_cur %>% write_csv("data/csv/CMR/cmr_nov2021_cur.csv")



#Chad
tcd_nov2021_cur <- read_csv("data/csv/tcd/tcd_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(tcd_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
tcd_nov2021_cur <- left_join(tcd_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
tcd_nov2021_cur %>% write_csv("data/csv/tcd/tcd_nov2021_cur.csv")

#CIV - 
civ_nov2021_cur <- read_csv("data/csv/civ/civ_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(civ_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name"))
civ_nov2021_cur <- left_join(civ_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name")) 
#write
civ_nov2021_cur %>% write_csv("data/csv/civ/civ_nov2021_cur.csv")

#GMB
gmb_nov2021_cur <- read_csv("data/csv/gmb/gmb_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(gmb_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name"))
gmb_nov2021_cur <- left_join(gmb_nov2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name")) 
#write
gmb_nov2021_cur %>% write_csv("data/csv/gmb/gmb_nov2021_cur.csv")

#Ghana 
gha_nov2021_cur <- read_csv("data/csv/gha/gha_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(gha_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
gha_nov2021_cur <- left_join(gha_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
gha_nov2021_cur %>% write_csv("data/csv/gha/gha_nov2021_cur.csv")

#GNB
gnb_nov2021_cur <- read_csv("data/csv/gnb/gnb_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(gnb_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name"))
gnb_nov2021_cur <- left_join(gnb_nov2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name")) 
#write
gnb_nov2021_cur %>% write_csv("data/csv/gnb/gnb_nov2021_cur.csv")

#Guinea
gin_nov2021_cur <- read_csv("data/csv/gin/gin_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(gin_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
gin_nov2021_cur <- left_join(gin_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
gin_nov2021_cur %>% write_csv("data/csv/gin/gin_nov2021_cur.csv")

#Mali - issue about how to classify Koro - because accessible/nonaccessible
mli_nov2021_cur <- read_csv("data/csv/mli/mli_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(mli_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs <- Indicateurs %>% mutate(adm2_name = case_when(
  adm2_name == "Koro_accessible" ~ "Koro_accessible_nov2021", TRUE ~ adm2_name))
sux1 <- anti_join(mli_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
mli_nov2021_cur <- left_join(mli_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
mli_nov2021_cur %>% write_csv("data/csv/mli/mli_nov2021_cur.csv") 

#Mauritania
mrt_nov2021_cur <- read_csv("data/csv/mrt/mrt_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(mrt_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
mrt_nov2021_cur <- left_join(mrt_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
mrt_nov2021_cur %>% write_csv("data/csv/mrt/mrt_nov2021_cur.csv") 

#Niger results shared were poor quality - only use the sheet (innacessible that seemed legit)
ner_nov2021_cur <- read_csv("data/csv/ner/ner_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(ner_nov2021_cur, Indicateurs,  by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs <- Indicateurs %>% mutate(adm2_name = case_when(
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
ner_nov2021_cur <- left_join(ner_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
ner_nov2021_cur %>% write_csv("data/csv/ner/ner_nov2021_cur.csv") 

#Senegal
sen_nov2021_cur <- read_csv("data/csv/sen/sen_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
sen_nov2021_cur <- sen_nov2021_cur %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#anti-join to see if it doesnt match
sux <- anti_join(sen_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
SEN <- Indicateurs %>% filter(adm0_name == "Senegal")
sux1 <- anti_join(SEN, sen_nov2021_cur, by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs <- Indicateurs %>% mutate(adm2_name = case_when(
  adm2_name == "Nioro du Rip" ~ "Nioro Du Rip",
  TRUE ~ adm2_name))
sen_nov2021_cur <- left_join(sen_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
sen_nov2021_cur %>% write_csv("data/csv/sen/sen_nov2021_cur.csv") 

#sle
sle_nov2021_cur <- read_csv("data/csv/sle/sle_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(sle_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
sle_nov2021_cur <- left_join(sle_nov2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
sle_nov2021_cur %>% write_csv("data/csv/sle/sle_nov2021_cur.csv") 

#tgo
tgo_nov2021_cur <- read_csv("data/csv/tgo/tgo_nov2021_cur.csv", lazy = FALSE) %>% select(-FCG_Poor:-rCSI_finalphase)
#anti-join to see if it doesnt match
sux1 <- anti_join(tgo_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name"))
tgo_nov2021_cur <- left_join(tgo_nov2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name")) 
#write
tgo_nov2021_cur %>% write_csv("data/csv/tgo/tgo_nov2021_cur.csv") 


###Nigeria 
#import excel file containing consolidated 
Indicateurs_nga <- read_excel("data/raw/CH_2021/nov 2021/Compilation Matrice Nigeria Nov2021.xlsx") %>% 
  select(adm0_name = ADMIN0Name,	adm1_name = ADMIN1Name, adm2_name = ADMIN2Name,
         FCG_Poor, FCG_Borderline,FCG_Acceptable,	FCG_finalphase,
         HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,
         HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	HHS_finalphase,
         LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,	LhHCSCat_finalphase,
         rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase)
#round all %to one decimal point
Indicateurs_nga <- Indicateurs_nga %>% mutate(across(c(FCG_Poor, FCG_Borderline,FCG_Acceptable,
                                               HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,
                                               HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	
                                               LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,
                                               rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3), round,1))

Indicateurs_nga <- Indicateurs_nga %>%   mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))


#many countries did not calculate final phase and a few calculations are incorrect so re-calculate the whole thing
#rCSI
Indicateurs_nga <- Indicateurs_nga %>%  mutate(rcsi23 = rCSI_Phase2 + rCSI_Phase3,
                                       rCSI_finalphase =
                                         case_when(
                                           rCSI_Phase3 >= 20 ~ 3,
                                           rCSI_Phase2 >= 20 | rcsi23 >= 20 ~ 2,
                                           rCSI_Phase1 >= 20 ~ 1,
                                           TRUE ~  NA_real_)) %>% select(-rcsi23)
#FCS
#Apply the Cadre Harmonise rules for phasing the Food Consumption Groups
Indicateurs_nga <- Indicateurs_nga %>% mutate(PoorBorderline = FCG_Poor + FCG_Borderline, FCG_finalphase = case_when(
  !is.na(FCG_finalphase) ~ FCG_finalphase,
  FCG_Poor < 5 ~ 1,  
  FCG_Poor >= 20 ~ 4,
  between(FCG_Poor,5,10) ~ 2, 
  between(FCG_Poor,10,20) & PoorBorderline < 30 ~ 2, 
  between(FCG_Poor,10,20) & PoorBorderline >= 30 ~ 3,
  TRUE ~  NA_real_)) %>% select(-PoorBorderline)
#HDDS
Indicateurs_nga <- Indicateurs_nga %>% mutate(
  phase2345 = HDDS_Phase2 +HDDS_Phase3 +HDDS_Phase4 +HDDS_Phase5,
  phase345 = HDDS_Phase3 +HDDS_Phase4 +HDDS_Phase5,  
  phase45 = HDDS_Phase4 +HDDS_Phase5, 
  HDDS_finalphase = case_when(
    HDDS_Phase5 >= 20 ~ 5,  
    HDDS_Phase4 >= 20 | phase45 >= 20 ~ 4, 
    HDDS_Phase3 >= 20 | phase345 >= 20 ~ 3, 
    HDDS_Phase2 >= 20 | phase2345 >= 20 ~ 2,  
    HDDS_Phase1 >= 20 ~ 1,
    TRUE ~  NA_real_)) %>% select(-phase2345,-phase345,-phase45)
#LHCS
Indicateurs_nga <- Indicateurs_nga %>% mutate(stresscrisisemergency = LhHCSCat_StressStrategies +LhHCSCat_CrisisStategies +LhHCSCat_EmergencyStrategies,
                                      crisisemergency = LhHCSCat_CrisisStategies +LhHCSCat_EmergencyStrategies,
                                      LhHCSCat_finalphase = case_when(
                                        LhHCSCat_EmergencyStrategies >= 20 ~ 4,
                                        crisisemergency >= 20 | LhHCSCat_CrisisStategies > 20 ~ 3,
                                        crisisemergency >= 20 & LhHCSCat_EmergencyStrategies < 20 ~ 3,
                                        LhHCSCat_NoStrategies < 80 & LhHCSCat_CrisisStategies < 20 ~ 2,
                                        LhHCSCat_NoStrategies >= 80 ~ 1,
                                        TRUE ~  NA_real_)) %>% select(-stresscrisisemergency, -crisisemergency)
#HHS
Indicateurs_nga <- Indicateurs_nga %>% mutate(phase2345 = `HHS_Phase2` + `HHS_Phase3` + `HHS_Phase4` + `HHS_Phase5`,
                                      phase345 = `HHS_Phase3` + `HHS_Phase4` + `HHS_Phase5`,
                                      phase45 = `HHS_Phase4` + `HHS_Phase5`,
                                      HHS_finalphase = case_when(
                                        HHS_Phase5 >= 20 ~ 5,
                                        HHS_Phase4 >= 20 | phase45 >= 20 ~ 4,
                                        HHS_Phase3 >= 20 | phase345 >= 20 ~ 3,
                                        HHS_Phase2 >= 20 | phase2345 >= 20 ~ 2,
                                        HHS_Phase1 >= 20 ~ 1,
                                        TRUE ~  NA_real_)) %>% select(-phase2345, -phase345, -phase45)
#import csv
nga_nov2021_cur <- read_csv("data/csv/nga/nga_nov2021_cur.csv", lazy = FALSE) 
nga_nov2021_cur <- nga_nov2021_cur %>%  mutate_at(c("adm0_name","adm1_name"), ~stri_trans_general(.x, id = "Latin-ASCII")) %>% mutate_at(c("adm0_name","adm1_name"), ~str_to_title(.x))

#join1
Indicateurs_nga_join1 <- Indicateurs_nga %>% filter(adm1_name %in% c("Adamawa","Borno","Yobe","Abia","Bauchi"))
nga_nov2021_cur_join1 <- nga_nov2021_cur %>% filter(adm1_name %in% c("Adamawa","Borno","Yobe","Abia","Bauchi"))
#check join
sux1 <- anti_join(nga_nov2021_cur_join1, Indicateurs_nga_join1, by = c("adm0_name","adm1_name","adm2_name"))
sux2 <- anti_join(Indicateurs_nga_join1, nga_nov2021_cur_join1, by = c("adm0_name","adm1_name","adm2_name"))
#recode for Borno, some IDP areas (Yobe areas cannot be joined because they are split into inaccessible vs accessible)
Indicateurs_nga_join1 <- Indicateurs_nga_join1 %>% mutate(adm2_name = recode(adm2_name,
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
nga_nov2021_cur_join1 <- left_join(nga_nov2021_cur_join1, Indicateurs_nga_join1, by = c("adm0_name","adm1_name","adm2_name"))


#join2
Indicateurs_nga_join2 <- Indicateurs_nga %>% filter(adm1_name %in% c("Zamfara","Cross River","Edo","Niger"))
nga_nov2021_cur_join2 <- nga_nov2021_cur %>% filter(adm1_name %in% c("South","East","North","Cross River/C","Cross River/N","Cross River/S","Edo/N","Edo/C","Edo/S","Western Zone","Northern Zone","Central Zone"))
#recode
Indicateurs_nga_join2 <- Indicateurs_nga_join2 %>% mutate(adm1_name = case_when(
  #Niger
  adm2_name == "Agaie" ~ "South",
  adm2_name == "Chanchaga" ~ "East",
  adm2_name == "Kontagora" ~ "North",
  #Cross River
  adm2_name == "Cross River Central" ~ "Cross River/C",
  adm2_name == "Cross River North" ~ "Cross River/N",
  adm2_name == "Cross River South" ~ "Cross River/S",
  #Edo
  adm2_name == "Akoko-Edo" ~ "Edo/N",
  adm2_name == "Esan West" ~ "Edo/C",
  adm2_name == "Oredo" ~ "Edo/S",
  #Zamfara
  adm2_name == "Zamfara West" ~ "Western Zone",
  adm2_name == "Zamfara North" ~ "Northern Zone",
  adm2_name == "Zamfara Central" ~ "Central Zone")) %>%
  select(adm1_name, FCG_Poor:rCSI_finalphase)
#check join
sux1 <- anti_join(nga_nov2021_cur_join2, Indicateurs_nga_join2, by = c("adm1_name"))
sux2 <- anti_join(Indicateurs_nga_join2, nga_nov2021_cur_join2, by = c("adm1_name"))
#join current
nga_nov2021_cur_join2 <- left_join(nga_nov2021_cur_join2, Indicateurs_nga_join2, by = c("adm1_name"))


#join3
Indicateurs_nga_join3 <- Indicateurs_nga %>% filter(adm1_name %in% c("Benue", "Enugu", "Federal Capital Territory", "Jigawa", "Kano", "Kaduna", "Katsina", "Kebbi", "Gombe", "Lagos", "Plateau", "Sokoto", "Taraba")) %>% select(adm1_name, adm2_name,FCG_Poor:rCSI_finalphase)                                                                  
nga_nov2021_cur_join3 <- nga_nov2021_cur %>% filter(adm1_name %in% c("Benue", "Enugu", "Federal Capital Territory", "Jigawa", "Kano", "Kaduna", "Katsina", "Kebbi", "Gombe", "Lagos", "Plateau", "Sokoto", "Taraba")) 

nga_nov2021_cur_join3 <- nga_nov2021_cur_join3 %>% mutate(adm2_key = case_when(
  #benue
  adm2_name %in% c("Katsina-Ala","Konshisha","Kwande","Logo","Ukum","Ushongo","Vandeikya") ~ "Northeast (Zone A)",
  adm2_name %in% c("Buruku","Gboko","Guma","Gwer East","Gwer West","Makurdi","Tarka") ~ "Northwest (Zone B)",
  adm2_name %in% c("Ado","Agatu","Apa","Obi","Ogbadibo","Ohimini","Oju","Okpokwu","Oturkpo") ~ "South (Zone C)",
  #enugu
  adm2_name %in% c("Igbo Etiti","Igbo Eze North","Igbo Eze South","Nsukka", "Udenu","Uzo Uwani") ~ "Enugu North",
  adm2_name %in% c("Aninri","Awgu","Ezeagu","Oji River","Udi") ~ "Enugu West",
  adm2_name %in% c("Enugu North","Enugu South","Enugu East","Isi Uzo","Nkanu East","Nkanu West") ~ "Enugu East",
  #FCT
  adm2_name %in% c("Abuja Municipal","Bwari") ~ "Urban",
  adm2_name %in% c("Abaji","Gwagwalada","Kuje","Kwali") ~ "Peri-Urban/Rural",
  #gombe
  adm2_name %in% c("Akko","Yamaltu Deba") ~ "Akko",
  adm2_name %in% c("Balanga","Billiri","Kaltungo","Shongom") ~ "Balanga",
  adm2_name %in% c("Dukku","Funakaye","Gombe","Kwami","Nafada") ~ "Dukku",
  #jigawa
  adm2_name %in% c("Birni Kudu","Buji","Dutse","Gwaram","Jahun","Kiyawa","Miga") ~ "Jigawa South-West",
  adm2_name %in% c("Auyo","Biriniwa","Guri","Hadejia","Kafin Hausa","Kaugama","Kiri Kasamma","Malam Madori") ~ "Jigawa North-East",
  adm2_name %in% c("Babura","Gagarawa","Garki","Gumel","Gwiwa","Kazaure","Maigatari","Roni","Sule-Tankarkar","Taura","Yankwashi","Ringim") ~ "Jigawa North-West",
  #kano
  adm2_name %in% c("Dala","D/Kudu","Fagge","G/Mallam","Gezawa","Gwale","Municipal","Kumbotso","Kura","Madobi","Minjibir","Nassarawa","Tarauni","Ungogo","Warawa") ~ "Central",
  adm2_name %in% c("Bagwai","Bichi","Danbatta","D/Tofa","Gabasawa","Gwarzo","Kabo","Karaye","Kunchi","Makoda","R/Gado","Shanono","Tofa","Tsanyawa") ~ "North",
  adm2_name %in% c("Ajingi","Albasu","Bebeji","Bunkure","Doguwa","Garko","Gaya","Kibiya","Kiru","Rano","Rogo","Sumaila","Takai","T/Wada","Wudil") ~ "South",
  #kaduna
  adm2_name %in% c("Birnin Gwari","Chikun","Giwa","Igabi","Kaduna North","Kaduna South","Kajuru") ~ "Kaduna Central",
  adm2_name %in% c("Jaba","Jema'a","Kachia","Kagarko","Kaura","Kauru","Sanga","Zango Kataf") ~ "Kaduna South",
  adm2_name %in% c("Ikara","Kubau","Kudan","Lere","Makarfi","Sabon Gari","Soba","Zaria") ~ "Kaduna North",
  #katsina
  adm2_name %in% c("Baure","Bindawa","Daura","Dutsi","Ingawa","Kankia","Kusada","Mai'adua","Mani","Mashi","Sandamu","Zango") ~ "Katsina North",
  adm2_name %in% c("Bakori","Dandume","Danja","Faskari","Funtua","Kafur","Kankara","Malumfashi","Matazu","Musawa","Sabuwa") ~ "Katsina South",
  adm2_name %in% c("Batagarawa","Batsari","Charanchi","Dan Musa","Dutsin-Ma","Jibia","Kaita","Katsina","Kurfi","Rimi","Safana") ~ "Katsina Central",
  #kebbi
  adm2_name %in% c("Arewa","Argungu","Augie","Bagudo","Dandi","Jega","Suru") ~ "Argungu",
  adm2_name %in% c("Aliero","Birnin Kebbi","Bunza","Gwandu","Kalgo","Koko/Besse","Maiyama") ~ "Birnin_kebbi",
  adm2_name %in% c("Fakai","Ngaski","Sakaba","Shanga","Danko/Wasagu","Yauri","Zuru") ~ "Zuru",
  #Lagos
  adm2_name %in% c("Agege","Ajeromi_ifelodun","Alimosho","Amuwo_odofin","Badagry","Ifako_ijaiye","Ikeja","Mushin","Ojo","Oshodi_osolo") ~ "Lagos West",
  adm2_name %in% c("Epe","Ibeju_lekki","Ikorodu","Kosofe","Shomolu") ~ "Lagos East",
  adm2_name %in% c("Apapa","Eti_osa","Lagos_island","Lagos_mainland","Surulere") ~ "Lagos Central",
  #Plateau
  adm2_name %in% c("Barkin Ladi","Bassa","Jos East","Jos North","Jos South","Riyom") ~ "Bassa",
  adm2_name %in% c("Bokkos","Kanam","Kanke","Mangu","Pankshin") ~ "Bokkos",
  adm2_name %in% c("Langtang North","Langtang South","Mikang","Qu'an Pan","Shendam","Wase") ~ "Shendam",
  #Sokoto 
  adm2_name %in% c("Binji","Gudu","Kware","Silame","Sokoto North","Sokoto South","Tangaza","Wamakko") ~ "Sokoto North",
  adm2_name %in% c("Gada","Goronyo","Gwadabawa","Illela","Isa","Rabah","Sabon Birni","Wurno") ~ "Sokoto East",
  adm2_name %in% c("Bodinga","Dange/Shuni","Kebbe","Shagari","Tambuwal","Tureta","Yabo") ~ "Sokoto South",
  #Taraba
  adm2_name %in% c("Bali","Gashaka","Gassol","Kurmi","Sardauna") ~ "Bali",
  adm2_name %in% c("Donga","Ibi","Takum","Ussa","Wukari") ~ "Ibi",
  adm2_name %in% c("Ardo Kola","Jalingo","Karim Lamido","Lau","Yorro","Zing") ~ "Lau"))
#check join
sux1 <- anti_join(nga_nov2021_cur_join3, Indicateurs_nga_join3, by = c("adm1_name", "adm2_key" = "adm2_name"))
sux2 <- anti_join(Indicateurs_nga_join3, nga_nov2021_cur_join3, by = c("adm1_name", "adm2_name" = "adm2_key")) 
#join current
nga_nov2021_cur_join3 <- left_join(nga_nov2021_cur_join3, Indicateurs_nga_join3, by = c("adm1_name", "adm2_key" = "adm2_name")) %>% select(-adm2_key)



#put it all together
nga_nov2021_cur <- bind_rows(nga_nov2021_cur_join1,nga_nov2021_cur_join2,nga_nov2021_cur_join3)
#write
nga_nov2021_cur %>% write_csv("C:/RBD_CHIPC_dataprocessing/data/csv/nga/nga_nov2021_cur.csv")

