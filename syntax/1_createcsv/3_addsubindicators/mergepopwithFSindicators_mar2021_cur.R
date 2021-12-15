library(readxl)
library(tidyverse)
library(readr)
library(stringi)

#import excel file containing consolidated 
Indicateurs <- read_excel("data/raw/CH_2021/matricesintermediares/Indicateurs.xlsm") %>% 
  select(adm0_name = ADMIN0Name,	adm1_name = ADMIN1Name, adm2_name = ADMIN2Name,
         FCG_Poor, FCG_Borderline,FCG_Acceptable,	FCG_finalphase,
         HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,
         HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	HHS_finalphase,
         LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,	LhHCSCat_finalphase,
         rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase)

#import Nigeria data
indicators_yobe <- read_excel("data/raw/CH_2021/matricesintermediares/Copy of Matrice_intermediaire_Yobe.xlsx") %>% mutate(adm0_name = "Nigeria") %>% 
  select(adm0_name, adm1_name = ADMIN1Name, adm2_name = ADMIN2Name,
         FCG_Poor, FCG_Borderline,FCG_Acceptable,	FCG_finalphase,
         HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,
         HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	HHS_finalphase,
         LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,	LhHCSCat_finalphase,
         rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase)

#add together
Indicateurs <- bind_rows(Indicateurs, indicators_yobe)

#round all %to one decimal point
Indicateurs <- Indicateurs %>% mutate(across(c(FCG_Poor, FCG_Borderline,FCG_Acceptable,
                                               HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,
                                               HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	
                                               LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,
                                               rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3), round,1))

#HHS values are wierd (% dont add up to 100%) - seems like there was a data processing error so remove all HHS
Indicateurs <- Indicateurs %>% mutate(HHS_Phase1 = NA,	HHS_Phase2 = NA,	HHS_Phase3 = NA,	HHS_Phase4 = NA,	HHS_Phase5 = NA,	HHS_finalphase = NA)
#There are also some phasings but there are no values (like rcsifinal phase but not rcsi breakdown)
Indicateurs <- Indicateurs %>% mutate(FCG_finalphase = NA, HDDS_finalphase= NA, HHS_finalphase= NA, LhHCSCat_finalphase= NA, rCSI_finalphase= NA)

#many countries did not calculate final phase so -recalculate final phase
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
         crisisemergency >= 20 & LhHCSCat_EmergencyStrategies < 20 ~ 3,
         LhHCSCat_NoStrategies < 80 & LhHCSCat_CrisisStategies < 20 ~ 2,
         LhHCSCat_NoStrategies >= 80 ~ 1,
         TRUE ~  NA_real_)) %>% select(-stresscrisisemergency, -crisisemergency)


#remove characters and proper case
Indicateurs <- Indicateurs %>%   mutate_at(c("adm0_name","adm1_name","adm2_name"), ~stri_trans_general(.x, id = "Latin-ASCII"))
Indicateurs <- Indicateurs %>%  mutate_at(c("adm0_name","adm1_name","adm2_name"), ~str_to_title(.x))
#There are some countries areas where analysis was only done at amd1 level so remove the adm2 duplication for these countries
Indicateurs_adm1 <-  Indicateurs %>% filter(adm0_name %in% c("Guinea-Bissau","Cote D'ivoire","Gambia","Ghana","Liberia")) %>% mutate(adm2_name = if_else(adm2_name == adm1_name,"",adm2_name)) %>%  mutate(adm2_name = na_if(adm2_name, ""))





#pullup and match country by country

#Burkina - 
bfa_mar2021_cur <- read_csv("data/csv/BFA/bfa_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(bfa_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
bfa_mar2021_cur <- left_join(bfa_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
bfa_mar2021_cur %>% write_csv("data/csv/BFA/bfa_mar2021_cur.csv")
 
#Cameroon - same problem as Cameroon with encoding
cmr_mar2021_cur <- read_csv("data/csv/cmr/cmr_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(cmr_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
sux2 <- anti_join(Indicateurs, cmr_mar2021_cur, by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs <- Indicateurs %>% mutate(adm1_name = case_when(
               adm2_name %in% c("Boyo","Momo") ~ "Northwest",
               adm2_name == "Menchum" ~ "Northweast",
               adm2_name == "Mezam" ~ "North West",
               TRUE ~ adm1_name))               
Indicateurs <- Indicateurs %>% mutate(adm2_name = recode(adm2_name,
                                                         "Haute-Sanaga" = "Haute Sanaga",
                                                         "Mbam-Et-Inoubou" = "Mbam Et Inoubou",
                                                         "Mbam-Et-Kim" = "Mbam Et Kim",
                                                         "Mefou-Et-Akono" = "Mefou Et Akono",
                                                         "Mefou-Et-Afamba" = "Mefou Et Afamba",
                                                         "Nyong-Et-Kelle" = "Nyong Et Kelle",
                                                         "Nyong-Et-Mfoumou" = "Nyong Et Mfoumou",
                                                         "Nyong-Et-Soo" = "Nyong Et So'o",
                                                         "Lom-Et-Djerem" = "Lom Et Djerem",
                                                         "Logone-Et-Chari" = "Logone Et Chari",
                                                         "Mayo-Danay" = "Mayo Danay")) 
cmr_mar2021_cur <- left_join(cmr_mar2021_cur,  Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
# #write
cmr_mar2021_cur %>% write_csv("data/csv/CMR/cmr_mar2021_cur.csv")


#Chad
tcd_mar2021_cur <- read_csv("data/csv/tcd/tcd_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(tcd_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
tcd_mar2021_cur <- left_join(tcd_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
tcd_mar2021_cur %>% write_csv("data/csv/tcd/tcd_mar2021_cur.csv")

#CIV - 
civ_mar2021_cur <- read_csv("data/csv/civ/civ_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(civ_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name"))
civ_mar2021_cur <- left_join(civ_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name")) 
#write
civ_mar2021_cur %>% write_csv("data/csv/civ/civ_mar2021_cur.csv")

#GMB
gmb_mar2021_cur <- read_csv("data/csv/gmb/gmb_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(gmb_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name"))
gmb_mar2021_cur <- left_join(gmb_mar2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name","adm2_name")) 
#write
gmb_mar2021_cur %>% write_csv("data/csv/gmb/gmb_mar2021_cur.csv")

#Ghana 
gha_mar2021_cur <- read_csv("data/csv/gha/gha_mar2021_cur.csv", lazy = FALSE) 
#anti-join to see if it doesnt match
sux1 <- anti_join(gha_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name"))
gha_mar2021_cur <- left_join(gha_mar2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name","adm2_name")) 
#write
gha_mar2021_cur %>% write_csv("data/csv/gha/gha_mar2021_cur.csv")


#GNB
gnb_mar2021_cur <- read_csv("data/csv/gnb/gnb_mar2021_cur.csv", lazy = FALSE) 
#anti-join to see if it doesnt match
sux1 <- anti_join(gnb_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs_adm1 <- Indicateurs_adm1 %>% mutate(adm1_name = recode(adm1_name,"Bolama/Bijagos" = "Bolama"))
gnb_mar2021_cur <- left_join(gnb_mar2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name","adm2_name")) 
#write
gnb_mar2021_cur %>% write_csv("data/csv/gnb/gnb_mar2021_cur.csv")


#LBR
lbr_mar2021_cur <- read_csv("data/csv/lbr/lbr_mar2021_cur.csv", lazy = FALSE) 
#anti-join to see if it doesnt match
sux1 <- anti_join(lbr_mar2021_cur, Indicateurs_adm1, by = c("adm0_name","adm1_name","adm2_name"))
lbr_mar2021_cur <- left_join(lbr_mar2021_cur, Indicateurs_adm1 , by = c("adm0_name","adm1_name","adm2_name")) 
#write
lbr_mar2021_cur %>% write_csv("data/csv/lbr/lbr_mar2021_cur.csv")



#Mali - issue about how to classify Koro - because accessible/nonaccessible
mli_mar2021_cur <- read_csv("data/csv/mli/mli_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(mli_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
Indicateurs <- Indicateurs %>% mutate(adm2_name = recode(adm2_name, 
                                                      "Gourma-Rharous" = "Gourma Rharous"))
mli_mar2021_cur <- left_join(mli_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
mli_mar2021_cur %>% write_csv("data/csv/mli/mli_mar2021_cur.csv") 

#Mauritania
mrt_mar2021_cur <- read_csv("data/csv/mrt/mrt_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(mrt_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
mrt_mar2021_cur <- left_join(mrt_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
mrt_mar2021_cur %>% write_csv("data/csv/mrt/mrt_mar2021_cur.csv") 

#Niger results shared were poor quality - only use the sheet (innacessible that seemed legit)
ner_mar2021_cur <- read_csv("data/csv/ner/ner_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
IndicateursNer <- Indicateurs %>% filter(adm2_name %in% c("Bosso",
                                         "Diffa_limitedaccess",
                                         "N'Guigmi_limitedaccess",
                                         "Dogondoutchi_limitedaccess",
                                         "Guidan Roumdji_limitedaccess",
                                         "Madarounfa_limitedaccess",
                                         "Tahoua_limitedaccess",
                                         "Tassara",
                                         "Tillia",
                                         "Abala",
                                         "Ayerou",
                                         "Banibangou",
                                         "Bankilare",
                                         "Filingue_limitedaccess",
                                         "Ouallam_limitedaccess",
                                         "Say_limitedaccess",
                                         "Tillaberi_limitedaccess",
                                         "Torodi",
                                         "Tera_limitedaccess"))
IndicateursNer <- IndicateursNer %>% mutate(adm2_name = recode(adm2_name, 
                                                         "Guidan Roumdji_limitedaccess" = "Guidan Roumdji_limitedaccessmarch2021",
                                                         "Madarounfa_limitedaccess" = "Madarounfa_limitedaccessmarch2021",
                                                         "Ouallam_limitedaccess" = "Ouallam_limitedaccessmarch2021",
                                                         "Tahoua_limitedaccess" = "Tahoua_limitedaccessmarch2021",
                                                         "Tera_limitedaccess" = "Tera_limitedaccessmarch2021"))
sux1 <- anti_join(IndicateursNer, ner_mar2021_cur, by = c("adm0_name","adm1_name","adm2_name"))
ner_mar2021_cur <- left_join(ner_mar2021_cur, IndicateursNer, by = c("adm0_name","adm1_name","adm2_name")) 
#write
ner_mar2021_cur %>% write_csv("data/csv/ner/ner_mar2021_cur.csv") 

#Senegal
sen_mar2021_cur <- read_csv("data/csv/sen/sen_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(sen_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
sen_mar2021_cur <- left_join(sen_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
sen_mar2021_cur %>% write_csv("data/csv/sen/sen_mar2021_cur.csv") 

#sle
sle_mar2021_cur <- read_csv("data/csv/sle/sle_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(sle_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
sle_mar2021_cur <- left_join(sle_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
sle_mar2021_cur %>% write_csv("data/csv/sle/sle_mar2021_cur.csv") 

#tgo
tgo_mar2021_cur <- read_csv("data/csv/tgo/tgo_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
sux1 <- anti_join(tgo_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name"))
tgo_mar2021_cur <- left_join(tgo_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
tgo_mar2021_cur %>% write_csv("data/csv/tgo/tgo_mar2021_cur.csv") 


#Nigeria - matrices missing for many areas and complicated - so just do Borno, Yobe & Adamawa
nga_mar2021_cur <- read_csv("data/csv/nga/nga_mar2021_cur.csv", lazy = FALSE)
#anti-join to see if it doesnt match
IndicateursNga <- Indicateurs %>% filter(adm0_name == "Nigeria") %>% filter(adm1_name %in% c("Adamawa","Borno","Yobe"))
sux1 <- anti_join(IndicateursNga, nga_mar2021_cur, by = c("adm0_name","adm1_name","adm2_name"))
#recode so names match Borno
IndicateursNga <- IndicateursNga %>%  mutate(adm2_name = recode(adm2_name,"Askirauba" = "Askirauba (Total)",
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
#
nga_mar2021_cur <- left_join(nga_mar2021_cur, Indicateurs, by = c("adm0_name","adm1_name","adm2_name")) 
#write
nga_mar2021_cur %>% write_csv("data/csv/nga/nga_mar2021_cur.csv") 



