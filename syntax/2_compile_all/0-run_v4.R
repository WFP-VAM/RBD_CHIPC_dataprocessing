library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(writexl)
library(fs)
library(stringdist)
library(googledrive)
library(googlesheets4)

#Notes
#how to include all - even those with phaseclass missing or 0
#find the few duplicates in geo dictionary
#how to back up geo dictionary
#CAR IPC data can definetly be cleaned up 

## File template
file_regex <- "^\\w{3}_(mar|nov|jun)\\d{4}_(cur|proj)\\.csv"
##
files <- dir_ls("data/csv", type = "file", recurse = TRUE)
## Check if all files follow the patterns
stopifnot(all(str_detect(basename(files), file_regex)))
files[which(!str_detect(basename(files), file_regex))]

exercise_code <- function(file) {
  case_when(
    str_detect(file, "nov") ~ 1L,
    str_detect(file, "mar") ~ 2L,
    str_detect(file, "jun") ~ 3L,
    TRUE ~ 0L
  )
}

exercise_label <- function(file) {
  case_when(
    str_detect(file, "nov") ~ "Sep-Dec",
    str_detect(file, "mar") ~ "Jan-May",
    str_detect(file, "jun") ~ "Jun-Aug",
    TRUE ~ ""
  )
}

exercise_year <- function(file)
  as.integer(str_extract(file, "\\d{4}"))

reference_code <- function(file) {
  if_else(str_detect(file, "proj"), if_else(str_detect(file, "nov2014"), 2L, 3L), exercise_code(file))
}

reference_label <- function(file) {
  if_else(str_detect(file, "proj"), if_else(str_detect(file, "nov2014"), "Jan-May", "Jun-Aug"), exercise_label(file))
}

reference_year <- function(file) {
  ex_year <- exercise_year(file)
  ex_code <- exercise_code(file)
  if_else(str_detect(file, "proj") & ex_code == 1L, ex_year + 1L, ex_year)
}


guess_file_encoding <- function(file) {
  guess_encoding(file) %>%
    slice(1) %>%
    pull(encoding)
}

dd <- map(files, function(l) {
  df <- suppressMessages(read_csv(l, locale = locale(encoding = guess_file_encoding(l))))
  df$exercise_year <- exercise_year(l)
  df$exercise_code <- exercise_code(l)
  df$reference_year <- reference_year(l)
  df$reference_code <- reference_code(l)
  df$exercise_label <- exercise_label(l)
  df$reference_label <- reference_label(l)
  df$chtype <- if_else(str_detect(l, "proj"), "projected", "current")
  df
})

## Bind all data
df <- bind_rows(dd)

# ## Remove accent and replace them by non ascii / proper case version
df <- df %>% mutate(adm0_name = iconv(str_to_title(adm0_name), from = "UTF-8",to = "ASCII//TRANSLIT"), 
                    adm1_name = iconv(str_to_title(adm1_name), from = "UTF-8",to = "ASCII//TRANSLIT"),
                    adm2_name = iconv(str_to_title(adm2_name), from = "UTF-8",to = "ASCII//TRANSLIT"))
df$adm0_name[str_which(df$adm0_name, regex("ivoire", ignore_case = TRUE))] <- "Cote d'Ivoire"  


### Remove areas that werent analyzed i.e. no population figures
all <- df %>%  filter(!is.na(population))




## Add geo dictionnary
url <- "https://docs.google.com/spreadsheets/d/1S9OPO-x8YUQbpJ06mrbOUDgZgQroJkbtwPjYM-9Eo6g/edit?usp=sharing"
geo_dict <- read_sheet(url, sheet = "geo_dict", col_types = "c")


## Inner join with the geo_dict table - why is the variable order coming out

## Inner join with the geo_dict table - some line(s) creating duplication of 8 rows
# seems to work with removing gaulcodes
all_matched <- left_join(all, geo_dict) %>%
  dplyr::select(-adm0_5_name, -adm1_name, -adm1_5_name, -adm2_name, -adm2_5_name, -adm3_name,
         adm0_name,  adm0_pcod3, adm0_pcod2,
         adm0_5_name = adm0_5_namechanged, 
         adm1_name = adm1_namechanged, adm1_pcod2,
         adm1_5_name = adm1_5_namechanged,  adm1_5_pcod2,
         adm2_name = adm2_namechanged, adm2_pcod2,
         adm2_5_name = adm2_5_namechanged,  adm2_5_pcod2,
         adm3_name = adm3_namechanged, adm3_pcod2) %>%  distinct()


## Check if any admin names dont match between data and geo-dictionary
all_non_matched <- distinct(anti_join(all, geo_dict))
all_non_matched


## Clean names, start with Caps - review if all this is necessary
all_matched <- mutate_if(all_matched, is.numeric, ~ as.integer(round(.x)))
all_matched$phase_class[all_matched$phase_class == 0] <- NA


### Reorganise columuns for export
 all_matched <- dplyr::select(all_matched,
                       adm0_name, adm0_pcod2, adm0_pcod3,
                       adm0_5_name, adm0_5_pcod2, 
                       adm1_name, adm1_pcod2,
                       adm1_5_name, adm1_5_pcod2,
                       adm2_name, adm2_pcod2,
                       adm2_5_name, adm2_5_pcod2,
                       adm3_name, adm3_pcod2,
                       exercise_code, exercise_label, exercise_year, chtype, reference_code, reference_label, reference_year,
                       population,phase_class,phase1,phase2,phase3,phase4,phase5,phase35,
                       foodconsumption_phase,livelihoods_phase,nutrition_phase,mortality_phase,
                       FCG_Poor, FCG_Borderline,FCG_Acceptable,	FCG_finalphase,
                       HDDS_Phase1,	HDDS_Phase2,	HDDS_Phase3,	HDDS_Phase4,	HDDS_Phase5,	HDDS_finalphase,
                       HHS_Phase1,	HHS_Phase2,	HHS_Phase3,	HHS_Phase4,	HHS_Phase5,	HHS_finalphase,
                       LhHCSCat_NoStrategies,	LhHCSCat_StressStrategies,	LhHCSCat_CrisisStategies,	LhHCSCat_EmergencyStrategies,	LhHCSCat_finalphase,
                       rCSI_Phase1,	rCSI_Phase2,	rCSI_Phase3,	rCSI_finalphase,
                       notes)


# Because there are a lot of problems with pre 2017 data, 
#out of all the data combined, only select 2014 to current for  Sahel/Nigeria 
# and after 2017 to current for other countries
sahel_countries_pcod3 <- c("BFA", "MLI", "MRT", "NER", "NGA", "SEN", "TCD")
final <- all_matched %>% filter(adm0_pcod3 %in% sahel_countries_pcod3 | (!adm0_pcod3 %in% sahel_countries_pcod3 & exercise_year >= 2017))

###
adm0_fiche_comm <- read_excel("data/fiche_comm/adm0_fiche_comm.xlsx")
glimpse(adm0_fiche_comm)

df_orig <- final %>%
  group_by(adm0_name, chtype, exercise_year, exercise_code, exercise_label, reference_year, reference_code, reference_label) %>%
  summarise(total_phase35 = sum(phase35, na.rm = TRUE)) %>%
  mutate(total_phase35_data = round(total_phase35)) %>%
  dplyr::select(-total_phase35) %>%
  ungroup()

###
df_fiche <- adm0_fiche_comm %>%
  group_by(adm0_name, chtype, exercise_year, exercise_code, reference_year, reference_code) %>%
  summarise(total_phase35 = sum(phase35, na.rm = TRUE)) %>%
  mutate(total_phase35_fiche = round(total_phase35)) %>%
  dplyr::select(-total_phase35) %>%
  ungroup() %>%
  mutate(chtype = tolower(chtype))

compar_df <- left_join(df_orig, df_fiche) %>%
  dplyr::select(-reference_code, -exercise_code) %>%
  mutate(diff_abs = abs(total_phase35_data - total_phase35_fiche),
         diff_perc = round(100 * (diff_abs / total_phase35_data), 2))

compar_df %>% View()
compar_df %>% write_xlsx("data/processed/compar_fiche.xlsx")



##CH_data only
#create concatenation of exercise/reference 
final <- final %>% 
  mutate(period = paste(exercise_label, exercise_year, chtype, reference_label, reference_year, sep = "_"),
         period_country = paste(adm0_pcod3,exercise_label, exercise_year, chtype, reference_label, reference_year, sep = "_"))
#filter only cadre harmonise data
cadre_harmonise <- final %>%  filter(adm0_pcod3 != "CAF") 
#create variable that shows whether to use period or not
cadre_harmonise <-  cadre_harmonise %>% mutate(usethisperiod = case_when(
  #use the jun update instead of the january projections for burkina, nigeria and togo
  period %in% c("Jun-Aug_2020_current_Jun-Aug_2020") ~ "Y",
  period_country %in% c("BFA_Jan-May_2020_projected_Jun-Aug_2020","NGA_Jan-May_2020_projected_Jun-Aug_2020","TGO_Jan-May_2020_projected_Jun-Aug_2020") ~ "N",
  #use january current and projections
  period %in% c("Jan-May_2014_current_Jan-May_2014","Jan-May_2014_projected_Jun-Aug_2014","Jan-May_2015_current_Jan-May_2015","Jan-May_2015_projected_Jun-Aug_2015",
                "Jan-May_2016_current_Jan-May_2016","Jan-May_2016_projected_Jun-Aug_2016","Jan-May_2017_current_Jan-May_2017","Jan-May_2017_projected_Jun-Aug_2017",
                "Jan-May_2018_current_Jan-May_2018","Jan-May_2018_projected_Jun-Aug_2018","Jan-May_2019_current_Jan-May_2019","Jan-May_2019_projected_Jun-Aug_2019",
                "Jan-May_2020_current_Jan-May_2020","Jan-May_2020_projected_Jun-Aug_2020","Jan-May_2021_current_Jan-May_2021","Jan-May_2021_projected_Jun-Aug_2021") ~ "Y",
  #use only september current estimates
  period %in% c("Sep-Dec_2014_current_Sep-Dec_2014","Sep-Dec_2015_current_Sep-Dec_2015","Sep-Dec_2016_current_Sep-Dec_2016","Sep-Dec_2017_current_Sep-Dec_2017","Sep-Dec_2018_current_Sep-Dec_2018",
                "Sep-Dec_2019_current_Sep-Dec_2019","Sep-Dec_2020_current_Sep-Dec_2020","Sep-Dec_2021_current_Sep-Dec_2021","Sep-Dec_2021_projected_Jun-Aug_2022") ~ "Y",
              
  TRUE ~ "N")) 

cadre_harmonise <-  cadre_harmonise %>% dplyr::select(-period, -period_country)

cadre_harmonise %>%  write_xlsx("data/processed/cadre_harmonise.xlsx")



##CAR IPC data only - new is broken down at adm2 level where old is only adm0 from old IPC reports
caf_ipc_new <- final %>% filter(adm0_pcod3 == "CAF") 
#Central African Republic - jan/may current 2020 and 2021 are really september (projections) but fine for now
caf_ipc_new <- caf_ipc_new %>% mutate(usethisperiod = case_when(period_country %in% c("CAF_Jun-Aug_2019_projected_Sep-Dec_2019",
                                                                                      "CAF_Sep-Dec_2019_projected_Jun-Aug_2020",
                                                                                      "CAF_Sep-Dec_2020_projected_Jun-Aug_2021") ~ "N", TRUE ~ "Y"))



#import old CAR IPC data pulled from reports
caf_ipc_old <- read_excel("data/raw/IPC_CAF/adm0_CAF_IPC_reports.xlsx") 
#combine old and new CAR
caf_ipc <- bind_rows(caf_ipc_new, caf_ipc_old) %>% dplyr::select(-period, -period_country, -adm0_gaulcode, -foodconsumption_phase:-rCSI_finalphase, -data_source)
write_xlsx(caf_ipc, "data/processed/caf_ipc.xlsx")




#combine CH and CAR IPC data
cadre_harmonise_caf_ipc <- bind_rows(cadre_harmonise, caf_ipc) 
#export the final thing
cadre_harmonise_caf_ipc %>%  write_xlsx("data/processed/cadre_harmonise_caf_ipc.xlsx")




