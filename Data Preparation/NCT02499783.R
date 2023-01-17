## Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study NCT02499783

## Locates files and variables of interest from data dictionary
# deid_adcdai         - CDAI, CDAIL
# deid_adsl (sub lvl) - TRTGRP, AGE, SEX, RACE, DURATION (DURDS), STEROID (CORBL), IMMUNOMOD (IMMBL), 
#                       CRPBL, ALBBL, WTBL (WEIGHT), HTBL (HEIGHT), BMIBL (BMI)
# deid_dm (demo)      - ETHNIC
# deid_mh             - ILEAL, COLON
# deid_su (sub use)   - SMOKING
# deid_pe (phy exam)  - PERIANAL, CURR.FISTULA, PRIOR.FISTULA
# 
# 
# Notes: No HxOfTNFi for all patients. Study required TNFi-naive. 
# 
# Details on actual treatment (deid_adsl)
# * TRTA: PBO/ADA - 160/80 MG (Placebo)
# * TRTA: ADA - 160/80/40 MG  (Active)


library(haven); library(data.table); library(magrittr); library(tidyverse)

###########################################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/NCT02499783/'
visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

# Placebo: 'PBO/ADA - 160/80 MG'
# Active:  'ADA - 160/80/40 MG'
participants <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>% # subject level information
  filter(TRTA %in% c('ADA - 160/80/40 MG')) %>%
  select(USUBJID, TRTA)

TRIAL_OUT <- data.frame(
  USUBJID = rep(participants %>% select(USUBJID) %>% pull(), each=length(visits)),
  VISIT   = rep(visits, times=dim(participants)[[1]]) 
)

# add treatment group
TRIAL_OUT <- TRIAL_OUT %>% 
  mutate(TRTGRP = 'Active')

TRIAL_OUT %>% head()

###########################################################################################################

## CDAI Values

# Remove participants if baseline is NA, or WEEK1:WEEK8 are all NA.
# Extract CDAI and CDAIL (Last Observation Carried Forward (LVCF)) values for each participant in cohorts ACTARM placebo or active
# Pivot longer to create a single record per patient. 


# Notes: Weeks >8 are OLE (open-label), so placebo data will stop at week 8
cdai <- read_sas(paste0(PATH,'deid_adcdai.sas7bdat')) %>%
  filter(grepl('DB', AVISIT)) %>%
  # manually filter weeks0-8 cdai
  filter(AVISIT=='BASELINE (WEEK 0 DB TREATMENT PERIOD)'&VISIT=='Baseline (Week 0)' |
         AVISIT=='WEEK 2 DB TREATMENT PERIOD'&VISIT=='Week 2' |
         AVISIT=='WEEK 4 DB TREATMENT PERIOD'&VISIT=='Week 4' |
         AVISIT=='WEEK 6 DB TREATMENT PERIOD'&VISIT=='Week 6' |
         AVISIT=='WEEK 8 DB TREATMENT PERIOD'&VISIT=='Week 8') %>%
  
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = if_else(VISIT=='Baseline (Week 0)','Baseline',VISIT)) %>%
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  # filter(VISIT %in% visits) %>%
  mutate(CDAI = O_AVAL) %>%
  select(USUBJID, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>%
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(USUBJID) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant (need to unnest cols to avoid weird error)
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL), values_fn=list ) %>%
  unnest(cols = everything())

# # find participants where valid weeks are not all NA
# id_na <- TRIAL_OUT %>%
#   select(USUBJID, CDAI_WEEK2, CDAI_WEEK4, CDAI_WEEK6, CDAI_WEEK8) %>%
#   filter_at(vars(-USUBJID), ~is.na(.)) %>%
#   select(USUBJID)
# 
# # remove participant if baseline cdai is NA or 
# # even weeks btw WEEK2:WEEK8 are NA
# TRIAL_OUT <- TRIAL_OUT %>%
#   filter(!is.na(CDAI_BASELINE)) %>%
#   anti_join(., id_na)
# # N removed = 0
# 
# dim(TRIAL_OUT)

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise_each(funs(sum(is.na(.))))
# 16 missing from week 8 (LVCF from week 6, week 4, week 2)


###########################################################################################################

## Baseline Covariates 

## AGE, SEX, RACE
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  # only Asian participants in this study
  mutate(RACE = toupper(RACE)) %>%
  select(USUBJID, AGE, SEX, RACE) %>% 
  left_join(TRIAL_OUT,.)

## ETHNIC
TRIAL_OUT <- read_sas(paste0(PATH,'deid_dm.sas7bdat')) %>%
  mutate(ETHNIC = ifelse(ETHNIC == "None", "Not Hispanic or Latino", ETHNIC)) %>%
  select(USUBJID, ETHNIC) %>% 
  left_join(TRIAL_OUT,.)

## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(WEIGHT..kg = WTBL,
         HEIGHT..cm = HTBL,
         BMI        = BMIBL) %>%
  select(USUBJID, WEIGHT..kg, HEIGHT..cm, BMI)%>% 
  left_join(TRIAL_OUT,.)


## IMMUNOMOD
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(IMMUNOMOD = if_else(IMMBL=='Y','Yes','No')) %>%
  select(USUBJID, IMMUNOMOD) %>% 
  left_join(TRIAL_OUT,.)


## STEROID
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(STEROID = if_else(CORBL=='Y','Yes','No')) %>%
  select(USUBJID, STEROID) %>% 
  left_join(TRIAL_OUT,.)


## HX.TNFi (no history of TNFi)
TRIAL_OUT <- TRIAL_OUT %>% 
  add_column(HxOfTNFi = 'No')


## CRP
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(CRP..mgL = CRPBL) %>%
  select(USUBJID, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read_sas('G:/Source Data/NCT02499783/deid_mh.sas7bdat') %>% # medical history
  filter(MHGRPID == "Crohn's Disease History",
         VISIT == "Screening") %>%
  mutate(Values = "Yes") %>%
  pivot_wider(id_cols = USUBJID, names_from = MHLOC, values_from = Values) %>%
  mutate(LOC.ILEAL = Ileal,
         LOC.COLON = Colonic) %>%
  select(USUBJID, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)


## Participants without records of having ileal involvement are imputed with 'No'
TRIAL_OUT <- TRIAL_OUT %>%
  mutate(across(c(LOC.ILEAL, LOC.COLON), ~ replace_na(.x, 'No')))


## DURATION
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(DURATION = DURDS) %>%
  select(USUBJID, DURATION) %>% 
  left_join(TRIAL_OUT,.)


## ALBUMIN
TRIAL_OUT <- read_sas(paste0(PATH,'deid_adsl.sas7bdat')) %>%
  mutate(ALBUMIN..gL= ALBBL) %>%
  select(USUBJID, ALBUMIN..gL) %>% 
  left_join(TRIAL_OUT,.)


## SMOKING
smok <- c('Cigarettes','Cigars','Pipes')

TRIAL_OUT <- read_sas(paste0(PATH,'deid_su.sas7bdat')) %>%
  filter(SUTRT %in% smok) %>%
  pivot_wider(id_cols = USUBJID, names_from = SUTRT, values_from = SUUSE) %>%
  mutate(SMOKING = ifelse(Cigarettes=="Current" | Cigars=="Current" | Pipes=="Current", "Yes",'No')) %>%
  select(USUBJID, SMOKING) %>%
  left_join(TRIAL_OUT,.)


## CURR.FISTULA
TRIAL_OUT <- read_sas(paste0(PATH,'deid_pe.sas7bdat')) %>% # physical exam
  filter(VISIT == "Screening",
         PETEST %in% c("Number of perianal/anal fistulas", "Number of abdominal fistulas")) %>%
  # Yes if at least one condition is true (> 0)
  mutate(temp = if_else(PEORRES > 0, 'Yes', 'No')) %>%
  group_by(USUBJID) %>%
  summarise(CURR.FISTULA = max(temp)) %>%
  select(USUBJID, CURR.FISTULA) %>% 
  left_join(TRIAL_OUT,.)


## PERIANAL
TRIAL_OUT <- read_sas(paste0(PATH,'deid_pe.sas7bdat')) %>% # physical exam
  filter(VISIT == "Screening",
         PETEST %in% c("Number of perianal/anal fistulas")) %>%
  mutate(PERIANAL = if_else(PEORRES > 0, 'Yes', 'No')) %>%
  select(USUBJID, PERIANAL) %>% 
  left_join(TRIAL_OUT,.)

## [CURR.PRIOR.STRICTURE]: missing

dim(TRIAL_OUT)

###########################################################################################################

## Arrange Final Dataframe

additional_vars <- c(
  RACE = NA_character_, ETHNIC = NA_character_, HEIGHT..cm = NA_real_, WEIGHT..kg = NA_real_, 
  LOC.COLON = NA_character_, DURATION = NA_real_, SMOKING = NA_character_, ALBUMIN..gL = NA_real_, 
  CURR.FISTULA = NA_character_, PERIANAL = NA_character_, CURR.PRIOR.STRICTURE = NA_character_
  )

# add trial identifiers and missing columns
TRIAL_OUT <- TRIAL_OUT %>%
  rename(ID = USUBJID) %>%
  # add identifiers
  mutate(TRIAL= 'NCT02499783', 
         DRUG = 'ADA',
         YEAR = 2015) %>% 
  # add missing additional variables 
  tibble::add_column(!!!additional_vars[!names(additional_vars) %in% names(.)]) %>% 
  # re-arrange
  select(ID, TRTGRP, TRIAL, DRUG, YEAR, CDAI_BASELINE:CDAIL_WEEK16, 
         AGE, SEX, BMI, CRP..mgL, HxOfTNFi, STEROID, IMMUNOMOD, LOC.ILEAL,
         RACE, ETHNIC, HEIGHT..cm, WEIGHT..kg, LOC.COLON, DURATION, SMOKING,
         ALBUMIN..gL, CURR.FISTULA, PERIANAL, CURR.PRIOR.STRICTURE) %>%
  arrange(TRTGRP)

view(TRIAL_OUT)

###########################################################################################################

## Save

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/NCT02499783_MASTER.csv', row.names = F)

###########################################################################################################

