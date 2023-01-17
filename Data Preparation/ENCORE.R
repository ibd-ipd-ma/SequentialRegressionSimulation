# Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study ENCORE(NCT00078611)

## Locates files and variables of interest from data dictionary
# c7demog   - GRPNAME, GENDER (SEX), AGE
# c7cdaisc  - CDAI0, CDAI4, CDAI8, CDAI12
# c7phys1   - STDHGHT, STDWGHT, (BMI)
# c7trthis  - MEDNAME (STEROID, IMMUNOMOD, HxOfTNFi)
# c7labsp1  - STDRESLT (CRP..mgL)
# c7dhist   - _DISSITE (LOCATION: ILEAL, COLON)
# c7labbc1  - RESULT (ALBUMIN..gL)
# c7smokih  - SMOKQUAN (SMOKING)
# c7cdaics/c7dhist - date difference (DURATION)


library(haven); library(data.table); library(magrittr); library(tidyverse); library(data.table)

###########################################################################################################

## Create dataframe with selected participants and week of visit
PATH = 'G:/Source Data/NCT00078611/ct-2019-093_cd307_data/'
visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read.csv(paste0(PATH,'c7demog.csv')) %>%
  select(DSUBJID, GRPNAME)

TRIAL_OUT <- data.frame(
  DSUBJID  = rep(participants %>% pull(DSUBJID), each=length(visits)),
  VISIT    = rep(visits, times=dim(participants)[[1]]) 
)

# Add treatment group
TRIAL_OUT <- left_join(TRIAL_OUT, participants) %>%
  mutate(TRTGRP = if_else(toupper(GRPNAME) == 'PLACEBO', 'Placebo', 'Active'))

TRIAL_OUT %>% head()

###########################################################################################################

## CDAI Values

# Remove participants if baseline is NA, or WEEK1:WEEK8 are all NA.
# Extract CDAI and CDAIL (Last Observation Carried Forward (LVCF)) values for each participant in cohorts ACTARM placebo or active
# Pivot longer to create a single record per patient. 

cdai <- read.csv(paste0(PATH,'c7cdaisc.csv')) %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = as.character(TPT_NAME)) %>%
  mutate(VISIT = if_else(TPT_NAME=='WEEK 0', 'BASELINE', VISIT)) %>%
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  filter(VISIT %in% visits) %>%
  # select cdai values
  mutate(CDAI = CDAISCRE) %>%
  select(DSUBJID, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>% 
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(DSUBJID) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )
dim(TRIAL_OUT)

# # find participants where all recorded cdai values are NA
# id_na <- TRIAL_OUT %>%
#   select(DSUBJID, CDAI_WEEK2, CDAI_WEEK4, CDAI_WEEK8) %>%
#   filter_at(vars(-DSUBJID), ~is.na(.)) %>%
#   select(DSUBJID)
# 
# # remove participant if baseline cdai is NA or 
# # recorded cdai values are NA (id_na)
# TRIAL_OUT <- TRIAL_OUT %>%
#   filter(!is.na(CDAI_BASELINE)) %>%
#   anti_join(., id_na)
# # 30 patients removed due to insufficient data
# 
# dim(TRIAL_OUT)

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise_each(funs(sum(is.na(.))))

###########################################################################################################

## Baseline Covariates 


## AGE, SEX
TRIAL_OUT <- read.csv(paste0(PATH,'c7demog.csv')) %>%
  mutate(SEX = as.character(GENDER)) %>%
  select(DSUBJID, AGE, SEX) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read.csv(paste0(PATH,'c7phys1.csv')) %>%
  filter(TPT_NAME=='SCREENING') %>%
  mutate(HEIGHT..cm = STDHGHT,
         WEIGHT..kg = STDWGHT, 
         # BMI = WEIGHT (kg) / (HEIGHT (cm) / 100 )^2   #kg/m^2
         BMI        = WEIGHT..kg / (HEIGHT..cm/100)^2 ) %>%
  select(DSUBJID, HEIGHT..cm, WEIGHT..kg, BMI)  %>% 
  left_join(TRIAL_OUT,.)


## IMMUNOMOD
TRIAL_OUT <- read.csv(paste0(PATH,'c7trthis.csv')) %>%
  # X_CURRTHE: currently on therapy
  select(DSUBJID, X_CURRTHE, MEDNAME) %>%
  # UPDATE HERE AS NEEDED
  filter(MEDNAME %like% 'METHOTREXATE|AZATHIOPRINE|IMMUNOSUPPRESANTS') %>% 
  mutate(temp = if_else(X_CURRTHE=='Yes', 'Yes', 'No')) %>%
  group_by(DSUBJID) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## STEROID
TRIAL_OUT <- read.csv(paste0(PATH,'c7trthis.csv')) %>%
  # X_CURRTHE: currently on therapy
  select(DSUBJID, X_CURRTHE, MEDNAME) %>%
  # UPDATE HERE AS NEEDED
  filter(MEDNAME %like% 'BUDESONIDE|STEROIDS') %>% 
  mutate(temp = if_else(X_CURRTHE=='Yes', 'Yes', 'No')) %>%
  group_by(DSUBJID) %>%
  summarise(STEROID = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## HX.TNFi
TRIAL_OUT <- read.csv(paste0(PATH,'c7trthis.csv')) %>%
  # X_PREVRCV: previously on therapy
  select(DSUBJID, X_PREVRCV, MEDNAME) %>%
  # UPDATE HERE AS NEEDED
  filter(MEDNAME %like% 'ANTI-TNF AGENTS') %>% 
  mutate(HxOfTNFi = if_else(X_PREVRCV=='Yes', 'Yes', 'No')) %>% 
  left_join(TRIAL_OUT,.)


## CRP
TRIAL_OUT <- read.csv(paste0(PATH,'c7labsp1.csv')) %>%
  filter(TPT_NAME=='SCREENING', 
         X_RESTYPE != 'Non result (explanation)') %>%
  # isolate most recent test (for patients with unscheduled tests)
  group_by(DSUBJID) %>% 
  filter(as.Date(EXAMDTXT) == max(as.Date(EXAMDTXT))) %>%
  ungroup() %>%
  # get CRP
  mutate(CRP..mgL = STDRESLT) %>%
  select(DSUBJID, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read.csv(paste0(PATH,'c7dhist.csv')) %>% 
  mutate(LOC.ILEAL = if_else(DISSITE %in% c('I','IC'), 'Yes', 'No'),
         LOC.COLON = if_else(DISSITE %in% c('C','IC'), 'Yes', 'No') ) %>%
  select(DSUBJID, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)

## ALBUMIN
TRIAL_OUT <- read.csv(paste0(PATH,'c7labbc1.csv')) %>%
  filter(TPT_NAME=='SCREENING',
         X_RESTYPE != 'Non result (explanation)') %>%
  # isolate most recent test (for patients with unscheduled tests)
  group_by(DSUBJID) %>% 
  filter(as.Date(EXAMDTXT) == min(as.Date(EXAMDTXT))) %>%
  ungroup() %>%
  # get CRP
  mutate(ALBUMIN..gL = STDRESLT) %>%
  select(DSUBJID, ALBUMIN..gL) %>% 
  left_join(TRIAL_OUT,.)


## SMOKING
TRIAL_OUT <- read.csv(paste0(PATH,'c7smokih.csv')) %>%
  mutate(SMOKING = X_SMOKQUA) %>%
  select(DSUBJID, SMOKING) %>% 
  left_join(TRIAL_OUT,.)

## [CURR.FISTULA]: missing
## [PERIANAL]: missing
## [CURR.PRIOR.STRICTURE]: missing
## [RACE]: missing
## [ETHNIC]: missing
## [DURATION]: missing

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
  rename(ID = DSUBJID) %>%
  # add identifiers
  mutate(TRIAL= 'ENCORE', 
         DRUG = 'NTZ',
         YEAR = 2004) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/ENCORE_MASTER.csv', row.names = F)

###########################################################################################################

