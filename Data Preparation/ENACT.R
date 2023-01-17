# Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study ENACT(NCT00032786)

## Locates files and variables of interest from data dictionary
# demog   - GRPNAME, GENDER (SEX), AGE
# cdaisco - CDAI, X_CDAIQUE=='Total Score'
# phys1   - STDHGHT
# cdmdiet - STEROID, IMMUNOMOD, HxOfTNFi
# lab_sp1 - STDRESLT (CRP..mgL)
# dhistcd - _DISSITE (LOCATION: ILEAL, COLON), WEIGHT
# 
# smokingh- SMOKQUAN (SMOKING)

library(haven); library(data.table); library(magrittr); library(tidyverse)

###########################################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/DataFromFirstResearchEnvironment/Source Data/NCT00032799/ct-2018-013_cd301_v01/'
visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read.csv(paste0(PATH,'demog.csv')) %>%
  select(PATID, GRPNAME)

TRIAL_OUT <- data.frame(
  PATID    = rep(participants %>% pull(PATID), each=length(visits)),
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


cdai <- read.csv(paste0(PATH,'cdaisco.csv')) %>%
  filter(X_CDAIQUE=='Total Score') %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = as.character(TPT_NAME)) %>%
  mutate(VISIT = if_else(TPT_NAME=='WEEK 0', 'BASELINE', VISIT)) %>%
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  filter(VISIT %in% visits) %>%
  # select cdai values
  mutate(CDAI = CDAISCRE) %>%
  select(PATID, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>% 
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(PATID) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )
dim(TRIAL_OUT)

# # find participants where all recorded cdai values are NA
# id_na <- TRIAL_OUT %>%
#   select(PATID, CDAI_WEEK2, CDAI_WEEK4, CDAI_WEEK4, CDAI_WEEK8) %>%
#   filter_at(vars(-PATID), ~is.na(.)) %>%
#   select(PATID)
# 
# # remove participant if baseline cdai is NA or 
# # recorded cdai values are NA (id_na)
# TRIAL_OUT <- TRIAL_OUT %>%
#   filter(!is.na(CDAI_BASELINE)) %>%
#   anti_join(., id_na)
# # 26 patients removed due to insufficient data
# 
# dim(TRIAL_OUT)

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise(across(everything(), ~sum(is.na(.))))

###########################################################################################################

## Baseline Covariates

## AGE, SEX
TRIAL_OUT <- read.csv(paste0(PATH,'demog.csv')) %>%
  mutate(SEX = as.character(GENDER)) %>%
  # remove age ranges
  mutate(AGE = as.character(AGE)) %>% 
  mutate(AGE = if_else(AGE=='65-69', '67', 
               if_else(AGE=='70-74', '72',
               if_else(AGE=='75+', '75', AGE)))) %>%
  mutate(AGE = as.double(AGE)) %>%
  select(PATID, AGE, SEX) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT
TRIAL_OUT <- read.csv(paste0(PATH,'phys1.csv')) %>%
  filter(TPT_NAME=='SCREENING') %>%
  mutate(HEIGHT..cm = STDHGHT) %>%
  select(PATID, HEIGHT..cm) %>% 
  left_join(TRIAL_OUT,.)


## WEIGHT
TRIAL_OUT <- read.csv(paste0(PATH,'dhistcd.csv')) %>%
  mutate(WEIGHT..kg = WEIGHT) %>%
  select(PATID, WEIGHT..kg) %>% 
  left_join(TRIAL_OUT,.)


## BMI
TRIAL_OUT <- TRIAL_OUT %>%
  mutate(BMI = WEIGHT..kg/(HEIGHT..cm/100)^2 )


## IMMUNOMOD 
imm <- 'METHOTREXATE|MERCAPTOPURINE|AZATH'

TRIAL_OUT <- read.csv(paste0(PATH,'conmeds.csv')) %>%
  # immunomodulator treatment for Crohn's Disease
  filter(INDICATI %like% 'CD|CROHN') %>% 
  mutate(temp = if_else(WHODESC %like% imm, 'Yes', 'No')) %>% 
  group_by(PATID) %>% 
  summarise(IMMUNOMOD = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## STEROID 
ster <- 'CORTICOSTEROIDS|PREDNISOLONE|PREDNISONE|BUDESONIDE|DECORTIN|ENTOCORT|MEDROL|RHINOCORT'

TRIAL_OUT <- read.csv(paste0(PATH,'conmeds.csv')) %>%
  # immunomodulator treatment for Crohn's Disease
  filter(INDICATI %like% 'CD|CROHN') %>% 
  mutate(temp = if_else(WHODESC %like% ster, 'Yes', 'No')) %>% 
  group_by(PATID) %>% 
  summarise(STEROID = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## HX.TNFi 
TRIAL_OUT <- read.csv(paste0(PATH,'cdmdiet.csv')) %>%
  filter(TPT_NAME == 'SCREENING') %>% 
  filter(X_MDCATEG=='Anti-TNF Therapy') %>%
  mutate(temp = 'Yes') %>%
  group_by(PATID) %>%
  summarise(HxOfTNFi = max(temp)) %>% 
  left_join(TRIAL_OUT,.)
  

#### Impute NA -> No for categorical vars (IMMUNOMOD, STEROID, HxOfTNFi)
# TRIAL_OUT <- TRIAL_OUT %>% 
#   mutate(across(c(IMMUNOMOD, STEROID, HxOfTNFi), ~ replace_na(.x, 'No')))


## CRP
TRIAL_OUT <- read.csv(paste0(PATH,'lab_sp1.csv')) %>%
  filter(TPT_NAME %in% c('WEEK 0'),
         # N = numeric result (exclude: S (subjective), C (non numeric))
         RESTYPE == 'N') %>%
  # isolate most recent test (for patients with multiple tests)
  group_by(PATID) %>%
  filter(as.Date(EXAMDTXT) == max(as.Date(EXAMDTXT))) %>%
  ungroup() %>%
  # get CRP
  mutate(CRP..mgL = STDRESLT) %>%
  select(PATID, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read.csv(paste0(PATH,'dhistcd.csv')) %>% 
  mutate(LOC.ILEAL = if_else(DISSITE %in% c('I','IC'), 'Yes', 'No'),
         LOC.COLON = if_else(DISSITE %in% c('C','IC'), 'Yes', 'No') ) %>%
  select(PATID, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)


## [DURATION]: missing
## [ALBUMIN]: missing
## [SMOKING]: missing
## [CURR.FISTULA]: missing
## [PERIANAL]: missing
## [CURR.PRIOR.STRICTURE]: missing
## [RACE]: missing
## [ETHNIC]: missing

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
  rename(ID = PATID) %>%
  # add identifiers
  mutate(TRIAL= 'ENACT', 
         DRUG = 'NTZ',
         YEAR = 2001) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/ENACT_MASTER.csv', row.names = F)

###########################################################################################################

