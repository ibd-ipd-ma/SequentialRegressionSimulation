## Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


################################################################################

## Study CERTIFI (NCT00771667)

## Locates files and variables of interest from data dictionary
# * demo     - VISIT, AGE, SEX, RACE, TRTGRPI, HEIGHT, WEIGHT, (BMI), IMM (IMMUNOMOD)
# * viscdai  - CDAI
# * rxclass  - RXDRUG (HxOfTNFi)
# * conmeds  - (STEROID)
# * cd_hx    - LOCALIXATION (COLON, ILEAL)
# * condhist - CHCONDD (SMOKING)
# * labchem  - LBSTRENSN (ALBUMIN)
# * vislabef - CRP
# * subjef   - BFIST (CURR.FISTULA)
# 
# Details for viscdai.xpt TRTCDI (trt code induction), TRTGRPI (trt group induction)
# * 1 = Placebo             (PLACEBO ARM)
# * 2 = Ustekinumab 1 mg/kg (Not FDA approval dose; don't include)
# * 3 = Ustekinumab 3 mg/kg (Not FDA approval dose; don't include)
# * 4 = Ustekinumab 6 mg/kg (FDA DOSE)

################################################################################

library(haven); library(data.table); library(magrittr); library(tidyverse)

################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/NCT00771667/Data/Data/'

visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16')

participants <- read_xpt(paste0(PATH, 'demo.xpt')) %>% 
  filter(TRTCDI %in% c(1,4)) %>%
  select(DUSUBJID, TRTGRPI)

TRIAL_OUT <- data.frame(
  DUSUBJID = rep(participants %>% pull(DUSUBJID), each=length(visits)),
  VISIT    = rep(visits, times=dim(participants)[[1]]) 
)

# Add treatment group
TRIAL_OUT <- left_join(TRIAL_OUT, participants) %>%
  mutate(TRTGRP = if_else(toupper(TRTGRPI) == 'PLACEBO', 'Placebo', 'Active'))

TRIAL_OUT %>% head()

################################################################################

## CDAI Values
# Remove participants if baseline is NA, or WEEK1:WEEK8 are all NA.
# Extract CDAI and CDAIL (Last Observation Carried Forward (LVCF)) values for 
#   each participant in cohorts ACTARM placebo or active
# Pivot longer to create a single record per patient. 


# viscdai - CDAI values
cdai <- read_xpt(paste0(PATH,'viscdai.xpt')) %>% 
  filter(TRTCDI %in% c(1,4)) %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  # filter included visits 
  filter(VISIT %in% visits) %>%
  select(DUSUBJID, TRTGRPI, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>% 
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(DUSUBJID) %>% fill(CDAIL, .direction='downup') %>% ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )
dim(TRIAL_OUT)

################################################################################

# missingness by TRTGRP
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK8) %>% 
  summarise(across(everything(), ~sum(is.na(.))))
# 10 missing from week 8

################################################################################

## Baseline Covariates


## AGE, SEX, RACE
TRIAL_OUT <- read_xpt(paste0(PATH, 'demo.xpt')) %>% 
  mutate(RACE = if_else(RACE=='Caucasian', 'WHITE', 
                        if_else(RACE=='Black', 'BLACK', 
                                if_else(RACE=='Asian', 'ASIAN', 'OTHER')))) %>%
  select(DUSUBJID, AGE, SEX, RACE) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read_xpt(paste0(PATH, 'demo.xpt')) %>% 
  # BMI = WEIGHT (kg) / (HEIGHT (cm) / 100 )^2   #kg/m^2
  mutate(BMI = BWEIGHT / (HEIGHT/100)^2,
         HEIGHT..cm = HEIGHT,
         WEIGHT..kg = BWEIGHT) %>%
  select(DUSUBJID, BMI, HEIGHT..cm, WEIGHT..kg) %>% 
  left_join(TRIAL_OUT,.)

## IMMUNOMOD
imm <- 'METHOTREXATE|MERCAPTOPURINE|AZATH|6-MP|6MP'

TRIAL_OUT <- read_xpt(paste0(PATH, 'conmeds.xpt')) %>% 
  filter(VISIT %in% c("Screening", "Week 0")) %>%
  mutate(temp = if_else( CMDECOD %like% imm | CMDECOD1 %like% imm, 'Yes', 'No' )) %>%
  group_by(DUSUBJID) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)

## STEROID
ster <- 'CORTICOSTEROIDS|PREDNISOLONE|PREDNISONE|BUDESONIDE|METHYLPREDNISOLONE|DECORTIN|ENTOCORT|MEDROL|RHINOCORT'

TRIAL_OUT <- read_xpt(paste0(PATH, 'conmeds.xpt')) %>% 
  filter(VISIT %in% c("Screening", "Week 0")) %>%
  mutate(temp = if_else( CMDECOD %like% ster | CMDECOD1 %like% ster, 'Yes', 'No' )) %>%
  group_by(DUSUBJID) %>%
  summarise(STEROID = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)

## HX.TNFi
tnf <- c('Adalimumab', 'Certolizumab Pegol', 'Infliximab')

TRIAL_OUT <- read_xpt(paste0(PATH, 'rxclass.xpt')) %>% 
  # find if participant took at least one TNF drug
  mutate(RXDRUG_BIN = if_else(RXDRUG %in% tnf, 'Yes', 'No')) %>%
  select(DUSUBJID, RXDRUG_BIN) %>%
  group_by(DUSUBJID) %>%
  summarise(HxOfTNFi = max(RXDRUG_BIN)) %>% 
  left_join(TRIAL_OUT,.)


## CRP
TRIAL_OUT <- read_xpt(paste0(PATH, 'vislabef.xpt')) %>%
  filter(VISIT == 'Baseline') %>%
  mutate(CRP..mgL = CRP) %>%
  select(DUSUBJID, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read_xpt(paste0(PATH, 'cd_hx.xpt')) %>%
  mutate(LOC.ILEAL = ILEUM) %>%
  mutate(LOC.COLON = COLON) %>%
  select(DUSUBJID, LOC.COLON, LOC.ILEAL) %>%
  left_join(TRIAL_OUT,.)


## Participants without records of using TNFI, steroid and immunomodulator are imputed with 'No'
TRIAL_OUT <- TRIAL_OUT %>%
  mutate(across(c(LOC.ILEAL, LOC.COLON), ~replace(., . == "", "No")))


## DURATION
TRIAL_OUT <- read_xpt(paste0(PATH, 'cd_hx.xpt')) %>%
  mutate(DURATION = DIS_DUR) %>%
  select(DUSUBJID, DURATION) %>%
  left_join(TRIAL_OUT,.)


## ALBUMIN
TRIAL_OUT <- read_xpt(paste0(PATH,'labchem.xpt')) %>% 
  filter(VISIT == 'Screening', LBTEST == 'Albumin') %>%
  # LBACTDY = actual day of lab test - edge case to remove 1 NA
  filter(LBACTDY < 0) %>% 
  # find most recent (max) lab record for those with multiple tests
  group_by(DUSUBJID) %>% 
  filter(LBACTDY == max(LBACTDY)) %>% 
  ungroup() %>% 
  #
  mutate(ALBUMIN..gL = LBSTRESN) %>%
  select(DUSUBJID, ALBUMIN..gL) %>% 
  left_join(TRIAL_OUT,.)


## SMOKING
TRIAL_OUT <- read_xpt(paste0(PATH, 'condhist.xpt')) %>% 
  filter(VISIT=="Screening", CHCONDD=="Cigarette Smoking, past or current") %>%
  mutate(SMOKING = CHHIST) %>%
  select(DUSUBJID, SMOKING) %>% 
  left_join(TRIAL_OUT,.)


## CURR.FISTULA
TRIAL_OUT <- read_xpt(paste0(PATH, 'subjef.xpt')) %>% 
  mutate(CURR.FISTULA = BFIST) %>%
  select(DUSUBJID, CURR.FISTULA) %>% 
  left_join(TRIAL_OUT,.)

## [PERIANAL]: missing

## [CURR.PRIOR.STRICTURE]: missing

## [ETHNICITY]: missing

dim(TRIAL_OUT)

################################################################################

## Arrange Final Dataframe

additional_vars <- c(
  RACE = NA_character_, ETHNIC = NA_character_, HEIGHT..cm = NA_real_, WEIGHT..kg = NA_real_, 
  LOC.COLON = NA_character_, DURATION = NA_real_, SMOKING = NA_character_, ALBUMIN..gL = NA_real_, 
  CURR.FISTULA = NA_character_, PERIANAL = NA_character_, CURR.PRIOR.STRICTURE = NA_character_
)

# add trial identifiers and missing columns
TRIAL_OUT <- TRIAL_OUT %>%
  rename(ID = DUSUBJID) %>%
  # add identifiers
  mutate(TRIAL= 'CERTIFI', 
         DRUG = 'UST',
         YEAR = 2008) %>% 
  # add missing additional variables 
  tibble::add_column(!!!additional_vars[!names(additional_vars) %in% names(.)]) %>% 
  # re-arrange
  select(ID, TRTGRP, TRIAL, DRUG, YEAR, CDAI_BASELINE:CDAIL_WEEK16, 
         AGE, SEX, BMI, CRP..mgL, HxOfTNFi, STEROID, IMMUNOMOD, LOC.ILEAL,
         RACE, ETHNIC, HEIGHT..cm, WEIGHT..kg, LOC.COLON, DURATION, SMOKING,
         ALBUMIN..gL, CURR.FISTULA, PERIANAL, CURR.PRIOR.STRICTURE) %>%
  arrange(TRTGRP)

view(TRIAL_OUT)

################################################################################

## Save

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/CERTIFI_MASTER.csv', row.names = F)

################################################################################

