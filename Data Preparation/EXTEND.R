# Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study EXTEND (NCT00348283)

## Locates files and variables of interest from data dictionary
# deid_rn        - USUBJID, TRTGRP
# deid_qs (cdai) - USUBJID, CPEVENT, QSRN
# 
# deid_dm (demo) - USUBJID, AGE, SEX, RACE, ETHNICITY, COUNTRY
# deid_vs        - USUBJID, VSWTKG (WEIGHT), VSHTCM (HEIGHT), (BMI)
# deid_cmhcm     - USUBJID, CMPT (STEROID), CMPT (IMMUNO)
# deid_cmho      - USUBJID, CMPT (HxOfTNFi)
# deid_dc        - USUBJID (FISTULA)
# deid_dh        - USUBJID, DHLILEM (ILEAL)
# deid_mh_crohn  - USUBJID, QSDT - MHSTDT (DURATION)
# deid_lb        - USUBJID, LBTEST (CRP), LBTEST (ALBUMIN)

# Details for deid_rn TRTGRP:
# 1 = ACTIVE ADALIMUMAB (ACTIVE ARM ONLY)
# 2 = PLACEBO (DON'T INCLUDE)

# Details for deid_qs (cdai values):
# Filter QSQID=='10A', CDAI values under QSRN


library(haven); library(data.table); library(magrittr); library(tidyverse); library(data.table)

###########################################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/NCT00348283/'
visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read_sas(paste0(PATH,'deid_rn.sas7bdat')) %>% 
  filter(TRTGRP == 'ACTIVE ADALIMUMAB') %>% 
  select(USUBJID, TRTGRP) 

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

cdai <- read_sas(paste0(PATH,'deid_qs.sas7bdat')) %>% 
  filter(QSQID=='10A') %>% 
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = gsub('\\s+','',toupper(CPEVENT))) %>%
  filter(VISIT %in% visits) %>%
  mutate(CDAI = QSRN) %>%
  select(USUBJID, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>%
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(USUBJID) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )

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
# # N removed = 5
# 
# dim(TRIAL_OUT) #64

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise(across(everything(), ~sum(is.na(.))))

###########################################################################################################

## Baseline Covariates 


## AGE, SEX, RACE, ETHNIC
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_dm.sas7bdat')) %>% 
  # convert RACE=LPC to =ASIAN or =OTHER
  mutate(RACE = if_else(RACECD=='A','ASIAN', 
                if_else(RACECD=='O','OTHER', 
                # if missing (M) -> NA
                if_else(RACECD=='M', NA_character_, RACE)))) %>%
  select(USUBJID, AGE, SEX, RACE, ETHNIC) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_vs.sas7bdat')) %>% 
  filter(CPEVENT=='SCREENING') %>% 
  mutate(WEIGHT..kg = VSWTKG) %>%
  mutate(HEIGHT..cm = VSHTCM) %>% 
  # kg/m^2
  mutate(BMI = WEIGHT..kg/(HEIGHT..cm/100)^2 ) %>% 
  select(USUBJID,WEIGHT..kg, HEIGHT..cm, BMI) %>% 
  left_join(TRIAL_OUT,.)


## IMMUNOMOD
imm <- "METHOTREXATE|MERCAPTOPURINE|AZATH"

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_cmhcm.sas7bdat')) %>%
  mutate(temp = if_else(CMPT %like% imm, 'Yes', 'No')) %>%
  group_by(USUBJID) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)


## STEROID
steroid_yes <- read.csv(file='G:/All Analysis/Doug/Week 6 identification/CleanedCrohnsData-07-14-2021.csv') %>% 
  filter(Trial=='EXTEND') %>% filter(SteroidUse=='Yes') %>% mutate(ID = as.character(ID)) %>% select(ID) %>% pull()

TRIAL_OUT <- TRIAL_OUT %>% 
  mutate(STEROID = if_else(USUBJID %in% steroid_yes, 'Yes', 'No'))


## HX.TNFi
# data located in two locations: cmho, cmhcm
tnfi <- "INFLIXIMAB|ADALIMUMAB|CERTOLIZUMAB"

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_cmho.sas7bdat')) %>%
  mutate(temp = if_else(CMPT %like% tnfi, 'Yes', 'No')) %>%
  group_by(USUBJID) %>%
  summarise(HxOfTNFi1 = max(temp)) %>% 
  ungroup() %>%
  left_join(TRIAL_OUT,.)

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_cmhcm.sas7bdat')) %>% 
  # can't manually check for TNF drugs
  mutate(temp = if_else(CMCAT %like% 'ANTI-TUMOR', 'Yes', 'No')) %>%
  group_by(USUBJID) %>%
  summarise(HxOfTNFi2 = max(temp)) %>% 
  ungroup() %>%
  left_join(TRIAL_OUT,.)

TRIAL_OUT <- TRIAL_OUT %>% 
  mutate(HxOfTNFi = if_else(HxOfTNFi1=='Yes' | HxOfTNFi2=='Yes', 'Yes', 'No')) %>% 
  select(-c(HxOfTNFi1, HxOfTNFi2))


## CRP
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_lb.sas7bdat')) %>% 
  filter(CPEVENT=='BASELINE') %>%
  filter(grepl('C-REACTIVE', LBTEST)) %>%
  mutate(CRP..mgL = LBORRESN) %>% 
  select(USUBJID, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_dh.sas7bdat')) %>%
  mutate(LOC.ILEAL = if_else(DHLILEM=='X','Yes','No')) %>%
  mutate(LOC.COLON = if_else(DHLCOL=='X','Yes','No')) %>%
  select(USUBJID, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)


## ALBUMIN
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_lb.sas7bdat')) %>% 
  filter(CPEVENT=='BASELINE') %>%
  filter(grepl('ALBUMIN', LBTEST)) %>% 
  mutate(ALBUMIN..gL = LBORRESN) %>%
  select(USUBJID, ALBUMIN..gL) %>% 
  left_join(TRIAL_OUT,.)


## PERIANAL
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_dc.sas7bdat')) %>%
  filter(CPEVENT=='BASELINE',
         DCLOC=='PERIANAL') %>%
  mutate(PERIANAL = if_else(DCNOCC==0, 'No', 'Yes')) %>% 
  select(USUBJID, PERIANAL) %>% 
  left_join(TRIAL_OUT,.)

## [CURR.PRIOR.STRICTURE]: missing
## [SMOKING]: missing
## [CURR.FISTULA]: missing
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
  rename(ID = USUBJID) %>%
  # add identifiers
  mutate(TRIAL= 'EXTEND', 
         DRUG = 'ADA',
         YEAR = 2006) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/EXTEND_MASTER.csv', row.names = F)

###########################################################################################################

