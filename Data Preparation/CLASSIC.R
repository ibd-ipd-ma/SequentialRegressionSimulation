# Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study CLASSIC 1/2(NCT00055523, NCT00055497)

## Locates files and variables of interest from data dictionary
# deid_rn        - PT, TRTGRP
# deid_cdaicdai  - PT, CPEVENT, QTXT=='Total', TSCOR (CDAI)
# 
# deid_demo      - PT, AGE, SEX, RACE
# deid_vs        - PT, VSWTKG (WEIGHT), VSHTCM (HEIGHT), (BMI)
# deid_cmhcm     - PT, CMPT (STEROID), CMPT (IMMUNO)
# deid_cmho      - PT, CMPT (HxOfTNFi)
# deid_dc        - PT (FISTULA)
# deid_dh        - PT, DHLILEM (ILEAL)
# deid_mh_crohn  - PT, QSDT - MHSTDT (DURATION)
# deid_lb        - PT, LBTEST (CRP), LBTEST (ALBUMIN)
# 
# Details on deid_rn TRTGRP:
# * 1 = PLACEBO 
# * 2 = 20 MG
# * 3 = 40 MG
# * 4 = 80 MG   (FDA drug dose; ACTIVE)
# 
# Details on deid_cdaicdai CPEVENT:
# * BASELINE, WEEK 1, 2, 4 (normal)
# * WEEK 6  = WEEK 2_EXT
# * WEEK 8  = WEEK 4_EXT
# * WEEK 12 = WEEK 8_EXT
# * WEEK 16 = WEEK 12_EXT

library(haven); library(data.table); library(magrittr); library(tidyverse)

###########################################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/NCT00055497/'
visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read_sas(paste0(PATH,'deid_rn.sas7bdat')) %>%
  # only TRTGRP (4) - RANDOME (80 MG)
  filter(TRTGRP == 4) %>% 
  select(PT, RANDOM)

TRIAL_OUT <- data.frame(
  PT      = rep(participants %>% select(PT) %>% pull(), each=length(visits)),
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

cdai <- read_sas(paste0(PATH,'deid_cdaicdai.sas7bdat')) %>%
  filter(QTXT== "Total") %>%
  mutate(CPEVENT = if_else(CPEVENT=='WEEK 2_EXT','WEEK 6',
                   if_else(CPEVENT=='WEEK 4_EXT','WEEK 8',
                   if_else(CPEVENT=='WEEK 8_EXT','WEEK 12',
                   if_else(CPEVENT=='WEEK 12_EXT','WEEK16', CPEVENT))))) %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = gsub('\\s+','',toupper(CPEVENT))) %>%
  filter(VISIT %in% visits) %>%
  # select columns
  mutate(CDAI = TSCOR) %>% 
  select(PT, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>%
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(PT) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )

# # find participants where valid weeks are not all NA
# id_na <- TRIAL_OUT %>%
#   select(PT, CDAI_WEEK1, CDAI_WEEK2, CDAI_WEEK4, CDAI_WEEK6, CDAI_WEEK8) %>%
#   filter_at(vars(-PT), ~is.na(.)) %>%
#   select(PT)
# 
# # remove participant if baseline cdai is NA or 
# # even weeks btw WEEK2:WEEK8 are NA
# TRIAL_OUT <- TRIAL_OUT %>%
#   filter(!is.na(CDAI_BASELINE)) %>%
#   anti_join(., id_na)
# # N removed = 0
# 
# dim(TRIAL_OUT) # 73

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise_each(funs(sum(is.na(.))))
# 6 missing from week 8 (LVCF from week 6, week 4, week 2, or week 1)

###########################################################################################################

## Baseline Covariates


## AGE, SEX, RACE
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_demo.sas7bdat')) %>%
  mutate(RACE = if_else(RACE1=='INDALASKA','OTHER', RACE1)) %>%
  select(PT, AGE, SEX, RACE) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read_sas(paste0(PATH, 'deid_vsvs.sas7bdat')) %>% 
  filter(CPEVENT == 'SCREENING') %>%
  mutate(WEIGHT..kg = WGT) %>%
  mutate(HEIGHT..cm = HGT) %>%
  mutate(BMI = WEIGHT..kg / (HEIGHT..cm/100)^2 ) %>%
  select(PT, WEIGHT..kg, HEIGHT..cm, BMI) %>% 
  left_join(TRIAL_OUT,.)


## IMMUNOMOD
imm <- 'METHOTREXATE|MERCAPTOPURINE|AZATH|6-MP|6 MP'

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_cmedcmed.sas7bdat')) %>% 
  mutate(temp = if_else(MED %like% imm, 'Yes', 'No')) %>%
  group_by(PT) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)
# full list of immunomodulators:
# read_sas(paste0(PATH, 'deid_cmedcmed.sas7bdat')) %>% filter(MED %like% imm) %>% distinct(MED)


## STEROID
ster <- "CORTICOSTEROIDS|PREDNISOLONE|PREDNISONE|BUDESONIDE|DECORTIN|ENTOCORT|MEDROL|RHINOCORT"

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_chemchem.sas7bdat')) %>% 
  # ensure we are getting baseline data (from DataSparsity.R line 252)
  filter(CPEVENT == 'BASELINE') %>% 
  select(PT, SMPLDT) %>% 
  distinct() %>% 
  left_join(read_sas(paste0(PATH, 'deid_cmedcmed.sas7bdat')), .) %>% 
  filter(STRTDT < SMPLDT) %>%
  filter(STOPDT > SMPLDT | STOPDT == "") %>% 
  # select steroids
  mutate(temp = if_else( MED %like% ster | MDPT %like% ster, 'Yes', 'No')) %>%
  group_by(PT) %>%
  summarise(STEROID = max(temp)) %>%
  ungroup() %>%
  left_join(TRIAL_OUT,.)
# full list of steroids:
# read_sas(paste0(PATH, 'deid_cmedcmed.sas7bdat')) %>% filter(MED %like% ster) %>% distinct(MED)
	

## HX.TNFi
tnf <- 'ADALIMUMAB|INFLIXIMAB|CERTOLIZUMAB'

TRIAL_OUT <- read_sas(paste0(PATH, 'deid_cmedcmed.sas7bdat')) %>%
  mutate(temp = if_else(MED %like% tnf, 'Yes', 'No')) %>%
  group_by(PT) %>%
  summarise(HxOfTNFi = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)
  

## CRP
TRIAL_OUT <- read_sas(paste0(PATH,'deid_normlab2.sas7bdat')) %>%
  filter(LPARM=='CRP', CPEVENT=='SCREENING') %>%
  # convert mg/dL to mg/L
  mutate(temp = LVALUEN*10) %>%
  group_by(PT) %>%
  # take max CRP for those with multiple tests
  summarise(CRP..mgL = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
TRIAL_OUT <- read_sas(paste0(PATH,'deid_medhmedh.sas7bdat')) %>%
  filter(QTXT == 'Inflammatory bowel disease') %>%
  mutate(LOC.ILEAL = if_else(grepl('ILE', toupper(SPEC)), 'Yes', 'No') ) %>%
  mutate(LOC.COLON = if_else(grepl('COLON', toupper(SPEC)), 'Yes', 'No') ) %>%
  select(PT, SPEC, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)

## ALBUMIN
## different units than original data
TRIAL_OUT <- read_sas(paste0(PATH,'deid_normlab2.sas7bdat')) %>%
  filter(LPARM=='ALBUMIN', 
         CPEVENT=='SCREENING') %>%
  mutate(temp = LVALUEN) %>%
  group_by(PT) %>%
  summarise(ALBUMIN..gL = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)

## SMOKING
TRIAL_OUT <- read_sas(paste0(PATH,'deid_toba.sas7bdat')) %>%
  mutate(SMOKING = if_else(TOBUSE=='USER','Yes','No')) %>%
  select(PT, SMOKING) %>% 
  left_join(TRIAL_OUT,.)


## PERIANAL
TRIAL_OUT <- read_sas(paste0(PATH,'deid_medhmedh.sas7bdat')) %>%
  filter(QTXT == 'Inflammatory bowel disease') %>%
  mutate(PERIANAL  = if_else(grepl('PERIANAL', toupper(SPEC)), 'Yes', 'No') ) %>%
  select(PT, PERIANAL) %>% 
  left_join(TRIAL_OUT,.)

## [CURR.PRIOR.STRICTURE]
## [CURR.FISTULA]
## [ETHNICITY]
## [DURATION]

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
  rename(ID = PT) %>%
  # add identifiers
  mutate(TRIAL= 'CLASSIC', 
         DRUG = 'ADA',
         YEAR = 2002) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/CLASSIC_MASTER.csv', row.names = F)

###########################################################################################################

