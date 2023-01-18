## Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


################################################################################

## Study UNITI2 (NCT01369342)

## Locates files and variables of interest from data dictionary
# DM (demo) - DUSUBJID, ACTARM, AGE, SEX, RACE, ETHNIC, 
#             QSTESTCD == "CDAIX11" (WEIGHT), QSTEST == "Height" (HEIGHT)
# QS (cdai) - DUSUBJID, QSTEST=='CDAI score', VISIT, QSSTRESN
# SU        - DUSUBJID, SUTRT == "CIGARETTE", SUOCCUR == "Y" (SMOKING)
# CM        - DUSUBJID, (HxOfTNFi)
# LB        - DUSUBJID, LBTESTCD == "CRP", LBTESTCD == "CRP", LBTESTCD == "ALB"
# MH        - DUSUBJID, MHSTDY (DURATION)
# SUPPMH    - DUSUBJID, QLABEL (ILEUM, COLON)
# FA        - DUSUBJID, FACAT (STEROID), FACAT (IMMUNOMOD)
# 
# * ACTARM = PLACEBO    (1)
# * ACTARM = UST_WGT130 (Not FDA approval dose; don't include)
# * ACTARM = UST_WGT260 (3) 
# * ACTARM = UST_WGT390 (4)
# * ACTARM = UST_WGT520 (5)


library(haven); library(data.table); library(magrittr); library(tidyverse)

################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/NCT01369342/SV/'

visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read_xpt(paste0(PATH,'dm.xpt')) %>%
  filter(ACTARM %in% c('PLACEBO',
                       'UST_WGT260','UST_WGT390','UST_WGT520')) %>%
  select(DUSUBJID, ACTARM)

TRIAL_OUT <- data.frame(
  DUSUBJID = rep(participants %>% pull(DUSUBJID), each=length(visits)),
  VISIT    = rep(visits, times=dim(participants)[[1]]) 
)

# Add treatment group
TRIAL_OUT <- left_join(TRIAL_OUT, participants) %>%
  mutate(TRTGRP = if_else(ACTARM == 'PLACEBO', 'Placebo', 'Active'))

TRIAL_OUT %>% head()

################################################################################

## CDAI Values
# Remove participants if baseline is NA, or WEEK1:WEEK8 are all NA.
# Extract CDAI and CDAIL (Last Observation Carried Forward (LVCF)) values for each participant in cohorts ACTARM placebo or active
# Pivot longer to create a single record per patient. 


cdai <- read_xpt(paste0(PATH,'qs.xpt')) %>%
  filter(QSTEST == 'CDAI score') %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = if_else(VISIT=='WEEK 0', 'BASELINE', VISIT)) %>%
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  filter(VISIT %in% visits) %>%
  # select cdai values
  mutate(CDAI = QSSTRESN) %>%
  select(DUSUBJID, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>% 
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(DUSUBJID) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  # pivot wider -> one record per participant
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )

dim(TRIAL_OUT)

################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise_each(funs(sum(is.na(.))))
# 17 missing from week 8 (lvcf from week 6 or week 3)

################################################################################

## Baseline Covariates


## AGE, SEX, RACE, ETHNIC
# RACE: MULTIPLE, NOT REPORTED, UNKNOWN, OTHER == OTHER
TRIAL_OUT <- read_xpt(paste0(PATH,'dm.xpt')) %>%
  mutate(RACE = if_else(RACE == 'BLACK OR AFRICAN AMERICAN', 'BLACK', 
                if_else(RACE == 'WHITE', 'WHITE', 
                if_else(RACE == 'ASIAN', 'ASIAN', 
                # if missing -> NA
                if_else(RACE %in% c('UNKNOWN','NOT REPORTED'), NA_character_, 'OTHER'))))) %>%
  select(DUSUBJID, AGE, SEX, RACE, ETHNIC) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT
TRIAL_OUT <- read_xpt(paste0(PATH,'qs.xpt')) %>%
  filter(QSTEST=="Height", VISIT=="WEEK 0") %>%
  mutate(HEIGHT..cm = QSSTRESN) %>%
  select(DUSUBJID, HEIGHT..cm) %>% 
  left_join(TRIAL_OUT,.)


## WEIGHT
TRIAL_OUT <- read_xpt(paste0(PATH,'qs.xpt')) %>%
  filter(QSTESTCD=="CDAIX11", VISIT=="WEEK 0") %>%
  mutate(WEIGHT..kg = QSSTRESN) %>%
  select(DUSUBJID, WEIGHT..kg) %>% 
  left_join(TRIAL_OUT,.)


## BMI calculated from weight and height
TRIAL_OUT <- TRIAL_OUT %>%
  mutate(BMI = WEIGHT..kg/(HEIGHT..cm/100)^2 )


## Immunomodulator
imm <- 'METHOTREXATE|MERCAPTOPURINE|AZATH'

TRIAL_OUT <- read_xpt(paste0(PATH,'cm.xpt')) %>% 
  filter(CMINDC %like% 'CROHN') %>% 
  mutate(temp = if_else(CMDECOD %like% imm, 'Yes', 'No')) %>%
  group_by(DUSUBJID) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  ungroup() %>% 
  left_join(TRIAL_OUT,.)


## STEROID
ster <- 'CORTICOSTEROIDS|PREDNISOLONE|PREDNISONE|BUDESONIDE|DECORTIN|ENTOCORT|MEDROL|RHINOCORT'

TRIAL_OUT <- read_xpt(paste0(PATH,'FA.xpt')) %>%
  filter(FATEST == "Occurence",
         FACAT == "BASELINE CORTICOSTEROID MED REVIEW",
         FASTRESC == "Y",
         VISIT == "WEEK 0") %>%
  group_by(DUSUBJID) %>%
  summarise(STEROID = 'Yes') %>%
  left_join(TRIAL_OUT,.)


## HX.TNFi
tnfi <- 'INFLIXIMAB|ADALIMUMAB|CERTOLIZUMAB'

TRIAL_OUT <- read_xpt(paste0(PATH,'cm.xpt')) %>%
  filter(CMSCAT == "ANTI-TNF",
         CMCLAS == "TUMOR NECROSIS FACTOR ALPHA (TNF-) INHIBITORS",
         CMSPID %in% c("SCREENING-CROHN'S DISEASE MEDICATION HISTORY - 1", 
                       "SCREENING-CROHN'S DISEASE MEDICATION HISTORY - 2")) %>%
  filter(CMTRT %like% tnfi |
         CMDECOD %like% tnfi) %>%
  # re-factor occur columns
  mutate(temp = if_else(CMOCCUR=='Y', 'Yes', 'No')) %>%
  # find if patient took at least 1 tnf
  group_by(DUSUBJID) %>%
  summarise(HxOfTNFi = max(temp)) %>% 
  left_join(TRIAL_OUT,.)

## Participants without records of using TNFI, steroid and immunomodulator are imputed with 'No'
TRIAL_OUT <- TRIAL_OUT %>%
  mutate(across(c(IMMUNOMOD, STEROID, HxOfTNFi), ~ replace_na(.x, 'No')))
  

## CRP
# numeric level (LBSTRESN), NAs are partially missing or <0.20.  <0.20 is replaced by 0
TRIAL_OUT <- read_xpt(paste0(PATH,'lb.xpt')) %>%
  filter(LBTESTCD=="CRP", 
         VISIT %in% c('WEEK 0','SCREENING')) %>%
  filter(!grepl('SCREENING',LBSPID)) %>%
  mutate(temp = if_else(LBSTRESC=='<0.20', 0, LBSTRESN)) %>%
  group_by(DUSUBJID) %>%
  slice(which.max(LBDY)) %>%
  summarise(CRP..mgL = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, [COLON]
TRIAL_OUT <- read_xpt(paste0(PATH,'suppmh.xpt')) %>%
  filter(QLABEL == 'Ileum') %>%
  mutate(LOC.ILEAL = if_else(QVAL=='Y','Yes','No')) %>%
  select(DUSUBJID, LOC.ILEAL) %>% 
  left_join(TRIAL_OUT,.)


## DURATION in years
TRIAL_OUT <- read_xpt(paste0(PATH,'mh.xpt')) %>%
  filter(MHTERM == "CROHN'S DISEASE",
         MHCAT == "CROHN'S DISEASE HISTORY",
         MHSCAT == "DIAGNOSIS INFORMATION",
         MHPRESP == "Y",
         MHOCCUR == "Y",
         VISIT == "SCREENING") %>%
  mutate(DURATION = abs(as.numeric(MHSTDY))/365) %>%
  select(DUSUBJID, MHSTDY, DURATION) %>% 
  left_join(TRIAL_OUT,.)


## ALBUMIN
TRIAL_OUT <- read_xpt(paste0(PATH,'lb.xpt')) %>%
  filter(LBTESTCD=='ALB', 
         VISIT=='WEEK 0') %>%
  mutate(temp = LBSTRESN) %>%
  group_by(DUSUBJID) %>%
  summarise(ALBUMIN..gL = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## SMOKING
TRIAL_OUT <- read_xpt(paste0(PATH,'su.xpt')) %>%
  filter(SUTRT == "CIGARETTE") %>%
  mutate(SMOKING = if_else(SUOCCUR=='Y','Yes','No')) %>%
  select(DUSUBJID, SMOKING) %>% 
  left_join(TRIAL_OUT,.)

## CURR.FISTULA

## [PERIANAL]

## CURR.PRIOR.STRICTURE

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
  mutate(TRIAL= 'UNITI2', 
         DRUG = 'UST',
         YEAR = 2011) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/UNITI2_MASTER.csv', row.names = F)

################################################################################

