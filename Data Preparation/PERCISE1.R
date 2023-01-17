## Variables of interest: 
#             patient ID; patient assigned group; week of visit; 
#             week 0 and week 8 CDAI; age; sex; race; ethnicity; weight; height; BMI; baseline smoking status;
#             history use of TNFi; baseline use of immunomodulator; baseline use of oral steroid;
#             baseline c-reactive protein (crp); ileal involvement; baseline Albumin; disease duration; 
#             current fistula; perianal; current prior stricture. 


###########################################################################################################

## Study PRECISE1 (NCT00152490)

## Locates files and variables of interest from data dictionary
# effcdai             - NCDAI (CDAI), BCRP (CRP)
# patpat (subj lvl)   - TREAT, AGE, SEXL, RACEL, HEI (HEIGHT), WEIGHT, BMI, SMOKEL (SMOKING)
# histher             - INFLIXL (HxOfTNFi)
# medcort             - RECSTERL (STEROID)
# medmed_redacted     - WHOPTNM (IMMUNOMOD)
# hiscrd              - CDDUR (DURATION), LOCL (LOCATION)
# effcla              - AFISTULL/OFISTULL (CURR.FISTULA)
# abche               - PRMCODL (ALBUMIN)
# 
# Details on treatment groups (patpat)
# * TREAT: Trt1 (Placebo)
# * TREAT: Trt2 (Active) == CDP870 400mg


library(haven); library(data.table); library(magrittr); library(tidyverse)

###########################################################################################################

## Create dataframe with selected participants and week of visit

PATH = 'G:/Source Data/files_UCB-RP5652-C87031/Files/C87031 ADaM/'

visits <- c('BASELINE','WEEK1','WEEK2','WEEK3','WEEK4','WEEK6','WEEK8','WEEK10','WEEK12','WEEK14','WEEK16') 

participants <- read_sas(paste0(PATH,'patpat.sas7bdat')) %>% # patient information
  filter(TREAT %in% c('Trt1','Trt2')) %>%
  select(PATNO, TREAT)

TRIAL_OUT <- data.frame(
  PATNO   = rep(participants %>% select(PATNO) %>% pull(), each=length(visits)),
  VISIT   = rep(visits, times=dim(participants)[[1]]) 
)

# add treatment group
TRIAL_OUT <- left_join(TRIAL_OUT, participants) %>%
  mutate(TRTGRP = if_else(TREAT=='Trt1','Placebo','Active'))
dim(TRIAL_OUT)

TRIAL_OUT %>% head()

###########################################################################################################

## CDAI Values

# Remove participants if baseline is NA, or WEEK1:WEEK8 are all NA.
# Extract CDAI and CDAIL (Last Observation Carried Forward (LVCF)) values for each participant in cohorts ACTARM placebo or active
# Pivot longer to create a single record per patient. 

cdai <- read_sas(paste0(PATH,'effcdai.sas7bdat')) %>%
  # filter included visits (all letters to upper cases, remove white space) - to match visits vector
  mutate(VISIT = if_else(VISRCD == 0,'BASELINE', VISITL)) %>%
  mutate(VISIT = gsub('\\s+','',toupper(VISIT))) %>%
  filter(VISIT %in% visits) %>%
  mutate(CDAI = NCDAI) %>%
  select(PATNO, VISIT, CDAI)

TRIAL_OUT <- left_join(TRIAL_OUT, cdai) %>%
  # LVCF
  mutate(CDAIL = CDAI) %>%
  group_by(PATNO) %>%
  fill(CDAIL, .direction='downup') %>%
  ungroup() %>% 
  pivot_wider( names_from = VISIT, values_from = c(CDAI, CDAIL) )

dim(TRIAL_OUT)

# # find participants where even weeks btw WEEK2:WEEK8 are NA
# id_na <- TRIAL_OUT %>%
#   select(PATNO, CDAI_WEEK2, CDAI_WEEK4, CDAI_WEEK6, CDAI_WEEK8) %>%
#   filter_at(vars(-PATNO), ~is.na(.)) %>%
#   select(PATNO)
# 
# # remove participant if baseline cdai is NA or 
# # even weeks btw WEEK2:WEEK8 are NA
# TRIAL_OUT <- TRIAL_OUT %>%
#   filter(!is.na(CDAI_BASELINE)) %>%
#   anti_join(., id_na)
# # N removed = 60
# 
# dim(TRIAL_OUT)

###########################################################################################################

# missingness per trt group
TRIAL_OUT %>% group_by(TRTGRP) %>% select(TRTGRP, CDAI_BASELINE:CDAI_WEEK16) %>% 
  summarise_each(funs(sum(is.na(.))))
# 177 missing from week 8 (LVCF from week 6, week 4, week 2)

###########################################################################################################

## Baseline Covariates 

## AGE, SEX, RACE
TRIAL_OUT <- read_sas(paste0(PATH,'patpat.sas7bdat')) %>%
  mutate(RACE = if_else(RACEL=='Caucasian', 'WHITE',
                if_else(RACEL=='Afro-caribbean','BLACK',
                if_else(RACEL=='Other','OTHER', 'ASIAN')))) %>%
  mutate(SEX  = if_else(SEXL=='Male','M','F')) %>%
  select(PATNO, AGE, SEX, RACE) %>% 
  left_join(TRIAL_OUT,.)


## HEIGHT, WEIGHT, BMI
TRIAL_OUT <- read_sas(paste0(PATH,'patpat.sas7bdat')) %>%
  mutate(WEIGHT..kg = WEIGHT,
         HEIGHT..cm = HEI*100) %>%
  select(PATNO, WEIGHT..kg, HEIGHT..cm, BMI)  %>% 
  left_join(TRIAL_OUT,.)


## IMMUNOMOD
TRIAL_OUT <- read_sas(paste0(PATH,'medmed_redacted.sas7bdat')) %>%
  mutate(temp = if_else(grepl("METHOTREXATE|MERCAPTOPURINE|AZATH",WHOPTNM),'Yes','No')) %>%
  group_by(PATNO) %>%
  summarise(IMMUNOMOD = max(temp)) %>% 
  left_join(TRIAL_OUT,.)


## STEROID
TRIAL_OUT <- read_sas(paste0(PATH,'medcort.sas7bdat')) %>%
  mutate(STEROID = RECSTERL) %>%
  select(PATNO, STEROID) %>% 
  left_join(TRIAL_OUT,.) %>% 
  # replace empty cells with NA
  mutate(across(c(STEROID), ~ na_if(.,'')))


## HX.TNFi
TRIAL_OUT <- read_sas(paste0(PATH,'histher.sas7bdat')) %>% 
  mutate(HxOfTNFi = INFLIXL) %>%
  select(PATNO, HxOfTNFi) %>% 
  left_join(TRIAL_OUT,.)
  

## CRP
TRIAL_OUT <- read_sas(paste0(PATH,'effcdai.sas7bdat')) %>%
  filter(VISIT==0) %>%
  mutate(CRP..mgL = BCRP) %>%
  select(PATNO, CRP..mgL) %>% 
  left_join(TRIAL_OUT,.)


## ILEAL, COLON
# LOC: 1 - L1-Terminal ileum 
#      2 - L2-Colon
#      3 - L3-Ileocolon
TRIAL_OUT <- read_sas(paste0(PATH,'hiscrd.sas7bdat')) %>% 
  mutate(LOC.ILEAL= if_else(LOC %in% c(1,3),'Yes','No'),
         LOC.COLON= if_else(LOC %in% c(2,3),'Yes','No')) %>%
  select(PATNO, LOC.ILEAL, LOC.COLON) %>% 
  left_join(TRIAL_OUT,.)


## DURATION
TRIAL_OUT <- read_sas(paste0(PATH,'hiscrd.sas7bdat')) %>% 
  mutate(DURATION = CDDUR) %>%
  select(PATNO, DURATION) %>% 
  left_join(TRIAL_OUT,.)


## ALBUMIN
# TRIAL_OUT <- read_sas('G:/Source Data/C87031/IPD/labche.sas7bdat') %>% 
#   filter(VISIT==0, PRMCODL=='Albumin')


## SMOKING
TRIAL_OUT <- read_sas(paste0(PATH,'patpat.sas7bdat')) %>%
  mutate(SMOKING = if_else(SMOKEL=='Current Smoker', 'Yes', 'No')) %>%
  select(PATNO, SMOKING) %>% 
  left_join(TRIAL_OUT,.)


## CURR.FISTULA
TRIAL_OUT <- read_sas(paste0(PATH,'effcla.sas7bdat')) %>% 
  filter(VISIT==0) %>%
  mutate(CURR.FISTULA = if_else(AFISTULL=="Present"|OFISTULL=="Present", 'Yes', 'No')) %>%
  select(PATNO, CURR.FISTULA) %>% 
  left_join(TRIAL_OUT,.)

## [PERIANAL]: missing

## [CURR.PRIOR.STRICTURE]: missing

## [ETHNICITY]: missing

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
  rename(ID = PATNO) %>%
  # add identifiers
  mutate(TRIAL= 'PRECISE1', 
         DRUG = 'CZP',
         YEAR = 2003) %>% 
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

write.csv(TRIAL_OUT, 'G:/Shan/Week 8 Identification/PRECISE1_MASTER.csv', row.names = F)

###########################################################################################################

