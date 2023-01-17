# Overview: 
# this file record published baseline characteristics and outcome summeries of each study and compare them to IPD results

# Outcome: Extended Data Figure 3: Quality Control 

library(tidyverse)
library(ggplot2)
# install.packages('naniar')
library(naniar)
# install.packages('patchwork')
library(patchwork)
library(viridis)

#####################################################################################################

## Published results

# UNITI1
uniti1GT <- tibble(
  DrugClass = rep("Il12",2), ActiveIngredient = rep("UST",2), Trial = rep("UNITI1",2), Group = c("Placebo", "Active"),
  
  # UPDATE TO WEEK 8 DATA                 
  N = c(247,249), 
  CDAI_baseline = c(319,327.6), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(20.2,37.8),
  Remission = c(7.3,20.9), # NA for week 6 
  Endpoint = c(NA,NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(37.3,37.3), Sex_Male = c(47.8,40.6), Race_White = c(NA,NA), Ethnicity_Hispanic = c(NA,NA), `Weight (kg)` = c(NA,NA), 
  `Height (cm)` = c(NA,NA), BMI = c(NA,NA), Smoking = c(NA,NA),HxOfTNFi = c(99.6,98.8), SteroidUse = c(44.9,43.4), ImmUse = c(32.8,31.3), 
  `CRP (mg/L)` = c(NA,NA), `Albumin (g/L)` = c(NA,NA), `Duration (yrs)` = c(12.1,12.7), Ileal = c(87,83.6), Perianal = c(NA,NA), 
  CurrentFistula = c(NA,NA), CurrentOrPriorStricture = c(NA,NA)
  )

# UNITI2
uniti2GT <- tibble(
  DrugClass = rep("Il12",2), ActiveIngredient = rep("UST",2), Trial = rep("UNITI2",2), Group = c("Placebo", "Active"),
  
  # UPDATE TO WEEK 8 DATA
  N = c(210,209),
  CDAI_baseline = c(302.2,302.2), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(32.1,57.9), # NA for week 6
  Remission = c(19.6,40.2), 
  Endpoint = c(28.7,55.5), # TOO 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(40.2,38.4), Sex_Male = c(47.1,43.1), Race_White = c(83.8,83.3), Ethnicity_Hispanic = c(NA,NA), `Weight (kg)` = c(74.0,71.9), 
  `Height (cm)` = c(NA,NA), BMI = c(NA,NA), Smoking = c(NA,NA), HxOfTNFi = c(37.7,31.1),SteroidUse = c(35.7,44), ImmUse = c(34.8,34.4), 
  `CRP (mg/L)` = c(NA,NA), `Albumin (g/L)` = c(NA,NA), `Duration (yrs)` = c(10.4,8.7), Ileal = c(82.4,79.4), Perianal = c(NA,NA), 
  CurrentFistula = c(15.7,14.8), CurrentOrPriorStricture = c(35.2,27.8)
  )

# CERTIFI
certifiGT <- tibble(
  DrugClass = rep("Il12",2), ActiveIngredient = rep("UST",2), Trial = rep("CERTIFI",2), Group = c("Placebo", "Active"),
      
  # UPDATE TO WEEK 8 DATA              
  N = c(132,131),
  CDAI_baseline = c(312.4,338), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(17.4,43.5),
  Remission = c(10.6,18.3), 
  Endpoint = c(NA,NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(39.5,39.4), Sex_Male = c(48.5,36.6), Race_White = c(NA,NA), Ethnicity_Hispanic = c(NA,NA), `Weight (kg)` = c(NA,NA), 
  `Height (cm)` = c(NA,NA), BMI = c(NA,NA), Smoking = c(NA,NA), HxOfTNFi = c(99.2,99.3), SteroidUse = c(55.3,45), ImmUse = c(22.7,26.7), 
  `CRP (mg/L)` = c(NA,NA), `Albumin (g/L)` = c(NA,NA), `Duration (yrs)` = c(12.4,12.7), Ileal = c(NA,NA), Perianal = c(NA,NA), 
  CurrentFistula = c(NA,NA),CurrentOrPriorStricture = c(NA,NA))

# ENACT
enactGT <- tibble(
  DrugClass = rep("Integrin",2), ActiveIngredient = rep("NTZ",2), Trial = rep("ENACT",2), Group = c("Placebo", "Active"),
        
  # UPDATE TO WEEK 8 DATA          
  N = c(181,724),
  CDAI_baseline = c(303,302), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(50,57), # NA for week 6
  Remission = c(28,34), # NA for week 6
  Endpoint = c(NA,NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(39,38), Sex_Male = c(40,43), Race_White = c(NA,NA), Ethnicity_Hispanic = c(NA,NA), `Weight (kg)` = c(NA,NA), 
  `Height (cm)` = c(NA,NA), BMI = c(NA,NA), Smoking = c(24,23), HxOfTNFi = c(38,40), SteroidUse = c(39,37), ImmUse = c(29,34), 
  `CRP (mg/L)` = c(23,20), `Albumin (g/L)` = c(NA,NA), `Duration (yrs)` = c(9.2,10.1), Ileal = c(72,79), Perianal = c(NA,NA), 
  CurrentFistula = c(NA,NA), CurrentOrPriorStricture = c(NA,NA)
)

# ENCORE
encoreGT <- tibble(
  DrugClass = rep("Integrin",2), ActiveIngredient = rep("NTZ",2), Trial = rep("ENCORE",2), Group = c("Placebo", "Active"),
        
  # UPDATE TO WEEK 8 DATA          
  N = c(250,259),
  CDAI_baseline = c(299.5,303.9), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(40,56), 
  Remission = c(21,32),
  Endpoint = c(NA,NA), 
  
  # BASELINE VALUES - KEEP SAME
  # BASELINE VALUES - KEEP SAME
  Age = c(37.7,38.1), Sex_Male = c(41,41), Race_White = c(94,95), Ethnicity_Hispanic = c(0,0), `Weight (kg)` = c(74.4,71.9), 
  `Height (cm)` = c(170.4,170.1), BMI = c(25.7,24.8), Smoking = c(19,22), HxOfTNFi = c(45,50), SteroidUse = c(38,42), ImmUse = c(38,37), 
  `CRP (mg/L)` = c(23.4,23.0), `Albumin (g/L)` = c(36.8,36.7), `Duration (yrs)` = c(10.0,10.1), Ileal = c(74,74), Perianal = c(NA,NA), 
  CurrentFistula = c(NA,NA), CurrentOrPriorStricture = c(NA,NA)
  )

# PRECISE1
precise1GT <- tibble(
  DrugClass = rep("TNFi",2), ActiveIngredient = rep("CZP",2), Trial = rep("PRECISE1",2), Group = c("Placebo", "Active"),
       
  # UPDATE TO WEEK 8 DATA              
  N = c(329,331),
  CDAI_baseline = c(297,300), 
  CDAI = c(NA,NA), 
  CDAI_reduction = c(NA,NA), 
  Response = c(29,35),
  Remission = c(19,24), 
  Endpoint = c(NA,NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(38,37), Sex_Male = c(40,47), Race_White = c(NA,NA), Ethnicity_Hispanic = c(NA,NA), `Weight (kg)` = c(NA,NA), 
  `Height (cm)` = c(NA,NA), BMI = c(24,24), Smoking = c(33,31), HxOfTNFi = c(NA,NA), SteroidUse = c(40,39), ImmUse = c(37,38), 
  `CRP (mg/L)` = c(NA,NA), `Albumin (g/L)` = c(NA,NA), `Duration (yrs)` = c(8,7), Ileal = c(NA,NA), Perianal = c(NA,NA), 
  CurrentFistula = c(NA,NA), CurrentOrPriorStricture = c(NA,NA)
  )

# 9783 
nct02499783GT <- tibble(
  DrugClass = rep("TNFi",1), ActiveIngredient = rep("ADA",1), Trial = rep("NCT02499783",1), Group = c("Active"),
                        
  # UPDATE TO WEEK 8 DATA
  N = c(102),
  CDAI_baseline = c(272.1), 
  CDAI = c(NA), 
  CDAI_reduction = c(NA), 
  Response = c(70), # NA for week 6
  Remission = c(45.4), # NA for week 6
  Endpoint = c(NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(33.2), Sex_Male = c(66), Race_White = c(0), Ethnicity_Hispanic = c(0), `Weight (kg)` = c(53.3), 
  `Height (cm)` = c(NA), BMI = c(NA), Smoking = c(NA), HxOfTNFi = c(0), SteroidUse = c(30), ImmUse = c(60), 
  `CRP (mg/L)` = c(23.9), `Albumin (g/L)` = c(NA), `Duration (yrs)` = c(3.1), Ileal = c(80), Perianal = c(NA), 
  CurrentFistula = c(NA), CurrentOrPriorStricture = c(NA)
  )

# CLASSIC
classicGT <- tibble(
  DrugClass = rep("TNFi",1), ActiveIngredient = rep("ADA",1), Trial = rep("CLASSIC",1), Group = c("Active"),
     
  # UPDATE TO WEEK 8 DATA               
  N = c(76), 
  CDAI_baseline = c(295), 
  CDAI = c(NA), 
  CDAI_reduction = c(NA), 
  Response = c(NA), 
  Remission = c(36),
  Endpoint = c(NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(39), Sex_Male = c(47), Race_White = c(NA), Ethnicity_Hispanic = c(NA), `Weight (kg)` = c(NA), 
  `Height (cm)` = c(NA), BMI = c(NA), Smoking = c(42), HxOfTNFi = c(0), SteroidUse = c(32), ImmUse = c(29), 
  `CRP (mg/L)` = c(14), `Albumin (g/L)` = c(NA), `Duration (yrs)` = c(NA), Ileal = c(64), Perianal = c(NA), 
  CurrentFistula = c(16), CurrentOrPriorStricture = c(NA))

# EXTEND
extendGT <- tibble(
  DrugClass = rep("TNFi",1), ActiveIngredient = rep("ADA",1), Trial = rep("EXTEND",1), Group = c("Active"),
      
  # UPDATE TO WEEK 8 DATA             
  N = c(64), 
  CDAI_baseline = c(318.7), 
  CDAI = c(NA), 
  CDAI_reduction = c(NA), 
  Response = c(NA), 
  Remission = c(47), 
  Endpoint = c(NA), 
  
  # BASELINE VALUES - KEEP SAME
  Age = c(37.1), Sex_Male = c(37.5), Race_White = c(92.2), Ethnicity_Hispanic = c(NA), `Weight (kg)` = c(NA), 
  `Height (cm)` = c(NA), BMI = c(NA), Smoking = c(29.7), HxOfTNFi = c(46.9), SteroidUse = c(14.1), ImmUse = c(43.8), 
  `CRP (mg/L)` = c(NA), `Albumin (g/L)` = c(NA), `Duration (yrs)` = c(10.4), Ileal = c(NA), Perianal = c(NA), 
  CurrentFistula = c(NA),CurrentOrPriorStricture = c(NA))

#####################################################################################################

# Concatenate Published Results
# Published Summary By Study
groundTruth <- uniti1GT %>%
  bind_rows(uniti2GT, certifiGT, enactGT, encoreGT, precise1GT, nct02499783GT, classicGT, extendGT) %>%
  arrange(DrugClass, ActiveIngredient, Trial, Group)

#####################################################################################################

## IPD results 

crohnsData <- read.csv(file='G:/Shan/Week 8 Identification/CombinedTrials/crohnsData_wk8.csv')

crohnsData %>% 
  # find those who don't have any cdai measurements btw weeks 1-8
  mutate(WK1_WK8_NA = if_else(is.na(CDAI_WEEK1) & 
                              is.na(CDAI_WEEK2) & 
                              is.na(CDAI_WEEK3) & 
                              is.na(CDAI_WEEK4) &
                              is.na(CDAI_WEEK6) &
                              is.na(CDAI_WEEK8), NA, TRUE)) %>% 
  select(TRIAL, CDAI_BASELINE, WK1_WK8_NA, BMI, CRP..mgL, IMMUNOMOD, STEROID, HxOfTNFi, LOC.ILEAL, 
         Sex_Male, Race_White, Ethnicity_Hispanic,
         LOC.COLON, SMOKING, PERIANAL, CURR.FISTULA, CURR.PRIOR.STRICTURE, CDAI_WEEK8) %>% 
  group_by(TRIAL) %>% 
  summarise(across(everything(), ~sum(is.na(.))))

crohnsData %>% 
  select(TRIAL, TRTGRP, IMMUNOMOD, STEROID, HxOfTNFi, LOC.ILEAL) %>% 
  group_by(TRIAL, TRTGRP) %>% 
  summarize_all(mean)

#####################################################################################################

# IPD Summary By Study
crohnsDataSubset2 <- crohnsData %>% 
  # create drug class
  mutate(DrugClass = if_else(DRUG=='UST','Il12', if_else(DRUG=='NTZ','Integrin', 'TNFi')), .after=TRIAL) %>%
  # rename covariates
  rename(Trial            = TRIAL) %>%
  rename(ActiveIngredient = DRUG) %>% 
  rename(Group            = TRTGRP) %>% 
  rename(Age              = AGE) %>% 
  rename(`CRP (mg/L)`     = CRP..mgL) %>%  
  rename(SteroidUse       = STEROID) %>%  
  rename(ImmUse           = IMMUNOMOD) %>%  
  rename(Ileal            = LOC.ILEAL) %>%  
  rename(CDAI_baseline    = CDAI_BASELINE) %>%  
  rename(CDAI             = CDAI_WEEK8) %>%  
  rename(CDAI_reduction   = CDAI_REDUCTION) %>% 
  # additional variables
  rename(`Weight (kg)` = WEIGHT..kg, `Height (cm)` = HEIGHT..cm, `Albumin (g/L)` = ALBUMIN..gL, `Duration (yrs)` = DURATION, 
         Smoking = SMOKING, Perianal = PERIANAL, CurrentFistula = CURR.FISTULA, CurrentOrPriorStricture = CURR.PRIOR.STRICTURE) %>% 
  # create outcome variables
  mutate(Remission        = if_else(CDAI    <= 150,    1, 0)) %>%
  mutate(Response         = if_else(CDAI_reduction >= 100,    1, 0)) %>%
  mutate(Endpoint         = if_else(Response==1|Remission==1, 1, 0)) %>% 
  # remove unnecessary columns
  select(-c(CDAI_WEEK1:CDAI_WEEK6, SEX)) %>% 
  # summarise
  group_by(DrugClass, ActiveIngredient, Trial, Group) %>%
  summarise(N = n(), 
            CDAI_baseline = mean(CDAI_baseline, na.rm = T), 
            CDAI = mean(CDAI, na.rm = T), 
            CDAI_reduction = mean(CDAI_reduction, na.rm = T), 
            Response = 100*sum(Response, na.rm = T)/n(), 
            Remission = 100*sum(Remission, na.rm = T)/n(),
            Endpoint = 100*sum(Endpoint, na.rm = T)/n(), 
            Age = mean(Age, na.rm = T), 
            Sex_Male = 100*sum(Sex_Male, na.rm = T)/n(), 
            Race_White = 100*sum(Race_White, na.rm = T)/n(),
            Ethnicity_Hispanic = 100*sum(Ethnicity_Hispanic, na.rm = T)/n(),
            `Weight (kg)` = mean(`Weight (kg)`, na.rm = T),
            `Height (cm)` = mean(`Height (cm)`, na.rm = T),
            BMI = mean(BMI, na.rm = T), 
            Smoking = 100*sum(Smoking, na.rm = T)/n(),
            HxOfTNFi = 100*sum(HxOfTNFi, na.rm = T)/n(), 
            SteroidUse = 100*sum(SteroidUse, na.rm = T)/n(), 
            ImmUse = 100*sum(ImmUse, na.rm = T)/n(), 
            `CRP (mg/L)` = mean(`CRP (mg/L)`, na.rm = T),
            `Albumin (g/L)` = mean(`Albumin (g/L)`, na.rm = T),
            `Duration (yrs)` = mean(`Duration (yrs)`, na.rm = T),
            Ileal = 100*sum(Ileal, na.rm = T)/n(), 
            Perianal = 100*sum(Perianal, na.rm = T)/n(),
            CurrentFistula = 100*sum(CurrentFistula, na.rm = T)/n(),
            CurrentOrPriorStricture = 100*sum(CurrentOrPriorStricture, na.rm = T)/n()
            ) %>%
  ungroup() %>%
  arrange(DrugClass, ActiveIngredient, Trial, Group) 

#####################################################################################################

## Compare Published vs IPD

calcDifference <- abs(round(100*(crohnsDataSubset2[,5:ncol(crohnsDataSubset2)] - groundTruth[,5:ncol(groundTruth)]) / 
                               ((crohnsDataSubset2[,5:ncol(crohnsDataSubset2)] + groundTruth[,5:ncol(groundTruth)]) / 2), 2))
calcDifference <- crohnsDataSubset2[,1:4] %>% bind_cols(calcDifference)
calcDifference <- calcDifference %>%
  mutate(TrialN = paste(DrugClass, ActiveIngredient, Trial, Group, sep = "-")) %>%
  dplyr::select(-c(DrugClass, ActiveIngredient, Trial, Group)) %>%
  pivot_longer(!TrialN, names_to = "feature", values_to = "abs_%_diff")

# Write for export
# write_csv(x = calcDifference, file = "E:/Results/Doug/Week 6 identification/Export/Tables/DifferenceVersusPublishedSummaryByStudy.csv")

#####################################################################################################

# Set a max value to help with plotting
calcDifference <- calcDifference %>%
  mutate(`abs_%_diff` = ifelse(`abs_%_diff` > 20, 20, `abs_%_diff`))

# Heatmap
calcDifference %>%
  mutate(feature = factor(feature, levels = colnames(groundTruth)[5:ncol(groundTruth)])) %>%
  filter(!feature %in% c('CDAI','CDAI_reduction',"Ethnicity_Hispanic", "Height (cm)", "Albumin (g/L)", "Perianal")) %>%
  ggplot(aes(x = feature, y = TrialN, fill = `abs_%_diff`)) +
    geom_tile() +
    scale_fill_viridis() +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

# Save the heatmap plot
# ggsave(filename = "E:/Results/Doug/Week 6 identification/Export/Figures/DifferenceVersusPublishedSummaryByStudy.pdf", plot = p2, height = 5, width = 7)

#####################################################################################################


# ScatterPlot for each variable
scatterData1 <- crohnsDataSubset2 %>%
  mutate(TrialN = paste(DrugClass, ActiveIngredient, Trial, Group, sep = "-")) %>%
  dplyr::select(-c(DrugClass, ActiveIngredient, Trial, Group)) %>%
  pivot_longer(!TrialN, names_to = "feature", values_to = "Vivli")

scatterData2 <- groundTruth %>%
  mutate(TrialN = paste(DrugClass, ActiveIngredient, Trial, Group, sep = "-")) %>%
  dplyr::select(-c(DrugClass, ActiveIngredient, Trial, Group)) %>%
  pivot_longer(!TrialN, names_to = "feature", values_to = "Published")

scatterData <- scatterData1 %>%
  inner_join(scatterData2, by = c("TrialN" = "TrialN", "feature" = "feature"))

scatterData %>%
  mutate(feature = factor(feature, levels = colnames(groundTruth)[5:ncol(groundTruth)])) %>%
  filter(!feature %in% c("Ethnicity_Hispanic", "Height (cm)", "Albumin (g/L)", "Perianal")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1)

ScatterPlotData <- scatterData %>%
  mutate(feature = factor(feature, levels = colnames(groundTruth)[5:ncol(groundTruth)])) %>%
  filter(!feature %in% c("Ethnicity_Hispanic", "Height (cm)", "Albumin (g/L)", "Perianal"))

# Write for export
# write_csv(x = ScatterPlotData, file = "E:/Results/Doug/Week 6 identification/Export/Tables/DifferenceVersusPublishedSummaryByStudyScatterPlot.csv")

# print.data.frame(scatterData %>% filter(feature=='Response'))
# print.data.frame(scatterData %>% filter(feature=='Remission'))
# print.data.frame(scatterData %>% filter(feature=='ImmUse'))

pYaxis <- ggplot(data.frame(l = "Vivli", x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) +
  theme_void() +
  coord_cartesian(clip = "off")
pXaxis <- ggplot(data.frame(l = "Published", x = 1, y = 1)) +
  geom_text(aes(x, y, label = l)) +
  theme_void() +
  coord_cartesian(clip = "off")

p1 <- ScatterPlotData %>%
  filter(feature == "N") %>%
  ggplot(aes(x = Published, y = Vivli)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
    geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
    geom_point() +
    facet_wrap(~feature) +
    xlim(c(0,1000)) +
    ylim(c(0,1000)) +
    ylab("") +
    xlab("") +
    theme_bw() +
    theme(aspect.ratio = 1)
    
p2 <- ScatterPlotData %>%
  filter(feature == "CDAI_baseline") %>%
  mutate(feature = recode(feature, "CDAI_baseline" = "Basline CDAI")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(270,340)) +
  ylim(c(270,340)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p3 <- ScatterPlotData %>%
#   filter(feature == "CDAI") %>%
#   mutate(feature = recode(feature, "CDAI" = "Week 8 CDAI")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(190,225)) +
#   ylim(c(190,225)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

# p4 <- ScatterPlotData %>%
#   filter(feature == "CDAI_reduction") %>%
#   mutate(feature = recode(feature, "CDAI_reduction" = "CDAI Reduction")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(70,105)) +
#   ylim(c(70,105)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

p5 <- ScatterPlotData %>%
  filter(feature == "Response") %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(20,75)) +
  ylim(c(20,75)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

p6 <- ScatterPlotData %>%
  filter(feature == "Remission") %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(5,60)) +
  ylim(c(5,60)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p7 <- ScatterPlotData %>%
#   filter(feature == "Endpoint") %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(20,60)) +
#   ylim(c(20,60)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

p8 <- ScatterPlotData %>%
  filter(feature == "Age") %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(32,41)) +
  ylim(c(32,41)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

p9 <- ScatterPlotData %>%
  filter(feature == "Sex_Male") %>%
  mutate(feature = recode(feature, "Sex_Male" = "Sex: Male")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(35,75)) +
  ylim(c(35,75)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p10 <- ScatterPlotData %>%
#   filter(feature == "Race_White") %>%
#   mutate(feature = recode(feature, "Race_White" = "Race: White")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(-1,100)) +
#   ylim(c(-1,100)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

# p11 <- ScatterPlotData %>%
#   filter(feature == "Weight (kg)") %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(52.5,75)) +
#   ylim(c(52.5,75)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

p12 <- ScatterPlotData %>%
  filter(feature == "BMI") %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(22.5,26.5)) +
  ylim(c(22.5,26.5)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p13 <- ScatterPlotData %>%
#   filter(feature == "Smoking") %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(20,45)) +
#   ylim(c(20,45)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

p14 <- ScatterPlotData %>%
  filter(feature == "HxOfTNFi") %>%
  mutate(feature = recode(feature, "HxOfTNFi" = "History of TNFi")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(-1,101)) +
  ylim(c(-1,101)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

p15 <- ScatterPlotData %>%
  filter(feature == "SteroidUse") %>%
  mutate(feature = recode(feature, "SteroidUse" = "Basline Steroids")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(10,75)) +
  ylim(c(10,75)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

p16 <- ScatterPlotData %>%
  filter(feature == "ImmUse") %>%
  mutate(feature = recode(feature, "ImmUse" = "Basline Immunomodulators")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(-1,70)) +
  ylim(c(-1,70)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p17 <- ScatterPlotData %>%
#   filter(feature == "CRP (mg/L)") %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(13,35)) +
#   ylim(c(13,35)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

# p18 <- ScatterPlotData %>%
#   filter(feature == "Duration (yrs)") %>%
#   mutate(feature = recode(feature, "Duration (yrs)" = "Disease Duration (yrs)")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(1,14)) +
#   ylim(c(1,14)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

p19 <- ScatterPlotData %>%
  filter(feature == "Ileal") %>%
  mutate(feature = recode(feature, "Ileal" = "Disease Location: Ileal")) %>%
  ggplot(aes(x = Published, y = Vivli)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
  geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
  geom_point() +
  facet_wrap(~feature) +
  xlim(c(65,90)) +
  ylim(c(65,90)) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1)

# p20 <- ScatterPlotData %>%
#   filter(feature == "CurrentFistula") %>%
#   mutate(feature = recode(feature, "CurrentFistula" = "Baseline Fistula")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(-1,20)) +
#   ylim(c(-1,20)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

# p21 <- ScatterPlotData %>%
#   filter(feature == "CurrentOrPriorStricture") %>%
#   mutate(feature = recode(feature, "CurrentOrPriorStricture" = "Baseline or Past Stricture")) %>%
#   ggplot(aes(x = Published, y = Vivli)) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_abline(intercept = 0, slope = 1.1, color = "grey50") +
#   geom_abline(intercept = 0, slope = 0.9, color = "grey50") +
#   geom_point() +
#   facet_wrap(~feature) +
#   xlim(c(14,36)) +
#   ylim(c(14,36)) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(aspect.ratio = 1)

# (pYaxis + ( (p1 | p2 | p3 | p4 | p5) / (p6 | p7 | p8 | p9 | p10) / (p11 | p12 | p13 | p14 | p15) / (p16 | p17 | p18 | p19 | p20) / (p21) ) + 
#     plot_layout(widths = c(1,25))) / pXaxis + plot_layout(heights = c(25,1))

layout <- "
ACCCCDDDDEEEEFFFF
ACCCCDDDDEEEEFFFF
ACCCCDDDDEEEEFFFF
ACCCCDDDDEEEEFFFF
AGGGGHHHHIIIIJJJJ
AGGGGHHHHIIIIJJJJ
AGGGGHHHHIIIIJJJJ
AGGGGHHHHIIIIJJJJ
AKKKKLLLLMMMM####
AKKKKLLLLMMMM####
AKKKKLLLLMMMM####
AKKKKLLLLMMMM####
#BBBBBBBBBBBBBBBB
"
# pFinal <- pYaxis + pXaxis + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + 
#   p13 + p14 + p15 + p16 + p17 + p18 + p19 + p20 + p21 + plot_layout(design = layout)

pFinal <- pYaxis + pXaxis + p1 + p2 + p5 + p6 + p8 + p9 + p12 +
  p14 + p15 + p16 + p19 + plot_layout(design = layout) & theme(plot.tag = element_text(size=20))
pFinal

ggsave(filename = "G:/Shan/Week 8 Identification/Figures/DifferenceVersusPublishedSummaryByStudyScatterPlot.pdf", plot = pFinal, height = 9, width = 15)
ggsave(filename = "G:/Shan/Week 8 Identification/Figures/DifferenceVersusPublishedSummaryByStudyScatterPlot.png", plot = pFinal, height = 9, width = 9)

#####################################################################################################
