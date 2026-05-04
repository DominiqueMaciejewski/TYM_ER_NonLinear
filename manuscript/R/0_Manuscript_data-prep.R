# ---------------------------
# Script name: 0. Manuscript - Data Preparation
#
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2023-07-31
# adjusted 2025-10-14 (making new random ID number for sharing)
# adjusted 2026-04-10 (recoded 1 non-binary participant together with males into other for sharing,
#                      take out previous synthetic dataset that we shared on Github. Now we share the 
#                      whole dataset via the Radboud Repository)
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Library ----------------------------------------------
library(worcs)
library(here)
library(dplyr)
library(esmpack)

# Options ----------------------------------------------

options(scipen = 999) 
here::i_am("manuscript/R/0_Manuscript_data-prep.R") #Set location of script

# Load in file ----------------------------------------------
# Note that the actual raw data from the Track your mood study is saved on the
# Radboud University server. Here, the data were cleaned. 
# Location of raw data: \\ru.nl\WrkGrp\STD-FSW-BSI-DP-Track_Your_Mood\2_Data\Raw\mPath_data_basic.csv
# Location of data cleaning script: \\ru.nl\WrkGrp\STD-FSW-BSI-DP-Track_Your_Mood\2_Data\Support\Code_Data-cleaning\TyM_Datacleaning_RMarkdown.Rmd

# The data cleaning script can also be found on OSF (https://osf.io/pe2xc)

load(here::here("raw data","Data_2023-02-10_Maciejewski-Dominique.RData"))

# Data cleaning ----------------------------------------------

## Calculate variable micro-intervention -------
# 0 = no micro-intervention received that day 
# 1 = yes, micro-intervention received that day
# Note: micro-interventions never happened at evening assessment,
# but we still control for it in analyses

data_all<-data_all %>% 
  dplyr::group_by(participant.ID,day) %>% 
  dplyr::mutate(micro=ifelse(sum(int.ass)==0,0,1))

## Select only evening questionnaire & relevant vars ---------------------------

data_evening <- data_all %>% 
  dplyr::filter(questionListName=="Evening questionnaire")

data_evening <- data_evening %>%
  dplyr::select(c(1:5,9:10,101,12,23:28,30,35,42,65:73,83:86,173:175,499))

## change ID number ---------------------------------
data_evening <- 
  data_evening %>%
  group_by(participant.ID) %>%
  mutate(.group = cur_group_id()) %>%
  ungroup() %>%
  mutate(participant.ID.new = sample(10000:99999, n_distinct(.group))[.group]) %>%
  select(-.group) 

# save coupling between study ID and newly generated ID as file
coupling_IDs <- 
  data_evening %>%
  select(participant.ID,participant.ID.new)

write.csv(coupling_IDs,"coupling_IDs.csv")

# remove original ID from dataset and rename participant ID
data_evening <- 
  data_evening %>%
  dplyr::select(-participant.ID) %>%
  dplyr::rename(participant.ID=participant.ID.new) %>%
  dplyr::select(participant.ID,everything())

## change variable format ---------------------------

data_evening <- data_evening %>%
  dplyr::mutate_at(vars(Dropped.out, Toest_aspect1,
                 Toest_aspect2, Toest_aspect3,
                 Appmal_noeven, Appmal_negti, Appmal_longexp,
                 p.em.ev, n.em.ev),
            as.factor) 

data_evening$participant.ID<-as.numeric(data_evening$participant.ID)

data_evening <- data_evening %>%
  dplyr::mutate_at(vars(participant.ID, max.week,
                        duration_sec_start_stop, sent.beeps.evening,
                        sent.beeps.evening, filledin, week,
                        comp.evening, Age_B, micro),
                   as.integer) 

# Change gender since there is one non-binary person in there
# For the analyses, we only use female vs other anyway.
#Info from final sample
# > gender_f <- (100/n_par_4)*(sum(data_person$Gender_B == 1, na.rm = TRUE)) #Percentage women
# > gender_m <- (100/n_par_4)*(sum(data_person$Gender_B == 2, na.rm = TRUE)) #Percentage men
# > gender_b <- (100/n_par_4)*(sum(data_person$Gender_B == 3, na.rm = TRUE)) #Percentage non-binary
# > gender_NA <- (100/n_par_4)*(sum(is.na(data_person$Gender_B))) #Percentage NA
# > gender_f
# [1] 83.5443
# > gender_m
# [1] 11.39241
# > gender_b
# [1] 1.265823
# > gender_NA
# [1] 3.797468

# Replace missing gender with mode gender (female) &
# dummy-code (female vs other) 
data_evening<-data_evening  %>% 
  dplyr::mutate(gender.dum =
                  ifelse(Gender_B=="female"|is.na(Gender_B), 1, 0)) %>% 
  dplyr::select(-Gender_B) %>%
  dplyr::select(participant.ID:p.er.sav,gender.dum,Age_B:micro)

data_evening$gender.dum <- as.factor(data_evening$gender.dum)

# Make student variable numeric (Note, in the original datacleaning, there was a coding mistake
# There were way more students than non-students, so Yes should have been No)
data_evening <- data_evening %>%
  mutate(
    Student_B = case_when(
      Student_B == "Yes" ~ 0, # 0 will be student (coding mistake during initial datacleaning)
      Student_B == "No" ~ 1, # 1 will be no student (coding mistake during initial datacleaning)
      TRUE ~ NA_real_
    )
  )

## Save as closed data ---------------------------

# On Github, we specify how people can request the data
closed_data(data_evening, synthetic = FALSE)


