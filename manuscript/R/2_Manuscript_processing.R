# ---------------------------
# Script name: 2. Manuscript - Data processing
# Description: Script for data processing (data checks etc)
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Setup ----------------------------------
source(here::here("manuscript/R/1_Manuscript_setup.R"))
here::i_am("manuscript/R/2_Manuscript_processing.R") #Set location of script

# Read in data ----
data_evening <- read.csv(here::here("data_evening.csv"))

# Rescale VAS scales to 0-10 ---------------------
divide_by_10 <- function(x) {
  return(x / 10)
}

scale_vars <- c("n.em.int", "n.em.cont", "n.er.rel", "n.er.eng", "n.er.rum", "n.er.reap", "n.er.dis", "n.er.sup",
                "p.em.int", "p.er.eng", "p.er.sav")

data_evening[scale_vars] <- apply(data_evening[scale_vars], 2, divide_by_10)

psych::describe(data_evening)

# N participants dropped out ----------------------------------------------
n_drop <- data_evening %>%
  filter(Dropped.out == 1) %>%
  nsub(participant.ID, .)

# Identify careless responding ----------------------------------------------

## Function count number of participants
n_par <- function(x) {
  nsub(participant.ID, x) }

## N participants & observations before exclusion ----
n_par_0 <- n_par(data_evening)
n_obs_0 <- sum(data_evening$filledin == 1)

## Step 1: Negative time between sending & opening questionnaire ------
data_evening <- data_evening %>%
  dplyr::filter(Appmal_negti=='no issue' | 
                  Appmal_negti=='negative time (due to clock change of daylight saving)' |
                  is.na(Appmal_negti)) 

n_obs_1 <- sum(data_evening$filledin == 1)

## Step 2: Questionnaire was answered after it expired ------
data_evening <- data_evening %>%
  dplyr::filter(Appmal_longexp=='no'|
                  is.na(Appmal_longexp)) 

n_obs_2 <- sum(data_evening$filledin == 1)

## Step 3: Total completion < 1 minute ------
data_evening <- data_evening %>%
  dplyr::filter(duration_sec_start_stop >=60 | is.na(duration_sec_start_stop)) 

n_par_3 <- n_par(data_evening)
n_obs_3 <- sum(data_evening$filledin == 1)

## Step 4: Exclude participants with zero variance  ------
data_evening <- check.timeinvar(n.em.int, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.em.cont, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.rel, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.eng, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.rum, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.reap, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.dis, participant.ID, data_evening, out = 3)
data_evening <- check.timeinvar(n.er.sup, participant.ID, data_evening, out = 3)

n_par_4 <- n_par(data_evening)
n_obs_4 <- sum(data_evening$filledin == 1)


## N participants & observations after exclusion ----
n_obs_st1<-n_obs_0-n_obs_1 #Observations excluded after step 1
n_obs_st2<-n_obs_1-n_obs_2 #Observations excluded after step 2
n_obs_st3<-n_obs_2-n_obs_3 #Observations excluded after step 3
n_obs_st4<-n_obs_3-n_obs_4 #Observations excluded after step 4
n_par_st4<-n_par_3-n_par_4 #Participants excluded after step 4

per_par_ex<-100-(100/n_par_0)*n_par_4 #Percentage of participants excluded overall
n_obs_ex<-n_obs_0-n_obs_4             #Number of observations excluded overall
per_obs_ex<-100-(100/n_obs_0)*n_obs_4 #Percentage of observations excluded overall

# Make person-level dataset ----
# (for later calculation of descriptives of time-invariant variables)
## Recalculate compliance evening ----
# During first data cleaning, I had a variable indicating individual compliance, but I had to adjust that after excluding observations

data_evening <- data_evening %>% dplyr::rename(comp.evening.old = comp.evening) # rename "old" compliance variable before exclusion

data_evening$comp.evening <- calc.nomiss(n.em.int, participant.ID, data=data_evening, expand=TRUE) #calculate new compliance variable

data_evening$comp.evening.per<-(100/data_evening$sent.beeps.evening)*data_evening$comp.evening #calculate compliance percentage

## Make dataset
data_person <- data_evening %>% 
  select(c("participant.ID","gender.dum","Age_B","Student_B",
           "sent.beeps.evening","Dropped.out","comp.evening",
           "comp.evening.per")) %>% 
  mutate_at(vars(gender.dum,Student_B,Dropped.out),as.numeric)

data_person<-data_person %>% 
  group_by(participant.ID) %>% 
  summarise_all(mean)

# Data checks ---
# check min and max of study variables
psych::describe(data_evening)

#check non-missing in design data
check.nomiss(participant.ID, participant.ID, data=data_evening)
check.nomiss(participant.ID, day, data=data_evening)
check.nomiss(participant.ID, week, data=data_evening)
check.nomiss(participant.ID, obs.evening, data=data_evening)

# check that age, sex, are really time-invariant within subjects
# (and that they are either completely missing or complete within subjects)
check.timeinvar(Age_B, participant.ID, data=data_evening, na.rm=FALSE)
check.timeinvar(gender.dum, participant.ID, data=data_evening, na.rm=FALSE)

# number of subjects and number of rows of data
nsub(data_evening$participant.ID)
nrow(data_evening)

# check that there are no duplicated day-beep combinations within each subject
check.nodup(interaction(day, obs.evening), participant.ID, data=data_evening)
# check that there are no duplicated obs values within each subject
check.nodup(obs.evening, participant.ID, data=data_evening)

# Bray-Curtis dissimilarity for ER Variability -------
# Load code for Bray-Curtis dissimilarity. Code was downloaded from https://github.com/taktsun/dissimilarity-for-ESM-data (file: BrayCurtisDissimilarity_Calculate.R)
source(here::here("manuscript/R/BrayCurtisDissimilarity_Calculate.R"))

# sort by participant and beepnumber
data_evening <- data_evening  %>%
  arrange(participant.ID, obs.evening)

# calculate 
data_evening <- calcBrayCurtisESM(d = data_evening, # dataframe
                                  vn =  c("n.er.rel","n.er.eng","n.er.rum","n.er.reap","n.er.dis","n.er.sup"), # list of variables
                                  pid = "participant.ID", # participant identifier
                                  tid = "obs.evening") # trigger/beep identifier

# Rescale dBC, mulitply by 10, so that it is on the same scale as the ER strategies
multiply_with_10 <- function(x) {
  return(x * 10)
}

scale_vars_dbc <- c("BrayCurtisFull.amm", "BrayCurtisRepl.amm", "BrayCurtisNest.amm")

data_evening[scale_vars_dbc] <- apply(data_evening[scale_vars_dbc], 2, multiply_with_10)

# Center variables ----------------------------------------------

## Age (grandmean) --------------
Age_m <- mean(data_person$Age_B, na.rm = TRUE) #mean

# Replace missing age with mean age (grand-mean)
data_evening<-data_evening  %>% 
  dplyr::mutate(Age_B =
                  ifelse(is.na(Age_B), Age_m, Age_B))

# Grand-mean center age
data_evening <- data_evening %>% 
  dplyr::mutate(Age.c = Age_B-Age_m)

## Time-varying predictors (person-mean) ----------------------

# emotion intensity and control
predictors <- c("n.em.int", "n.em.cont")
data_evening[, paste0(predictors, ".c")] <- sapply(predictors, function(var) calc.mcent(get(var), participant.ID, data = data_evening))

# check dataset
psych::describe(data_evening)

# Save processed files and outputs ----

# Sample flow (participants included/excluded)
sample_flow <- list(
  n_par_0 = n_par_0,
  n_obs_0 = n_obs_0,
  n_obs_1 = n_obs_1,
  n_obs_2 = n_obs_2,
  n_obs_3 = n_obs_3,
  n_obs_4 = n_obs_4,
  n_par_3 = n_par_3,
  n_par_4 = n_par_4,
  n_obs_st1 = n_obs_st1,
  n_obs_st2 = n_obs_st2,
  n_obs_st3 = n_obs_st3,
  n_obs_st4 = n_obs_st4,
  n_par_st4 = n_par_st4,
  n_obs_ex = n_obs_ex,
  per_obs_ex = per_obs_ex,
  per_par_ex = per_par_ex,
  n_drop = n_drop
)

saveRDS(sample_flow, here::here("manuscript/R/output/sample_flow.rds"))
saveRDS(data_person,  here::here("manuscript/R/output/data_person_clean.rds"))
saveRDS(data_evening, here::here("manuscript/R/output/data_evening_clean.rds"))

