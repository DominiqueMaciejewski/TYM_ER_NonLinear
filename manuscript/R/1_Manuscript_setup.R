# ---------------------------
# Script name: 1. Manuscript - Setup
# Description: Script for setup (libraries, options) 
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

### Library ----
# Load library 
library(papaja)
library(worcs)
library(here)
library(dplyr)
library(tidyr)
library(plyr)
library(esmpack)
library(betapart) 
library(psych)
library(misty)
library(nlme)
library(lmerTest)
library(sjPlot)
library(lme4)
library(performance)
library(flextable)
library(rempsyc)
library(Hmisc)
library(ggpubr)
library(stats)
library(reghelper)
library(reshape2)
library(emmeans)

sessionInfo()


### Options ----
# Seed for random number generation
set.seed(42)

here::i_am("manuscript/R/1_Manuscript_setup.R") #Set location of script

# Prevent scientific notation
options(scipen = 999)

# List of variables for later analyses
ESM_variables <- c("n.em.int", "n.em.cont", 
                   "n.er.rel", "n.er.eng", "n.er.rum",
                   "n.er.reap", "n.er.dis", "n.er.sup")

ESM_names <- c('Emotion intensity', 'Emotion controllability', 
               'Relaxation', 'Expression', 'Rumination',
               'Reappraisal', 'Distraction', 'Suppression')

