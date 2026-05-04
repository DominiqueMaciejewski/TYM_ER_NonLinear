# ---------------------------
# Script name:  Power analysis - Data Preparation
#
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2023-03-24
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#


# Options ####

options(scipen = 999) 
here::i_am("preregistration/Power-analyses_prepare_data.R") #Set location of script

# Library ####

library(worcs)
library(here)
library(readxl)
library(dplyr)
library(labelled)

# Data preparation ###

# read in codebook with variable names
var_names <- read_excel(here::here("raw data", "RESSEMA_data_CODEBOOK.xlsx"))

# Read in data in dat. format
ressema_df <- read.delim(here::here("raw data", "RESSEMA_data.dat"),
                         header=FALSE,
                         col.names = var_names$`VARIABLE NAME`)

ressema_df<-set_variable_labels(ressema_df,         
                    .labels = var_names$EXPLANATION)

# Select needed variables 
# For ER strategies, we only chose the items that we assessed in TyM
ressema_df <- ressema_df %>%
  dplyr::select(c("sema_id", "int", "ctrl",
                  "relax2", "exp1","rumi2",
                  "reap1", "dist2", "sup1",
                  "PSS", "NA."))

# Recode -999 to NA
ressema_df[ressema_df == -999] <- NA

# grouping variable needs to be a factor
ressema_df[["sema_id"]] <- as.factor(ressema_df[["sema_id"]])

# Save ####
open_data(ressema_df)

          