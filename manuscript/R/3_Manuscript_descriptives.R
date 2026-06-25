# ---------------------------
# Script name: 3. Manuscript - descriptives
# Description: Script for participant characteristics and descriptives
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Setup ----
source(here::here("manuscript/R/1_Manuscript_setup.R"))
here::i_am("manuscript/R/3_Manuscript_descriptives.R") #Set location of script

data_evening <- readRDS(here::here("manuscript/R/output/data_evening_clean.rds"))
data_person  <- readRDS(here::here("manuscript/R/output/data_person_clean.rds"))
sample_flow  <- readRDS(here::here("manuscript/R/output/sample_flow.rds"))

# Participant characteristics ----

## Age ----
Age_m <- mean(data_person$Age_B, na.rm = TRUE) #mean
Age_sd <- sd(data_person$Age_B, na.rm = TRUE) #sd
Age_min <- min(data_person$Age_B, na.rm = TRUE) #minimum
Age_max <- max(data_person$Age_B, na.rm = TRUE) #maximum

## Student ----
student <- (100/sample_flow$n_par_4)*(sum(data_person$Student_B == 1, na.rm = TRUE)) #Percentage students

# ## Subset of participants who allow to have data shared (will only share that subset, 1=No, 2=Yes)
# share <- sum(data_person$Toest_aspect1 == 2, na.rm = TRUE)

# Procedure ---

## Number of beeps below 60 days ----------------
# sent.beeps.evening includes the onboarding day, so participants that participated all days will have 61 days 
n_beep1 <- sum(data_person$sent.beeps.evening == 60) #59 days
n_beep2 <- sum(data_person$Dropped.out == 0 & data_person$sent.beeps.evening < 60) #fewer, due to technical error (n=0)
n_beep3 <- sum(data_person$Dropped.out == 1 & data_person$sent.beeps.evening < 60) #fewer, due to drop out (n=2)

## Average compliance number ----------------
comp_m <- mean(data_person$comp.evening)
comp_sd <- sd(data_person$comp.evening)
comp_min <- min(data_person$comp.evening)
comp_max <- max(data_person$comp.evening)

## Average compliance percentage ----------------
comp_m_p <- mean(data_person$comp.evening.per)
comp_sd_p <- sd(data_person$comp.evening.per)
comp_min_p <- min(data_person$comp.evening.per)
comp_max_p <- max(data_person$comp.evening.per)

## Overall compliance ----------------
n_sent <- sum(data_person$sent.beeps.evening) #total sent beeps
n_obs_p <- (100/n_sent)*sample_flow$n_obs_4 #percentage of answered beep out of total sent beeps

## Compliance across weeks ----------------
comp_week<-data_evening %>%
  dplyr::group_by(week,filledin) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(Compliance = 100*(n / sum(n)))%>%
  dplyr::mutate(cum = cumsum(n))

comp1<-comp_week[2,4] #percentage of valid questionnaires week 1
comp2<-comp_week[10,4] #percentage of valid questionnaires week 5
comp3<-comp_week[18,4] #percentage of valid questionnaires week 9

## Individual compliance and study variables ----------------

# Here I correlate number of valid assessments per individual to study variables
# For ESM variables, I use person-level indices

### Calculate within-person aggregates of ESM study variables ----

# Loop over the variables and use dplyr to summarize
data_M <- data_evening %>%
  dplyr::group_by(participant.ID) %>%
  dplyr::summarize(
    across(all_of(ESM_variables), ~mean(., na.rm = TRUE), .names = "imean_{.col}"))

# Merge with person-level dataset
data_person <- left_join(data_person, data_M, by = "participant.ID")

### Association with compliance ----

#### Continuous variables ----

# List of columns to exclude from correlation analysis
exclude_columns <- c("participant.ID", "Toest_aspect1", "comp.evening", "comp.evening.per" , "gender.dum", "Student_B", "sent.beeps.evening", "Dropped.out")

# Exclude the columns from the list of all column names
variables_correlation <- setdiff(names(data_person), exclude_columns)

# Function to calculate correlation and p-value for a given variable
correlation_with_comp_evening <- function(variable) {
  cor_test_result <- cor.test(data_person$comp.evening, data_person[[variable]])
  return(list(estimate = cor_test_result$estimate,
              p_value = cor_test_result$p.value))
}

# Apply the function to each variable and store the results in a list
correlation_results <- lapply(variables_correlation, correlation_with_comp_evening)

# Convert the list to a data frame
correlation_df <- data.frame(variable = variables_correlation,
                             estimate = sapply(correlation_results, "[[", "estimate"),
                             p_value = sapply(correlation_results, "[[", "p_value"))


#### Categorical variables ----

# Categorical variables
variables_ttest <- c("gender.dum", "Student_B")

# Function to calculate correlation and p-value for a given variable
ttest_with_comp_evening <- function(variable) {
  t_test_result <- t.test(data_person$comp.evening ~ data_person[[variable]])
  return(list(estimate = t_test_result$statistic,
              p_value = t_test_result$p.value))
}

# Apply the function to each variable and store the results in a list
ttest_results <- lapply(variables_ttest, ttest_with_comp_evening)

# Convert the list to a data frame
ttest_df <- data.frame(variable = variables_ttest,
                       estimate = sapply(ttest_results, "[[", "estimate"),
                       p_value = sapply(ttest_results, "[[", "p_value"))


### Identify significant associations ----

# Merge correlation and t-test results
comp_ass<-rbind(correlation_df,ttest_df)

# Add a new column indicating p-value significance
comp_ass$significant <- comp_ass$p_value < 0.05

# Print the resulting dataframe
print(comp_ass)

# Subset rows with p_value < 0.05/ >.05
significant_rows <- subset(comp_ass, p_value < 0.05) 
nonsignificant_rows <- subset(comp_ass, p_value > 0.05)

# Significant results
## Suppression
sig_e_comp_sup <- significant_rows$estimate[1]
sig_p_comp_sup <- significant_rows$p_value[1]

# Non-significant results, minimum and maximum
nsig_e_comp_ESM_min <- min(nonsignificant_rows$estimate)
nsig_e_comp_ESM_max <- max(nonsignificant_rows$estimate)
nsig_p_comp_ESM_min <- min(nonsignificant_rows$p_value)
nsig_p_comp_ESM_max <- max(nonsignificant_rows$p_value)

# Calculate how often most intense emotion was related to most intense event ---
per_em_ev_rel<-mean(data_evening$n.em.ev == 0,na.rm=TRUE) * 100

# Descriptives of within-person means & variance of ESM variables ----------------------------------------------

## Setup ----
# Create a function to calculate mean, min, max and SD of within-person means & mean of within-person SD for each variable
calculate_descriptives <- function(var) {
  mean_var <- mean(data_person[[paste0("imean_", var)]], na.rm = TRUE)
  min_var <- min(data_person[[paste0("imean_", var)]], na.rm = TRUE)
  max_var <- max(data_person[[paste0("imean_", var)]], na.rm = TRUE)
  sd_bet_var <- sd(data_person[[paste0("imean_", var)]], na.rm = TRUE)
  
  # Create a data frame with the results for each variable
  result <- data.frame(
    variable = paste0(var),
    Mean = mean_var,
    Min = min_var,
    Max = max_var,
    SD_Between = sd_bet_var,
    stringsAsFactors = FALSE
  )
  return(result)
}

# Use lapply to calculate descriptives for each variable
descriptives <- lapply(ESM_variables, calculate_descriptives)

# Combine the results into a single data frame
results_desc <- do.call(rbind, descriptives)

## ICC results -------------------------------------------------------------

# Create a function to calculate ICC for each variable
calculate_icc <- function(var) {
  lme <- lmer(as.formula(paste(var, "~ 1 + (1 | participant.ID)")), data = data_evening)
  return(performance::icc(lme)$ICC_adjusted)
}

# Use lapply to calculate ICC for each variable and store results in a list
icc_results_list <- lapply(ESM_variables, calculate_icc)

# Convert the list to a dataframe
results_icc <- data.frame(variable = ESM_variables,
                          adjusted_icc = unlist(icc_results_list),
                          stringsAsFactors = FALSE)

results_icc<-apa_num(results_icc,gt1=FALSE)

# Extract minimum and maximum
icc_min<-as.numeric(min(results_icc$adjusted_icc))
icc_max<-as.numeric(max(results_icc$adjusted_icc))

# Extract percentages
icc_per_max<-100-(icc_min*100)
icc_per_min<-100-(icc_max*100)

## % zero responses ----------------------------------------------

# Create a function to calculate percentages for each variable
calculate_percentages <- function(variable) {
  # Calculate the percentage of zeros
  zeros <- (sum(data_evening[[variable]] == 0, na.rm = TRUE) / sum(!is.na(data_evening[[variable]]))) * 100
  
  # Create a data frame for the variable with percentages
  row <- data.frame(variable = variable, Percentage_Zeros = zeros, stringsAsFactors = FALSE)
  return(row)
}

# Use lapply to calculate percentages for each variable
results_per_list <- lapply(ESM_variables, calculate_percentages)

# Combine the results into a single data frame
results_per <- do.call(rbind, results_per_list)

# merge dataframes and rename rows
res_table_des <- cbind(results_desc, results_per, results_icc)%>%
  cbind(ESM_names,.)%>%
  select(-contains("variable")) 

# Rename columns
res_table_des_columns<-c("","Mean", "Min", "Max", "$SD$", "\\% 0 resp", "ICC")

colnames(res_table_des) <- res_table_des_columns

# Show table
res_table_des

## paired t-tests ER strategies ----------------------------------------------
# Subset the dataframe to include only the "imean" & "ER" variables
imean_ER_vars <- data_M %>% 
  select(starts_with("imean")) %>%
  select(contains(".er"))

imean_ER_vars<-as.data.frame(imean_ER_vars)

num_vars <- ncol(imean_ER_vars)

# Create an empty dataframe to store the results of the paired t-test
paired_t_test <- data.frame(Variable1 = character(),
                            Variable2 = character(),
                            t_value = numeric(),
                            p_value = numeric(),
                            stringsAsFactors = FALSE)

# Perform pairwise t-tests and store the results (loop through all combinations)
for (i in 1:(num_vars - 1)) {
  for (j in (i + 1):num_vars) {
    # Perform paired t-test
    t_test <- t.test(imean_ER_vars[, i], imean_ER_vars[, j], paired = TRUE)
    
    # Store the results in the dataframe
    paired_t_test <- rbind(paired_t_test, data.frame(Variable1 = names(imean_ER_vars)[i],
                                                     Variable2 = names(imean_ER_vars)[j],
                                                     t_value = t_test$statistic,
                                                     p_value = t_test$p.value,
                                                     stringsAsFactors = FALSE))
  }
}

print(paired_t_test)


# not significant 
paired_t_test[paired_t_test$p_value > 0.05, ]

# It is a big table, so I will extract a summary of the results

## Difference distraction, suppression, rumination 
dis_sup_rum<- paired_t_test %>%
  filter(Variable1 == "imean_n.er.dis" & Variable2 == "imean_n.er.sup" |
           Variable1 == "imean_n.er.rum" & Variable2 == "imean_n.er.dis" |
           Variable1 == "imean_n.er.rum" & Variable2 == "imean_n.er.sup")

dis_sup_rum_t_min<-min(dis_sup_rum$t_value)
dis_sup_rum_t_max<-max(dis_sup_rum$t_value)
dis_sup_rum_p_min<-min(dis_sup_rum$p_value)

## Difference relaxation and expression
rel_n.eng <- paired_t_test %>%
  filter(Variable1 == "imean_n.er.rel" & Variable2 == "imean_n.er.eng")

rel_n.eng_t<-rel_n.eng$t_value
rel_n.eng_p<-rel_n.eng$p_value

## All significant ones
sig_ttest<-paired_t_test[paired_t_test$p_value < 0.05, ]

sig_ttest_min<-min(abs(sig_ttest$t_value))
sig_ttest_max<-max(abs(sig_ttest$t_value))
sig_ttest_p_max<-max(sig_ttest$p_value)

# Within- and between-person correlation ----

# calculate within-person and between-person correlation and save results as excelfile
multilevel.cor(data_evening[, ESM_variables],
               cluster = data_evening$participant.ID, 
               split = FALSE, sig = TRUE, 
               tri.lower = FALSE,
               print = c("cor", "p"),
               write = "correlation")

# Move file to output folder
file.rename(
  "correlation.xlsx",
  here::here("manuscript", "R", "output", "correlation.xlsx")
)

# Read data from Excel sheets (results of multilevel correlations)
cor_raw <- readxl::read_excel(here::here("manuscript/R/output/correlation.xlsx"), sheet = 2) %>%
  as.data.frame()

p_raw <- readxl::read_excel(here::here("manuscript/R/output/correlation.xlsx"), sheet = 3) %>%
  as.data.frame()

# Function to assign stars based on p-values
p_to_stars <- function(p) {
  case_when(
    is.na(p)      ~ NA_character_,
    p < 0.001     ~ "***",
    p < 0.01      ~ "**",
    p < 0.05      ~ "*",
    TRUE          ~ ""
  )
}

format_cor_value <- function(x) {
  out <- papaja::apa_num(x, gt1 = FALSE)
  ifelse(out == "> .99", " ", out)
}

add_stars <- function(cor_val, stars) {
  ifelse(
    is.na(stars) | stars == "",
    cor_val,
    paste0(cor_val, stars)
  )
}

# Keep first column as variable names
colnames(cor_raw)[1] <- "variables"
colnames(p_raw)[1] <- "variables"

# Create star matrix form p-values
p_stars <- p_raw %>%
  mutate(across(-variables, p_to_stars))

# Create correlation matrix
cor_fmt <- cor_raw %>%
  mutate(across(-variables, format_cor_value))

# Add significance stars to correlation matrix
res_table_cor <- cor_fmt

res_table_cor[-1] <- Map(
  add_stars,
  cor_fmt[-1],
  p_stars[-1]
)

# Replace variable names with manuscript labels
ESM_names_no <- c(
  "1. Negative emotion intensity",
  "2. Negative emotion control",
  "3. Relaxation",
  "4. Expression",
  "5. Rumination",
  "6. Reappraisal",
  "7. Distraction",
  "8. Suppression"
)

res_table_cor <- res_table_cor %>%
  mutate(variables = ESM_names_no)

# Rename columns
colnames(res_table_cor) <- c("", paste0(1:8, "."))

# Show table
res_table_cor

# Save processed files and outputs ----
descriptives <- list(
  n_beep1=n_beep1,
  comp1 = comp1,
  comp3 = comp3,
  comp_min = comp_min,
  comp_max =comp_max,
  comp_m = comp_m,
  comp_sd = comp_sd,
  comp_m_p = comp_m_p,
  comp_sd_p = comp_sd_p,
  comp_min_p = comp_min_p,
  comp_max_p = comp_max_p,
  n_obs_p = n_obs_p,
  n_sent = n_sent,
  sig_e_comp_sup = sig_e_comp_sup,
  sig_p_comp_sup = sig_p_comp_sup,
  nsig_e_comp_ESM_min = nsig_e_comp_ESM_min,
  nsig_e_comp_ESM_max = nsig_e_comp_ESM_max,
  nsig_p_comp_ESM_min = nsig_p_comp_ESM_min,
  nsig_p_comp_ESM_max = nsig_p_comp_ESM_max,
  Age_m = Age_m,
  Age_sd = Age_sd,
  Age_min = Age_min,
  Age_max = Age_max,
  student = student,
  dis_sup_rum_t_min = dis_sup_rum_t_min,
  dis_sup_rum_t_max = dis_sup_rum_t_max,
  dis_sup_rum_p_min = dis_sup_rum_p_min,
  rel_n.eng_t = rel_n.eng_t,
  rel_n.eng_p = rel_n.eng_p,
  sig_ttest_min = sig_ttest_min,
  sig_ttest_max = sig_ttest_max,
  sig_ttest_p_max = sig_ttest_p_max,
  icc_min = icc_min,
  icc_max = icc_max,
  icc_per_min = icc_per_min,
  icc_per_max = icc_per_max,
  per_em_ev_rel = per_em_ev_rel,
  res_table_des = res_table_des,
  res_table_cor = res_table_cor
)

saveRDS(descriptives,  here::here("manuscript/R/output/descriptives.rds"))

