# ---------------------------
# Script name: 4. Manuscript - multilevel models
# Description: Script for multilevel models
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Setup ----
source(here::here("manuscript/R/1_Manuscript_setup.R"))
here::i_am("manuscript/R/4_Manuscript_multilevel.R") #Set location of script

data_evening <- readRDS(here::here("manuscript/R/output/data_evening_clean.rds"))
descriptives <- readRDS(here::here("manuscript/R/output/descriptives.rds"))

# 4 random effects per model, namely:
# - correlation random slope quadratic term emotion characteristics and random intercept emotion
# characteristics
# - random slope quadratic term emotion characteristics
# - correlation random slope linear term emotion characteristics and random intercept emotion characteristics
# - random slope linear term emotion characteristics

## ADJUSTMENT BASED ON REVISION 1: ONLY FOCUSED ON SWITCHING COMPONENT IN MAIN MANUSCRIPT

# Set up lists of variables ----------------------

# Negative ER variables (outcome) 
n.outcomes <- c("n.er.rel","n.er.eng","n.er.rum","n.er.reap","n.er.dis","n.er.sup", 
                "BrayCurtisRepl.amm", "BrayCurtisNest.amm")

# Negative ER variables (outcome) - single ER only
n.single.outcomes <- c("n.er.rel","n.er.eng","n.er.rum","n.er.reap","n.er.dis","n.er.sup")

# Outcome names - reported in paper (We report Endorsement change in the supplement)
n.outcome.names <- c('Relaxation','Expression','Rumination', 
                     'Reappraisal', 'Distraction','Suppression',
                     'Strategy Switching', 'Endorsement Change')

# Negative emotion variables (predictor)
n.predictors <- c("n.em.int.c", "n.em.cont.c")

# Column names for MLM tables
mlm.columns <- c("ER outcomes","est", "$SE$", "$p_{adj}$", "est", "$SE$", "$p_{adj}$",
                 "est", "$SE$", "$p_{adj}$", "est", "$SE$", "$p_{adj}$")

# Models ----------------------

## Negative ER and emotions ---------------------- 
# Create an empty data frame to store coefficients 
coefs.n <- data.frame() 

# Create an empty list to store fit.lme objects 
fits.n <- list() 

# Loop through all combinations of predictor outcome variables 
for (n.predictor in n.predictors) { 
  for (n.outcome in n.outcomes) { 
    formula <- as.formula(paste(n.outcome, "~ 1 +", n.predictor, "+ I(", n.predictor, "^2) + Age.c + gender.dum + micro + obs.evening")) 
    random <- as.formula(paste("~ 1 +", n.predictor, "+ I(", n.predictor, "^2) | participant.ID")) 
    fit.lme <- try(lme(formula, 
                       random = random, 
                       correlation = corAR1(), 
                       data = data_evening, 
                       na.action = na.exclude, 
                       method = 'REML', 
                       control = lmeControl(opt = 'optim')), 
                   silent = FALSE) 
    
    if (!inherits(fit.lme, "try-error")) { 
      coef_table <- summary(fit.lme)$tTable
      quad_term <- paste0("I(", n.predictor, "^2)")
      
      lin.est <- coef_table[n.predictor, "Value"]
      lin.se  <- coef_table[n.predictor, "Std.Error"]
      lin.p   <- coef_table[n.predictor, "p-value"]
      
      qua.est <- coef_table[quad_term, "Value"]
      qua.se  <- coef_table[quad_term, "Std.Error"]
      qua.p   <- coef_table[quad_term, "p-value"]
      
      coefs.n <- rbind(coefs.n, data.frame(predictor = n.predictor, 
                                           outcome = n.outcome, 
                                           lin.est = lin.est, 
                                           lin.se = lin.se, 
                                           lin.p = lin.p, 
                                           qua.est = qua.est, 
                                           qua.se = qua.se, 
                                           qua.p = qua.p)) 
      fits.n[[paste(n.predictor, n.outcome, sep = ".")]] <- fit.lme 
    } 
  } 
} 

# Summary of results 
fit.summary.n <- lapply(fits.n, summary) 

# View the coefficients data frame 
coefs.n %>% mutate_if(is.numeric, round, digits = 3)

## Interaction Negative emotion Intensity and controllability (Exploratory Test) ----------------------

# Create an empty data frame to store coefficients
coefs.mod <- data.frame()

# Create an empty list to store fit.lme objects
fits.mod <- list()

# Loop through all combinations of predictor outcome variables
for (n.single.outcome in n.single.outcomes) {
  formula <- as.formula(paste(n.single.outcome, "~ 1 + n.em.int.c + n.em.cont.c + n.em.int.c*n.em.cont.c + Age.c + gender.dum + micro + obs.evening"))
  random <- as.formula(paste("~ 1 + n.em.int.c + n.em.cont.c + n.em.int.c*n.em.cont.c | participant.ID"))
  fit.lme <- try(lme(formula, 
                     random = random, 
                     correlation = corAR1(),
                     data = data_evening, 
                     na.action = na.exclude, 
                     method = 'REML',
                     control = lmeControl(opt = 'optim')), 
                 silent = FALSE)
  
  if (!inherits(fit.lme, "try-error")) {
    coef_table <- summary(fit.lme)$tTable
    interaction_term <- "n.em.int.c:n.em.cont.c"
    
    mod.est <- coef_table[interaction_term, "Value"]
    mod.se  <- coef_table[interaction_term, "Std.Error"]
    mod.p   <- coef_table[interaction_term, "p-value"]
    
    coefs.mod <- rbind(coefs.mod, data.frame(outcome = n.single.outcome, 
                                             mod.est = mod.est, 
                                             mod.se = mod.se, 
                                             mod.p = mod.p))
    
    fits.mod[[paste(n.single.outcome, sep = ".")]] <- fit.lme
  }
}

# Summary of results
fit.summary.mod <- lapply(fits.mod, summary)

# View the coefficients data frame
coefs.mod %>% mutate_if(is.numeric, round, digits = 3)


# Multiple Testing Adjustment ----------------------
## Transpose all dataframes, so that each effect is in one row

coefs.n.lin <- coefs.n %>%
  select(c("outcome","lin.est","lin.se","lin.p"))

coefs.n.qua <- coefs.n %>%
  select(c("outcome","qua.est","qua.se","qua.p"))


## Rename rows for rbind 
names_est <- c("outcome","est","SE","p")

colnames(coefs.n.lin)<-names_est
colnames(coefs.n.qua)<-names_est
colnames(coefs.mod)<-names_est

## Bind all coefficients 
all_estimates <- rbind(coefs.n.lin,coefs.n.qua,coefs.mod)

## Correct for multiple testing using the Holm method
# According to the description of the p.adjust package, Holm is preferred over 
# Bonferroni: "There seems no reason to use the unmodified Bonferroni correction 
# because it is dominated by Holm's method, which is also valid under arbitrary assumptions."

all_estimates$p.adj<-p.adjust(all_estimates$p, method = "holm")

# Show table
all_estimates

# Compare model fit of linear and quadratic model (not pre-registered) ----------------------
# as a response to a co-author, we also added model comparisons 
# Comment: Exploratory comparisons on model fit metrics (AIC/BIC/RMSE) may show that quadratic fits better in all cases (that's my guess). This will make your the purpose of this paper - to remind people to consider and analyze quadratic effect - stand out even more.
# Note that I need to refit the models with ML since REML (Restricted Maximum Likelihood) is not appropriate when the models have different fixed effects 

## ADJUSTMENT BASED ON REVISION 1: ADDED BIC TO TABLE

## Fit Quadratic models
# Create an empty list to store fit.lme objects
fits.n.quad <- list()

# Create an empty data frame to store AIC/BIC results
aicbic_results <- data.frame()

# Quadratic models

for (n.predictor in n.predictors) {
  for (n.outcome in n.outcomes) {
    
    formula <- as.formula(paste(n.outcome, "~ 1 +", n.predictor, "+ I(", n.predictor, "^2) + Age.c + gender.dum + micro + obs.evening")
    )
    
    random <- as.formula(paste("~ 1 +", n.predictor, "+ I(", n.predictor, "^2) | participant.ID")
    )
    
    fit.lme <- try(
      lme(formula, 
          random = random, 
          correlation = corAR1(),
          data = data_evening, 
          na.action = na.exclude, 
          method = "ML",
          control = lmeControl(opt = "optim")), 
      silent = FALSE
    )
    
    model_name <- paste(n.predictor, n.outcome, sep = ".")
    fits.n.quad[[model_name]] <- fit.lme
    
    if (!inherits(fit.lme, "try-error")) {
      aicbic_results <- rbind(aicbic_results, data.frame(
        predictor = n.predictor,
        outcome = n.outcome,
        model_name = model_name,
        model_type = "quadratic",
        AIC = AIC(fit.lme),
        BIC = BIC(fit.lme)
      ))
    }
  }
}

## Fit linear models
# Create an empty list to store fit.lme objects
fits.n.lin <- list()

# Linear models: single ER strategies only

for (n.predictor in n.predictors) {
  for (n.outcome in n.outcomes) {
    
    formula <- as.formula(paste(n.outcome, "~ 1 +", n.predictor, "+ Age.c + gender.dum + micro + obs.evening")
    )
    
    random <- as.formula(paste("~ 1 +", n.predictor, "| participant.ID")
    )
    
    fit.lme <- try(
      lme(formula, 
          random = random, 
          correlation = corAR1(),
          data = data_evening, 
          na.action = na.exclude, 
          method = "ML",
          control = lmeControl(opt = "optim")), 
      silent = FALSE
    )
    
    model_name <- paste(n.predictor, n.outcome, sep = ".")
    fits.n.lin[[model_name]] <- fit.lme
    
    if (!inherits(fit.lme, "try-error")) {
      aicbic_results <- rbind(aicbic_results, data.frame(
        predictor = n.predictor,
        outcome = n.outcome,
        model_name = model_name,
        model_type = "linear",
        AIC = AIC(fit.lme),
        BIC = BIC(fit.lme)
      ))
    }
  }
}

# Summary of results
fit.summary.n.lin <- lapply(fits.n.lin, summary)

## Compare linear (model 1) and quadratic (model 2) models
columns <- c("n.er.rel", "n.er.eng", "n.er.rum", "n.er.reap", "n.er.dis", "n.er.sup",
             "BrayCurtisRepl.amm", "BrayCurtisNest.amm")

predictors <- c("n.em.int.c", "n.em.cont.c")

results_n_er_comp <- list()

for (pred in predictors) {
  for (col in columns) {
    
    model_name <- paste(pred, col, sep = ".")
    
    fit1 <- fits.n.lin[[model_name]]
    fit2 <- fits.n.quad[[model_name]]
    
    if (!inherits(fit1, "try-error") && !inherits(fit2, "try-error")) {
      
      anova_res <- anova(fit1, fit2)
      
      results_n_er_comp[[paste(pred, col, sep = "_")]] <- list(
        predictor = pred,
        outcome = col,
        AIC_model1 = anova_res$AIC[1],
        AIC_model2 = anova_res$AIC[2],
        BIC_model1 = anova_res$BIC[1],
        BIC_model2 = anova_res$BIC[2],
        l_ratio = anova_res$L.Ratio[2],
        p_value = anova_res$`p-value`[2]
      )
    }
  }
}

# Convert to data.frame
results_n_er_anova <- data.frame(
  predictor = sapply(results_n_er_comp, function(x) x$predictor),
  outcome = sapply(results_n_er_comp, function(x) x$outcome),
  AIC_model1 = sapply(results_n_er_comp, function(x) x$AIC_model1),
  AIC_model2 = sapply(results_n_er_comp, function(x) x$AIC_model2),
  BIC_model1 = sapply(results_n_er_comp, function(x) x$BIC_model1),
  BIC_model2 = sapply(results_n_er_comp, function(x) x$BIC_model2),
  l_ratio = sapply(results_n_er_comp, function(x) x$l_ratio),
  p_value = sapply(results_n_er_comp, function(x) x$p_value),
  row.names = NULL
)

results_n_er_anova$delta_AIC <- results_n_er_anova$AIC_model1 - results_n_er_anova$AIC_model2
results_n_er_anova$delta_BIC <- results_n_er_anova$BIC_model1 - results_n_er_anova$BIC_model2

results_n_er_anova <- results_n_er_anova %>%
  select(predictor, AIC_model1, AIC_model2, delta_AIC,
         BIC_model1, BIC_model2, delta_BIC,
         l_ratio, p_value)

# Show table
results_n_er_anova

# Add predicted values of outcomes at meaningful predictor values --- 

## ADJUSTMENT BASED ON REVISION 1: TRANSLATED QUADRATIC COEFFICIENTS INTO PREDICTED OUTCOMES AT MEANINGFUL VALUES OF THE PREDICTOR (-2SD, MEAN, +2SD)

## Get within-person SD of predictor
sd_n_em_int <- data_evening %>%
  group_by(participant.ID) %>%
  summarise(sd = sd(n.em.int.c, na.rm = TRUE)) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE)) %>%
  pull(mean_sd)

sd_n_em_cont <- data_evening %>%
  group_by(participant.ID) %>%
  summarise(sd = sd(n.em.cont.c, na.rm = TRUE)) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE)) %>%
  pull(mean_sd)

## Compute predicted values with emmeans package for models with significant quadratic terms
# For controllability/intensity, we use values of -2SD, the mean (which is 0 because it is centered), and +2SD
# The other variables are covariates and will be held constant. Age was already centered, thus we use 0.
# For categorical covariates (gender and microintervention), we use the reference category (0 = female & no micro-intervention)
# obs.evening was not centered. Here, we use the midpoint of the study (day 30 of 60)

reap.int <- emmeans(fits.n$n.em.int.c.n.er.reap, ~ n.em.int.c,
                    at = list(n.em.int.c = c(-2*sd_n_em_int, 0, 2*sd_n_em_int), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

sup.int <- emmeans(fits.n$n.em.int.c.n.er.sup, ~ n.em.int.c,
                    at = list(n.em.int.c = c(-2*sd_n_em_int, 0, 2*sd_n_em_int), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

reap.con <- emmeans(fits.n$n.em.cont.c.n.er.reap, ~ n.em.cont.c,
                    at = list(n.em.cont.c = c(-2*sd_n_em_cont, 0, 2*sd_n_em_cont), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

sup.con <- emmeans(fits.n$n.em.cont.c.n.er.sup, ~ n.em.cont.c,
                   at = list(n.em.cont.c = c(-2*sd_n_em_cont, 0, 2*sd_n_em_cont), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

rum.con <- emmeans(fits.n$n.em.cont.c.n.er.rum, ~ n.em.cont.c,
                    at = list(n.em.cont.c = c(-2*sd_n_em_cont, 0, 2*sd_n_em_cont), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

BrayCurtisNest.amm.con <- emmeans(fits.n$n.em.cont.c.BrayCurtisNest.amm, ~ n.em.cont.c,
                   at = list(n.em.cont.c = c(-2*sd_n_em_cont, 0, 2*sd_n_em_cont), Age.c = 0,gender.dum = 0, micro = 0, obs.evening = 30))

## Put results in table
pred_quad_models <- bind_rows(
  as.data.frame(reap.int) %>% dplyr::rename(level = n.em.int.c) %>% mutate(model = "Intensity - Reappraisal"),
  as.data.frame(sup.int)  %>% dplyr::rename(level = n.em.int.c) %>% mutate(model = "Intensity - Suppression"),
  as.data.frame(reap.con) %>% dplyr::rename(level = n.em.cont.c) %>% mutate(model = "Controllability - Reappraisal"),
  as.data.frame(sup.con)  %>% dplyr::rename(level = n.em.cont.c) %>% mutate(model = "Controllability - Suppression"),
  as.data.frame(rum.con)  %>% dplyr::rename(level = n.em.cont.c) %>% mutate(model = "Controllability - Rumination"),
  as.data.frame(BrayCurtisNest.amm.con)  %>% dplyr::rename(level = n.em.cont.c) %>% mutate(model = "Controllability - Strategy Switching"),
  ) %>%
  mutate(
    level_label = case_when(
      level < 0 ~ "Low",
      level == 0 ~ "Mean",
      level > 0 ~ "High"
    )
  ) %>%
  select(model, level_label, emmean) %>%
  pivot_wider(names_from = level_label,values_from = emmean) %>%
  as.data.frame()

pred_quad_models

# Save processed files and outputs ----
names <- list(
  mlm.columns=mlm.columns,
  n.outcome.names=n.outcome.names,
  names_est=names_est
)

saveRDS(all_estimates,  here::here("manuscript/R/output/all_estimates.rds"))
saveRDS(fits.n, here::here("manuscript/R/output/fits.n.rds"))
saveRDS(results_n_er_anova, here::here("manuscript/R/output/results_n_er_anova.rds"))
saveRDS(names, here::here("manuscript/R/output/names.rds"))
saveRDS(pred_quad_models, here::here("manuscript/R/output/pred_quad_models.rds"))



