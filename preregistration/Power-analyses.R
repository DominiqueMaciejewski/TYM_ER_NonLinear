#' ---
#' title: "Power analysis - Main analyses"
#' author: "Dominique Maciejewski"
#' ---

# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#
# Notes: Big thanks to Ginette Lafit for providing me with a script for estimating power for the quadratic effects!

# Options ----------------------------------------------

options(scipen = 999) 
here::i_am("preregistration/Power-analyses.R") #Set location of script
sessionInfo()

# Library ----------------------------------------------

library(worcs)
library(here)
library(esmpack)
library(nlme)
library(dplyr)
library(lmerTest)
library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(shinyjs)
library(compiler)
library(future.apply)
library(PowerAnalysisIL)
library(tictoc)
library(kableExtra)
library(viridis)
library(writexl)

#+ cache=TRUE

# Read in data ----------------------------------------------
load_data()

# Rescale variables ----------------------------------------------

# Write function that rescales by 10
divide_by_10 <- function(x) {
  return(x / 10)
}

# apply to all variables that use scale 1-100
ressema_df[,2:9] <- apply(ressema_df[,2:9], 2, divide_by_10)

# Input for simulation study ----------------------------------------------

# For the input of the simulation studies, we take results from Medland et al. (2020)
# We run analyses for all RQ's
# RQ1: Within-person association emotional intensity/controllability and ER 
# RQ2: Moderation by psychopathology for within-person association emotional intensity/controllability and ER 

## Step 1: Obtain the mean and the variances of the predictor variables ----

### Emotional intensity and controllability (level 1 predictor) ----

# For this, we run an unconditional MLM with emotional intensity/controllability as outcome and no predictors
# Note that emotional intensity/controllability are our predictors in the actual analyses!

# From these results, we need to extract 
# -	mu.X: Mean of the Level 1 continuous predictor X (fixed intercept)
# -	sigma.X: Standard deviation of the Level 1 continuous predictor X 
# -	rho.X: Autocorrelation of the Level 1 continuous predictor X 
# -	sigma.X.v0: Standard deviation of random intercept of the Level 1 continuous predictor X

# Function for looping lme for two predictors

predictor <- 1
outcomes <- c("int", "ctrl")
coefs.df.0.X <- data.frame()

for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, "~", predictor))
  fit.lme.0 <- try(lme(formula, 
                     random = ~1 | sema_id, 
                     correlation = corAR1(),
                     data = ressema_df, 
                     na.action = na.omit, 
                     method = 'REML',
                     control = lmeControl(opt='optim')), 
                 silent = FALSE) 
  
  # Extract the coefficients and add them to the data frame
  if (!inherits(fit.lme.0, "try-error")) {
    mu.x <- coef(summary(fit.lme.0))[1, 1]
    sigma.x <- as.numeric(VarCorr(fit.lme.0)[1, 2])
    rho.x <- as.numeric(coef(fit.lme.0$modelStruct$corStruct, unconstrained = FALSE))
    sigmav0.x <- as.numeric(VarCorr(fit.lme.0)[2, 2])
    
    coefs.df.0.X <- rbind(coefs.df.0.X, data.frame(outcome = outcome, 
                                               mu.x = mu.x, 
                                               sigma.x = sigma.x, 
                                               rho.x = rho.x, 
                                               sigmav0.x = sigmav0.x))
  } else {
    print(fit.lme.0) # print the error message if the model failed to fit
    

    }
}

# View the coefficients data frame
coefs.df.0.X %>% 
  mutate_if(is.numeric, round, digits = 3)

### Psychopathology (level 2 predictor) ----

# For this, we run an unconditional MLM with psychopathology as outcome and no predictors
# Note that psychopathology will be our predictor(i.e., moderator) in the actual analyses!

# From these results, we need to extract 
# -	mu.W: Mean of the Level 2 continuous moderator W (fixed intercept)
# -	sigma.W: Standard deviation of the Level 2 continuous moderator W 

# Function for looping over two predictors

# Create a list of two variables
variables <- c("PSS", "NA.")

# Create an empty list to store the results
coefs.df.0.W <- list()

# Loop through the variables and calculate the group mean and standard deviation
for (variable in variables) {
  groupmean <- aggregate(ressema_df[[variable]], 
                         list(ressema_df$sema_id), 
                         FUN = mean, 
                         data = ressema_df, 
                         na.rm = TRUE)
  
  mean_W <- mean(groupmean[,2])
  sd_W <- sd(groupmean[,2])
  
  # Add the results to the list
  coefs.df.0.W[[variable]] <- list(mean_W = mean_W, sd_W = sd_W)
}

# Access the results for each variable
coefs.df.0.W$PSS$mean_W
coefs.df.0.W$PSS$sd_W

coefs.df.0.W$NA.$mean_W
coefs.df.0.W$NA.$sd_W


## Step 2: Obtain the parameter estimates from the conditional MLM's ----

# Person-center level-1 predictor variables
ressema_df$int.c<-calc.mcent(int, sema_id, data=ressema_df)
ressema_df$ctrl.c<-calc.mcent(ctrl, sema_id, data=ressema_df)

# Grandmean-center level-2 predictor variables
ressema_df <- ressema_df %>% 
  dplyr::mutate(PSS.c = PSS-coefs.df.0.W$PSS$mean_W,
                NA.c = NA.-coefs.df.0.W$NA.$mean_W)

### 1. Within-person association between emotional context and ER ----
# Linear and quadratic within-person association between emotion intensity/controllability 
# (predictors) and six ER strategies (outcomes)
# This is model 3 from the shiny-app by Lafit et al. (2021), 
# but Ginette Lafit wrote a script that extends model 3 to quadratic predictors

# From these results, we need to extract 
# b00: Fixed intercept
# b10: Fixed effect of Level 1 continuous predictor X on the outcome (fixed slope)
# b11: Fixed effect of Level 1 continuous predictor X^2 the outcome (fixed slope)
#
# sigma: Std. deviation of the Level 1 error
# rho: Autocorrelation of the Level 1 error
#
# sigma.v0: Std. deviation of the random intercept
# sigma.v1: Std. deviation of the random slope of predictor X
# sigma.v2: Std. deviation of the random slope of predictor X^2
# rho.v01: correlation between the random intercept and the random slope of predictor X
# rho.v02: correlation between the random intercept and the random slope of predictor X^2
# rho.v12: correlation between the random slope of predictor X and the random slope of predictor X^2

# Compute the quadratic predicts 
ressema_df$int.c.2 = I(ressema_df$int.c^2)
ressema_df$ctrl.c.2 = I(ressema_df$ctrl.c^2)

# Function for looping lme for all predictor and outcome variable combinations

# Set up a list of predictor and outcome variable names
predictors <- c("int.c", "ctrl.c")
outcomes <- c("relax2", "exp1", "rumi2", "reap1", "dist2", "sup1")


# Create an empty data frame to store coefficients
coefs.df.int.2 <- data.frame()
coefs.df.ctrl.2 <- data.frame()

# Create an empty list to store fit.lme objects
fits.int.2 <- list()
fits.ctrl.2 <- list()

# Loop through all combinations of predictor and response variables
# intensity
  for (outcome in outcomes) {
    formula <- as.formula(paste(outcome, " ~ 1 + int.c + int.c.2")) 
    random <- as.formula(paste(" ~ 1 + int.c + int.c.2| sema_id"))
    fit.lme.2 <- try(lme(formula, 
                       random = random, 
                       correlation = corAR1(),
                       data = ressema_df, 
                       na.action = na.omit, 
                       method = 'REML',
                       control = lmeControl(opt='optim')), 
                   silent = FALSE) 
    
    # Extract the coefficients and add them to the data frame
    if (!inherits(fit.lme.2, "try-error")) {
      b00 <- coef(summary(fit.lme.2))[1, 1] 
      b10 <- coef(summary(fit.lme.2))[2, 1] 
      p10 <- coef(summary(fit.lme.2))[2, 5] 
      b11 <- coef(summary(fit.lme.2))[3, 1] 
      p11 <- coef(summary(fit.lme.2))[3, 5] 
      
      sigma <- as.numeric(VarCorr(fit.lme.2)[4, 2])
      rho <- as.numeric(coef(fit.lme.2$modelStruct$corStruct, unconstrained = FALSE))
      
      sigma.v0 <- as.numeric(VarCorr(fit.lme.2)[1, 2]) 
      sigma.v1 <- as.numeric(VarCorr(fit.lme.2)[2, 2]) 
      sigma.v2 <- as.numeric(VarCorr(fit.lme.2)[3, 2])
      rho.v01 <- as.numeric(VarCorr(fit.lme.2)[2, 3])
      rho.v02 <- as.numeric(VarCorr(fit.lme.2)[3, 3])
      rho.v12 <- as.numeric(VarCorr(fit.lme.2)[3, 4])
      
      coefs.df.int.2 <- rbind(coefs.df.int.2, data.frame(predictor = "int.c.2", 
                                                 outcome = outcome, 
                                                 b00 = b00, 
                                                 b10 = b10, 
                                                 p10 = p10, 
                                                 b11 = b11, 
                                                 p11 = p11,
                                                 sigma = sigma, 
                                                 rho = rho, 
                                                 sigma.v0 = sigma.v0, 
                                                 sigma.v1 = sigma.v1, 
                                                 sigma.v2 = sigma.v2, 
                                                 rho.v01 = rho.v01,
                                                 rho.v02 = rho.v02,
                                                 rho.v12 = rho.v12))
      
      # Save the fit.lme object to the list
      fits.int.2[[paste("int.c.2", outcome, sep = ".")]] <- fit.lme.2
    }
  }

# control

  for (outcome in outcomes) {
    formula <- as.formula(paste(outcome, " ~ 1 + ctrl.c + ctrl.c.2")) 
    random <- as.formula(paste(" ~ 1 + ctrl.c + ctrl.c.2| sema_id"))
    fit.lme.2 <- try(lme(formula, 
                         random = random, 
                         correlation = corAR1(),
                         data = ressema_df, 
                         na.action = na.omit, 
                         method = 'REML',
                         control = lmeControl(opt='optim')), 
                     silent = FALSE) 
    
    # Extract the coefficients and add them to the data frame
    if (!inherits(fit.lme.2, "try-error")) {
      b00 <- coef(summary(fit.lme.2))[1, 1] 
      b10 <- coef(summary(fit.lme.2))[2, 1] 
      p10 <- coef(summary(fit.lme.2))[2, 5] 
      b11 <- coef(summary(fit.lme.2))[3, 1] 
      p11 <- coef(summary(fit.lme.2))[3, 5] 
      
      sigma <- as.numeric(VarCorr(fit.lme.2)[4, 2])
      rho <- as.numeric(coef(fit.lme.2$modelStruct$corStruct, unconstrained = FALSE))
      
      sigma.v0 <- as.numeric(VarCorr(fit.lme.2)[1, 2]) 
      sigma.v1 <- as.numeric(VarCorr(fit.lme.2)[2, 2]) 
      sigma.v2 <- as.numeric(VarCorr(fit.lme.2)[3, 2])
      rho.v01 <- as.numeric(VarCorr(fit.lme.2)[2, 3])
      rho.v02 <- as.numeric(VarCorr(fit.lme.2)[3, 3])
      rho.v12 <- as.numeric(VarCorr(fit.lme.2)[3, 4])
      
      coefs.df.ctrl.2 <- rbind(coefs.df.ctrl.2, data.frame(predictor = "ctrl.c.2", 
                                                 outcome = outcome, 
                                                 b00 = b00, 
                                                 b10 = b10, 
                                                 p10 = p10, 
                                                 b11 = b11, 
                                                 p11 = p11,
                                                 sigma = sigma, 
                                                 rho = rho, 
                                                 sigma.v0 = sigma.v0, 
                                                 sigma.v1 = sigma.v1, 
                                                 sigma.v2 = sigma.v2, 
                                                 rho.v01 = rho.v01,
                                                 rho.v02 = rho.v02,
                                                 rho.v12 = rho.v12))
      
      # Save the fit.lme object to the list
      fits.ctrl.2[[paste("ctrl.c.2", outcome, sep = ".")]] <- fit.lme.2
    }
  }


# View the coefficients data frame
coefs.df.int.2 %>% 
  mutate_if(is.numeric, round, digits = 3)

coefs.df.ctrl.2 %>% 
  mutate_if(is.numeric, round, digits = 3)

# View all summaries
fit.summary.int.2 <- lapply(fits.int.2, summary)
fit.summary.ctrl.2 <- lapply(fits.ctrl.2, summary)

### 2. Moderation by between-person difference in psychopathology ----
# Moderation by between-person psychopathology of within-person association 
# between emotion intensity/controllability and six ER strategies
# Model 7 from the shiny-app by Lafit et al. (2021)
# 
# From these results, we need to extract 
# b00: Fixed intercept
# b01: Effect of Level-2 continuous moderator W on the intercept
# b10: Fixed effect of Level-1 continuous predictor X on the outcome (fixed slope)
# b11: Effect of Level-2 continuous moderator W on the slope(i.e., interaction)
#
# sigma: Std. deviation of the Level 1 error
# rho: Autocorrelation of the Level 1 error
# 
# sigma.v0: Std. deviation of the random intercept
# sigma.v1: Std. deviation of the random slope of predictor X
# rho.v01: correlation between the random intercept and the random slope of predictor X

# Function for looping lme for all predictor and outcome variable combinations

# Create an empty data frame to store coefficients
coefs.df.int.3 <- data.frame()
coefs.df.ctrl.3 <- data.frame()

# Create an empty list to store fit.lme objects
fits.int.3 <- list()
fits.ctrl.3 <- list()

# Loop through all combinations of predictor and response variables

# intensity 
for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, " ~ 1 + int.c + PSS.c + int.c:PSS.c")) 
  random <- as.formula(paste(" ~ 1 + int.c | sema_id"))
  fit.lme.3 <- try(lme(formula, 
                       random = random, 
                       correlation = corAR1(),
                       data = ressema_df, 
                       na.action = na.omit, 
                       method = 'REML',
                       control = lmeControl(opt='optim')), 
                   silent = FALSE) 
  
  # Extract the coefficients and add them to the data frame
  if (!inherits(fit.lme.3, "try-error")) {
    b00 <- coef(summary(fit.lme.3))[1, 1] 
    b01 <- coef(summary(fit.lme.3))[3, 1] 
    p01 <- coef(summary(fit.lme.3))[3, 5] 
    
    b10 <- coef(summary(fit.lme.3))[2, 1] 
    p10 <- coef(summary(fit.lme.3))[2, 5] 
    
    b11 <- coef(summary(fit.lme.3))[4, 1] 
    p11 <- coef(summary(fit.lme.3))[4, 5] 
    
    sigma <- as.numeric(VarCorr(fit.lme.3)[3, 2])
    rho <- as.numeric(coef(fit.lme.3$modelStruct$corStruct, unconstrained = FALSE))
    
    sigma.v0 <- as.numeric(VarCorr(fit.lme.3)[1, 2]) 
    sigma.v1 <- as.numeric(VarCorr(fit.lme.3)[2, 2]) 
    
    rho.v01 <- as.numeric(VarCorr(fit.lme.3)[2, 3])
    
    coefs.df.int.3 <- rbind(coefs.df.int.3, data.frame(predictor = "int.c",
                                                       moderator = "Stress",
                                                       outcome = outcome, 
                                                       b00 = b00, 
                                                       b01 = b01, 
                                                       p01 = p01, 
                                                       b10 = b10, 
                                                       p10 = p10, 
                                                       b11 = b11, 
                                                       p11 = p11,
                                                       sigma = sigma, 
                                                       rho = rho, 
                                                       sigma.v0 = sigma.v0, 
                                                       sigma.v1 = sigma.v1, 
                                                       rho.v01 = rho.v01))
    
    # Save the fit.lme object to the list
    fits.int.3[[paste("int.c", outcome, sep = ".")]] <- fit.lme.3
  }
}

# control

for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, " ~ 1 + ctrl.c + PSS.c + ctrl.c:PSS.c")) 
  random <- as.formula(paste(" ~ 1 + ctrl.c | sema_id"))
  fit.lme.3 <- try(lme(formula, 
                       random = random, 
                       correlation = corAR1(),
                       data = ressema_df, 
                       na.action = na.omit, 
                       method = 'REML',
                       control = lmeControl(opt='optim')), 
                   silent = FALSE) 
  
  # Extract the coefficients and add them to the data frame
  if (!inherits(fit.lme.3, "try-error")) {
    b00 <- coef(summary(fit.lme.3))[1, 1] 
    b01 <- coef(summary(fit.lme.3))[3, 1] 
    p01 <- coef(summary(fit.lme.3))[3, 5] 
    
    b10 <- coef(summary(fit.lme.3))[2, 1] 
    p10 <- coef(summary(fit.lme.3))[2, 5] 
    
    b11 <- coef(summary(fit.lme.3))[4, 1] 
    p11 <- coef(summary(fit.lme.3))[4, 5] 
    
    sigma <- as.numeric(VarCorr(fit.lme.3)[3, 2])
    rho <- as.numeric(coef(fit.lme.3$modelStruct$corStruct, unconstrained = FALSE))
    
    sigma.v0 <- as.numeric(VarCorr(fit.lme.3)[1, 2]) 
    sigma.v1 <- as.numeric(VarCorr(fit.lme.3)[2, 2]) 
    
    rho.v01 <- as.numeric(VarCorr(fit.lme.3)[2, 3])
    
    coefs.df.ctrl.3 <- rbind(coefs.df.ctrl.3, data.frame(predictor = "ctrl.c",
                                                       moderator = "Stress",
                                                       outcome = outcome, 
                                                       b00 = b00, 
                                                       b01 = b01, 
                                                       p01 = p01, 
                                                       b10 = b10, 
                                                       p10 = p10, 
                                                       b11 = b11, 
                                                       p11 = p11,
                                                       sigma = sigma, 
                                                       rho = rho, 
                                                       sigma.v0 = sigma.v0, 
                                                       sigma.v1 = sigma.v1, 
                                                       rho.v01 = rho.v01))
    
    # Save the fit.lme object to the list
    fits.ctrl.3[[paste("ctrl.c", outcome, sep = ".")]] <- fit.lme.3
  }
}

# View the coefficients data frame
coefs.df.int.3 %>% 
  mutate_if(is.numeric, round, digits = 3)

coefs.df.ctrl.3 %>% 
  mutate_if(is.numeric, round, digits = 3)

# View all summaries
fit.summary.int.3 <- lapply(fits.int.3, summary)
fit.summary.ctrl.3 <- lapply(fits.ctrl.3, summary)

# Simulation study ----------------------------------------------

## 1. Power Analyses within-person associations ----
# Note: This script was made by Ginette Lafit

### Function to generate data ----
# Simulate data from Model 3: Y ~ b00 + b10*X + b11*X^2 with random slope

Sim.Data.ML.3.VAR = function(N,T.obs,
                             b00,b10,b11,
                             sigma,rho,
                             sigma.v0,sigma.v1,sigma.v2,
                             rho.v01,rho.v02,rho.v12,
                             mu.X,sigma.X,sigma.X.v0,rho.X,isX.center){
  
  # Total number of subjects
  N = N
  
  # Create variables days, beeps per day and Z
  data.IL = expand.grid(Time=1:T.obs,subjno=1:N)
  
  # Simulate error level-1 
  if (rho == 0 | length(rho) == 0){E = rnorm(T.obs*N,0,sigma)}
  if (abs(rho) > 0){
    AR.epsilon = list(order=c(1,0,0), ar=rho)
    E = rep(0,T.obs*N)
    for (i in 1:N){
      E[which(data.IL$subjno==i)] = arima.sim(n=T.obs,AR.epsilon)*sigma*sqrt(1-rho^2)
    }}
  
  # Simulate error level-2
  # Simulate between-subject random effect
  Sigma.v = diag(c(sigma.v0^2,sigma.v1^2,sigma.v2^2))
  Sigma.v[1,2] = Sigma.v[2,1] =rho.v01*sigma.v0*sigma.v1
  Sigma.v[1,3] = Sigma.v[3,1] =rho.v02*sigma.v0*sigma.v2
  Sigma.v[2,3] = Sigma.v[3,2] =rho.v12*sigma.v1*sigma.v2
  
  if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}
  
  V.i = mvrnorm(N,rep(0,ncol(Sigma.v)),Sigma.v)
  V = matrix(0,T.obs*N,3)
  for (i in 1:N){
    V[which(data.IL$subjno==i),1] = V.i[i,1]
    V[which(data.IL$subjno==i),2] = V.i[i,2]
    V[which(data.IL$subjno==i),2] = V.i[i,2]
  }
  
  # Simulate error level-1 for X
  if (rho.X == 0 | length(rho.X) == 0){E.X = rnorm(T.obs*N,0,sigma.X)}
  if (abs(rho.X) > 0){
    AR.epsilon = list(order=c(1,0,0), ar=rho.X)
    E.X = rep(0,T.obs*N)
    for (i in 1:N){
      E.X[which(data.IL$subjno==i)] = arima.sim(n=T.obs,AR.epsilon)*sigma.X*sqrt(1-rho.X^2)
    }}
  
  # Simulate random intercept of the time varying variable X
  if (sigma.X.v0<= 0) {stop('The variance of the random intercept of the time-varying predictor must be positive')}
  nu.00.X = rnorm(N,mean=0,sd=sigma.X.v0)
  
  # Simulate time varying variable X
  if (sigma.X<= 0) {stop('The variance of the time-varying predictor must be positive')}
  X = rep(0,N*T.obs)
  for (i in 1:N){
    X[which(data.IL$subjno==i)] = mu.X + nu.00.X[i] + E.X[which(data.IL$subjno==i)]
  }
  
  B00 = rep(b00,nrow(data.IL))
  B10 = rep(b10,nrow(data.IL))
  B11 = rep(b11,nrow(data.IL))
  
  if (isX.center==TRUE){
    # Mean centered time varying variable per-individual
    for (i in 1:N){
      X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
    }}
  
  # Create quadratic variable X^2
  X.2 = rep(0,N*T.obs)
  for (i in 1:N){
    X.2[which(data.IL$subjno==i)] = I(X[which(data.IL$subjno==i)]^2)
  }
  
  # Compute Dependent Variable
  Y = B00 + B10*X + B11*X.2 + V[,1] + V[,2]*X + V[,3]*X.2 + E
  
  # Create a data frame
  data = data.frame(cbind(data.IL,Y,X,X.2)) 
}




#### Function to conduct the simulation-based power analysis ----

# Simulation-based power analysis

## This function uses Monte Carlo simulations for computing standard errors and statistical power. 

Power.Simulation.ML.3.VAR = function(data,T.obs,
                                     b00,b10,b11,
                                     sigma,rho,
                                     sigma.v0,sigma.v1,sigma.v2,
                                     rho.v01,rho.v02,rho.v12,
                                     mu.X,sigma.X,sigma.X.v0,rho.X,isX.center,
                                     alpha,
                                     side.test,
                                     Opt.Method){
  
  # Fit linear mixed-effects models
  
  if (Opt.Method == 'ML'){
    # Maximum Likelihood
    fit.lme = try(lme(Y ~ X + X.2, random = ~ 1 + X + X.2|subjno,correlation = corAR1(),data=data,na.action=na.omit,method='ML',
                      control=lmeControl(opt='optim')),silent = FALSE)
  }
  
  if (Opt.Method == 'REML'){
    fit.lme = try(lme(Y ~ X + X.2, random = ~ 1 + X + X.2|subjno,correlation = corAR1(),data=data,na.action=na.omit,method='REML',
                      control=lmeControl(opt='optim')),silent = FALSE)
  }
  
  if (length(fit.lme)>1){
    
    # Obtain the estimated coefficients of the model
    beta.hat.lme = coef(summary(fit.lme))[,'Value']
    
    # Obtain the standard errors
    StdError.beta.lme = coef(summary(fit.lme))[,'Std.Error']
    
    # Compute power and standard error from lme  
    
    if (side.test == 1){ # One-side test: positive
      p.value = pt(coef(summary(fit.lme))[,4], coef(summary(fit.lme))[,3], lower = FALSE)
    }
    
    if (side.test == 2){ # One-side test: negative
      p.value = pt(coef(summary(fit.lme))[,4], coef(summary(fit.lme))[,3], lower = TRUE)
      
    }
    
    if (side.test == 3){ # Two-tailed test
      p.value = 2*pt(-abs(coef(summary(fit.lme))[,4]), coef(summary(fit.lme))[,3])
    }
    
    power.hat.lme = p.value < alpha
    
    return(list(beta.hat.lme=beta.hat.lme,
                power.hat.lme=power.hat.lme,
                StdError.beta.lme=StdError.beta.lme,
                p.value=p.value))}
  
  if (length(fit.lme)==1){
    return(list(fit.lme))
  }}


# Function to conduct Monte Carlo simulation for conducting power analysis

## This function conduct Monte Carlo simulation for conducting power analysis using analytic derivation and estimation-based methods.

Power.Simulation.Estimates.ML.3.VAR = function(N,T.obs,
                                               b00,b10,b11,
                                               sigma,rho,
                                               sigma.v0,sigma.v1,sigma.v2,
                                               rho.v01,rho.v02,rho.v12,
                                               mu.X,sigma.X,sigma.X.v0,rho.X,isX.center,
                                               alpha,
                                               side.test,
                                               Opt.Method,R){
  
  tic()
  # Simulate data from the linear mixed-effects model  
  data = lapply(1:R, function(r) 
    Sim.Data.ML.3.VAR(N,T.obs,
                      b00,b10,b11,
                      sigma,rho,
                      sigma.v0,sigma.v1,sigma.v2,
                      rho.v01,rho.v02,rho.v12,
                      mu.X,sigma.X,sigma.X.v0,rho.X,isX.center))  
  
  # Simulation-based power analysis using Monte Carlo simulation
  fit.list.sim = lapply(1:R, function(r) Power.Simulation.ML.3.VAR(data[[r]],T.obs,
                                                                   b00,b10,b11,
                                                                   sigma,rho,
                                                                   sigma.v0,sigma.v1,sigma.v2,
                                                                   rho.v01,rho.v02,rho.v12,
                                                                   mu.X,sigma.X,sigma.X.v0,rho.X,isX.center,
                                                                   alpha,
                                                                   side.test,
                                                                   Opt.Method))
  toc(log = TRUE, quiet = TRUE)
  log.txt =  tic.log(format = TRUE)
  log.lst = tic.log(format = FALSE)
  tic.clearlog()
  timings.sim.power.simulation = unlist(lapply(log.lst, function(x) x$toc - x$tic))
  
  # Get a vector with the iterations that converge
  errors = rep(0,R)
  for (r in 1:R){errors[r] = length(fit.list.sim[[r]])}
  
  R.converge = which(errors>1)
  
  # Number of replicates that converge
  n.R = length(R.converge)
  
  # Estimates the fixed effects
  beta.hat.lme.list = matrix(unlist(lapply(R.converge, function(r)
    fit.list.sim[[r]]$beta.hat.lme)), 
    ncol=3, byrow=TRUE)
  colnames(beta.hat.lme.list) = c('b00','b10','b11')
  
  beta.hat.lme = colMeans(beta.hat.lme.list)
  beta.hat.lme.se = apply(beta.hat.lme.list,2,sd)/sqrt(n.R)
  
  # Power
  power.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) 
    fit.list.sim[[r]]$power.hat.lme)), 
    ncol=3, byrow=TRUE)
  colnames(power.hat.lme.list) = c('b00','b10','b11')
  
  power.hat.lme = colMeans(power.hat.lme.list)
  power.hat.lme.se = sqrt(power.hat.lme*(1-power.hat.lme))/sqrt(n.R)
  
  return(list(beta.hat.lme.list=beta.hat.lme.list,
              beta.hat.lme=beta.hat.lme,
              beta.hat.lme.se=beta.hat.lme.se,
              power.hat.lme.list=power.hat.lme.list,
              power.hat.lme=power.hat.lme,
              power.hat.lme.se=power.hat.lme.se,
              timings.sim.power.simulation=timings.sim.power.simulation,
              n.R=n.R))}




### Power Analysis ----

# Create an empty list to store the results

power.int <- data.frame()
power.ctrl <- data.frame()

#### Emotional intensity ----

##### Set values ----

rows <- (1:6) 

# row 1: relaxation
# row 2: expression
# row 3: rumination
# row 4: reappraisal 
# row 5: distraction 
# row 6: suppression 

for (row in rows) {
  
  print(Sys.time())

N = c(70,80) # Vector with the number of participants for which compute power
T.obs = 60 # Number of repeated measurement occasions for each individual

isX.center = TRUE # Person-mean center the Level 1 continuous predictor X

b00 = coefs.df.int.2[[row,"b00"]] # Fixed intercept
b10 = coefs.df.int.2[[row,"b10"]] # Fixed effect of Level 1 continuous predictor X on the outcome
b11 = coefs.df.int.2[[row,"b11"]] # Fixed effect of Level 1 continuous predictor X^2 on the outcome

sigma = coefs.df.int.2[[row,"sigma"]] # Std. deviation of the Level 1 error
rho = coefs.df.int.2[[row,"rho"]] # Autocorrelation of the Level 1 error

sigma.v0 = coefs.df.int.2[[row,"sigma.v0"]] # Std. deviation of the random intercept
sigma.v1 = coefs.df.int.2[[row,"sigma.v1"]] # Std. deviation of the random slope of predictor X
sigma.v2 = coefs.df.int.2[[row,"sigma.v2"]] # Std. deviation of the random slope of predictor X^2
rho.v01 = coefs.df.int.2[[row,"rho.v01"]] # correlation between the random intercept and the random slope of predictor X
rho.v02 = coefs.df.int.2[[row,"rho.v02"]] # correlation between the random intercept and the random slope of predictor X^2
rho.v12 = coefs.df.int.2[[row,"rho.v12"]] # correlation between the random slopes of predictor X and X^2

mu.X = coefs.df.0.X[[1,"mu.x"]] # Mean of the Level 1 continuous predictor X
sigma.X = coefs.df.0.X[[1,"sigma.x"]] # Standard deviation of the Level 1 continuous predictor X
rho.X = coefs.df.0.X[[1,"rho.x"]] # Autocorrelation of the Level 1 continuous predictor X
sigma.X.v0 = coefs.df.0.X[[1,"sigmav0.x"]] # Standard deviation of random intercept of the Level 1 continuous predictor X

alpha = 0.05 # Significant level
side.test = 3 # Two-tailed test H1: b10 different from 0
Opt.Method = 'REML' # Set the optimization method for lme 
R = 1000 # Number of Monte Carlo replicates



##### Conduct power analysis ----

set.seed(123) # Set seed or the random number generator
# Function for computing power using simulation-based approach using REML
Power.Simulation.list.REML = lapply(1:length(N), function(n){
  Power.Simulation.Estimates.ML.3.VAR(N[[n]],T.obs,
                                      b00,b10,b11,
                                      sigma,rho,
                                      sigma.v0,sigma.v1,sigma.v2,
                                      rho.v01,rho.v02,rho.v12,
                                      mu.X,sigma.X,sigma.X.v0,rho.X,isX.center,
                                      alpha,
                                      side.test,
                                      Opt.Method='REML',R)})

# Number of replicates that converge
n.converge = cbind(N, unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$n.R)))
colnames(n.converge) = c('N','N.Rep.Converged')
n.converge = data.frame(n.converge)

##### Results: Fixed effect estimates ----

# We obtained the estimates of the fixed effects when using the simulation-based approach.
# We obtain the estimates of the value of the fixed intercept.

# Create tables with estimates
# Estimates of the fixed effects
beta.b00.hat = cbind(N,
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b00'])),
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b00'])))

colnames(beta.b00.hat) = c('N','Mean.REML','SE.REML')
beta.b00.hat = data.frame(beta.b00.hat)

print(beta.b00.hat) 

# We obtain the estimates of the value of the fixed slope of variable X.

beta.b10.hat = cbind(N,
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b10'])),
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b10'])))

colnames(beta.b10.hat) = c('N','Mean.REML','SE.REML')
beta.b10.hat = data.frame(beta.b10.hat)

print(beta.b10.hat) 




# We obtain the estimates of the value of the fixed slope of variable W.

beta.b11.hat = cbind(N,
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b11'])),
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b11'])))

colnames(beta.b11.hat) = c('N','Mean.REML','SE.REML')
beta.b11.hat = data.frame(beta.b11.hat)

print(beta.b11.hat) 



##### Results: Power of the fixed effects ----
# We obtain the computed power for the fixed intercept.
# Estimate of power
## b00
power.b00.hat = cbind(N,
                      unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b00'])))
colnames(power.b00.hat) = c('N','Simulation.REML')
power.b00.hat = data.frame(power.b00.hat)

print(power.b00.hat) 




# Computed power of the value of the fixed effect of the Level 1 continuous predictor X.

## b10
power.b10.hat = cbind(N,
                      unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b10'])))
colnames(power.b10.hat) = c('N','Simulation.REML')
power.b10.hat = data.frame(power.b10.hat)

print(power.b10.hat) 



# Computed power of the value of the fixed effect of the Level 1 continuous predictor W.

## b11
power.b11.hat = cbind(N,
                      unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b11'])))
colnames(power.b11.hat) = c('N','Simulation.REML')
power.b11.hat = data.frame(power.b11.hat)

print(power.b11.hat) 




##### Summary of the computational time (in seconds) ----
# Create table with computational times
Timing.power = cbind(N,
                     unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$timings.sim.power.simulation)))
colnames(Timing.power) = c('N','Simulation.REML')
Timing.power = data.frame(Timing.power)

print(Timing.power)



##### Bind results ----
power.int <- rbind(power.int, data.frame(predictor = row,
                                 N = power.b11.hat$N,
                                 N.Rep.Converged = n.converge$N.Rep.Converged,
                                 Timing.power=Timing.power$Simulation.REML,
                                 power.X=power.b10.hat$Simulation.REML,
                                 power.X2=power.b11.hat$Simulation.REML))

power.int <- power.int %>%
  dplyr::mutate(predictor=recode(predictor,
                                 '1' = 'relaxation', 
                                 '2' = 'expression',
                                 '3' = 'rumination',
                                 '4' = 'reappraisal',
                                 '5' = 'distraction',
                                 '6' = 'suppression'))


print(Sys.time())

}

#### Emotional control ----

##### Set values ----

for (row in rows) {
  
  print(Sys.time())
  
  N = c(70,80) # Vector with the number of participants for which compute power
  T.obs = 60 # Number of repeated measurement occasions for each individual
  
  isX.center = TRUE # Person-mean center the Level 1 continuous predictor X
  
  b00 = coefs.df.ctrl.2[[row,"b00"]] # Fixed intercept
  b10 = coefs.df.ctrl.2[[row,"b10"]] # Fixed effect of Level 1 continuous predictor X on the outcome
  b11 = coefs.df.ctrl.2[[row,"b11"]] # Fixed effect of Level 1 continuous predictor X^2 on the outcome
  
  sigma = coefs.df.ctrl.2[[row,"sigma"]] # Std. deviation of the Level 1 error
  rho = coefs.df.ctrl.2[[row,"rho"]] # Autocorrelation of the Level 1 error
  
  sigma.v0 = coefs.df.ctrl.2[[row,"sigma.v0"]] # Std. deviation of the random intercept
  sigma.v1 = coefs.df.ctrl.2[[row,"sigma.v1"]] # Std. deviation of the random slope of predictor X
  sigma.v2 = coefs.df.ctrl.2[[row,"sigma.v2"]] # Std. deviation of the random slope of predictor X^2
  rho.v01 = coefs.df.ctrl.2[[row,"rho.v01"]] # correlation between the random intercept and the random slope of predictor X
  rho.v02 = coefs.df.ctrl.2[[row,"rho.v02"]] # correlation between the random intercept and the random slope of predictor X^2
  rho.v12 = coefs.df.ctrl.2[[row,"rho.v12"]] # correlation between the random slopes of predictor X and X^2
  
  mu.X = coefs.df.0.X[[2,"mu.x"]] # Mean of the Level 1 continuous predictor X
  sigma.X = coefs.df.0.X[[2,"sigma.x"]] # Standard deviation of the Level 1 continuous predictor X
  rho.X = coefs.df.0.X[[2,"rho.x"]] # Autocorrelation of the Level 1 continuous predictor X
  sigma.X.v0 = coefs.df.0.X[[2,"sigmav0.x"]] # Standard deviation of random intercept of the Level 1 continuous predictor X
  
  alpha = 0.05 # Significant level
  side.test = 3 # Two-tailed test H1: b10 different from 0
  Opt.Method = 'REML' # Set the optimization method for lme 
  R = 1000 # Number of Monte Carlo replicates
  
  
  
  ##### Conduct power analysis ----
  
  set.seed(123) # Set seed or the random number generator
  # Function for computing power using simulation-based approach using REML
  Power.Simulation.list.REML = lapply(1:length(N), function(n){
    Power.Simulation.Estimates.ML.3.VAR(N[[n]],T.obs,
                                        b00,b10,b11,
                                        sigma,rho,
                                        sigma.v0,sigma.v1,sigma.v2,
                                        rho.v01,rho.v02,rho.v12,
                                        mu.X,sigma.X,sigma.X.v0,rho.X,isX.center,
                                        alpha,
                                        side.test,
                                        Opt.Method='REML',R)})
  
  # Number of replicates that converge
  n.converge = cbind(N, unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$n.R)))
  colnames(n.converge) = c('N','N.Rep.Converged')
  n.converge = data.frame(n.converge)
  
  
  ##### Results: Fixed effect estimates ----
  
  # We obtained the estimates of the fixed effects when using the simulation-based approach.
  # We obtain the estimates of the value of the fixed intercept.
  
  # Create tables with estimates
  # Estimates of the fixed effects
  beta.b00.hat = cbind(N,
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b00'])),
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b00'])))
  
  colnames(beta.b00.hat) = c('N','Mean.REML','SE.REML')
  beta.b00.hat = data.frame(beta.b00.hat)
  
  print(beta.b00.hat) 
  
  # We obtain the estimates of the value of the fixed slope of variable X.
  
  beta.b10.hat = cbind(N,
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b10'])),
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b10'])))
  
  colnames(beta.b10.hat) = c('N','Mean.REML','SE.REML')
  beta.b10.hat = data.frame(beta.b10.hat)
  
  print(beta.b10.hat) 
  
  
  
  # We obtain the estimates of the value of the fixed slope of variable W.
  
  beta.b11.hat = cbind(N,
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme['b11'])),
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$beta.hat.lme.se['b11'])))
  
  colnames(beta.b11.hat) = c('N','Mean.REML','SE.REML')
  beta.b11.hat = data.frame(beta.b11.hat)
  
  print(beta.b11.hat) 
  
  
  
  ##### Results: Power of the fixed effects ----
  # We obtain the computed power for the fixed intercept.
  # Estimate of power
  ## b00
  power.b00.hat = cbind(N,
                        unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b00'])))
  colnames(power.b00.hat) = c('N','Simulation.REML')
  power.b00.hat = data.frame(power.b00.hat)
  
  print(power.b00.hat)
  

  
  
  # Computed power of the value of the fixed effect of the Level 1 continuous predictor X.
  
  ## b10
  power.b10.hat = cbind(N,
                        unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b10'])))
  colnames(power.b10.hat) = c('N','Simulation.REML')
  power.b10.hat = data.frame(power.b10.hat)
  
  print(power.b10.hat) 
  

  
  
  # Computed power of the value of the fixed effect of the Level 1 continuous predictor W.
  
  ## b11
  power.b11.hat = cbind(N,
                        unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$power.hat.lme['b11'])))
  colnames(power.b11.hat) = c('N','Simulation.REML')
  power.b11.hat = data.frame(power.b11.hat)
  
  print(power.b11.hat) 
  

  
  
  
  ##### Summary of the computational time (in seconds) ----
  # Create table with computational times
  Timing.power = cbind(N,
                       unlist(lapply(1:length(N), function(n) Power.Simulation.list.REML[[n]]$timings.sim.power.simulation)))
  colnames(Timing.power) = c('N','Simulation.REML')
  Timing.power = data.frame(Timing.power)
  
  print(Timing.power) 
  
  
  
  ##### Bind results ----
  power.ctrl <- rbind(power.ctrl, data.frame(predictor = row,
                                             N = power.b11.hat$N,
                                             N.Rep.Converged = n.converge$N.Rep.Converged,
                                             Timing.power=Timing.power$Simulation.REML,
                                             power.X=power.b10.hat$Simulation.REML,
                                             power.X2=power.b11.hat$Simulation.REML))
  
  power.ctrl <- power.ctrl %>%
    dplyr::mutate(predictor=recode(predictor,
                                   '1' = 'relaxation', 
                                   '2' = 'expression',
                                   '3' = 'rumination',
                                   '4' = 'reappraisal',
                                   '5' = 'distraction',
                                   '6' = 'suppression'))
  
  print(Sys.time())
  
}

#### Summary of all results ----

power.int %>% 
  mutate_if(is.numeric, round, digits = 3)

power.ctrl %>% 
  mutate_if(is.numeric, round, digits = 3)

write_xlsx(power.int, here::here("preregistration", "power_emo_intensity.xlsx"))
write_xlsx(power.ctrl, here::here("preregistration", "power_emo_control.xlsx"))

## 2. Power Analyses moderation MLM ----

# I used the shiny app by Lafit et al (2021) to run the simulation study for the moderation effects.
# Analyses were done in the app, so the script and results are not here.
# I only ran two model combinations (intensity & relaxation and control & reappraisal with perceived stress as moderator)
# Essentially, the analyses clearly showed that I have insufficient power to detect a cross-level interaction (power was .04 and .32, respectively),
# which is why I did not even test for the other model combinations.

