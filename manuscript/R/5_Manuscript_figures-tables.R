# ---------------------------
# Script name: 5. Manuscript - Tables and Figures
# Description: Script for creating Tables and Figures
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Setup ----
source("manuscript/R/1_Manuscript_setup.R")

here::i_am("manuscript/R/5_Manuscript_figures-tables.R") #Set location of script

data_evening <- readRDS("manuscript/R/output/data_evening_clean.rds") #Load data
all_estimates <- readRDS("manuscript/R/output/all_estimates.rds")
fits.n <- readRDS("manuscript/R/output/fits.n.rds")
results_n_er_anova <- readRDS("manuscript/R/output/results_n_er_anova.rds")
names <- readRDS("manuscript/R/output/names.rds")
pred_quad_models <- readRDS("manuscript/R/output/pred_quad_models.rds")

# Figures ----

## Violin Figure ----

### Functions ----

# Summary for violin plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# GGplot for violin plot
ggplot_vio <- function(data, labels, plot_title) {
  ggplot(data = data, aes(x = variable, y = value, fill = variable)) +
    geom_violin() +
    stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
    scale_x_discrete(labels = labels) +
    ylab("Intensity") +
    xlab("") +
    ggtitle(plot_title) +
    ylim(0, 10) +
    papaja::theme_apa()   +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, hjust = .5),
          legend.position = "none") +
    stat_summary(fun.data = data_summary)
}

# Violin plots of ESM variables 

# reshape: Emotion, Negative ER and Positive ER
data_graph <- data_evening %>%
  select(c("participant.ID",all_of(ESM_variables)))

data_graph_em_long <- data_graph %>% 
  pivot_longer(cols = 2:3, names_to = "variable", values_to = "value")

data_graph_er_long <- data_graph %>% 
  pivot_longer(cols = 4:9, names_to = "variable", values_to = "value")

# Sort so that boxplot is ordered correctly
data_graph_em_long$variable <- factor(data_graph_em_long$variable, c(ESM_variables[1:2]))
data_graph_er_long$variable <- factor(data_graph_er_long$variable, c(ESM_variables[3:9]))

# Boxplot Variables using Function (defined at top)
vio_em <- ggplot_vio(data_graph_em_long, ESM_names[1:2], plot_title = "Emotion characteristics") 
vio_er <- ggplot_vio(data_graph_er_long, ESM_names[3:9], plot_title = "Emotion regulation strategies") 

vio <- ggarrange(vio_em,vio_er, 
                 ncol = 1, nrow = 2,
                 widths = 10, heights = 20)

ggsave("manuscript/vio_apa.png", vio, width = 6, height = 7.5)


## Multilevel Figure ----
## Extract predicted values

### Negative emotion intensity
data_evening$pred.n.int.rel <- predict(fits.n$n.em.int.c.n.er.rel)
data_evening$pred.n.int.eng <- predict(fits.n$n.em.int.c.n.er.eng)
data_evening$pred.n.int.rum <- predict(fits.n$n.em.int.c.n.er.rum)
data_evening$pred.n.int.reap <- predict(fits.n$n.em.int.c.n.er.reap)
data_evening$pred.n.int.dis <- predict(fits.n$n.em.int.c.n.er.dis)
data_evening$pred.n.int.sup <- predict(fits.n$n.em.int.c.n.er.sup)
data_evening$pred.n.int.dbc.swi <- predict(fits.n$n.em.int.c.BrayCurtisRepl.amm)
#data_evening$pred.n.int.dbc.end <- predict(fits.n$n.em.int.c.BrayCurtisNest.amm)

### Negative emotion Controllability
data_evening$pred.n.cont.rel <- predict(fits.n$n.em.cont.c.n.er.rel)
data_evening$pred.n.cont.eng <- predict(fits.n$n.em.cont.c.n.er.eng)
data_evening$pred.n.cont.rum <- predict(fits.n$n.em.cont.c.n.er.rum)
data_evening$pred.n.cont.reap <- predict(fits.n$n.em.cont.c.n.er.reap)
data_evening$pred.n.cont.dis <- predict(fits.n$n.em.cont.c.n.er.dis)
data_evening$pred.n.cont.sup <- predict(fits.n$n.em.cont.c.n.er.sup)
data_evening$pred.n.cont.dbc.swi <- predict(fits.n$n.em.cont.c.BrayCurtisRepl.amm)
#data_evening$pred.n.cont.dbc.end <- predict(fits.n$n.em.cont.c.BrayCurtisNest.amm)

## Make ggplots
# Functions

## GGPlot for linear multilevel model - negative emotion intensity - significant
ggplot_mlm_lin_n_int <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    geom_smooth(aes(group=1), method=lm, formula = y ~ x, se=TRUE, fullrange=FALSE, lty=1, linewidth=2, color="blue") +
    xlab("Negative emotion intensity") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)  }

## GGPlot for quadratic multilevel model - negative emotion intensity
ggplot_mlm_qua_n_int <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x + I(x^2), se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    geom_smooth(aes(group=1), method=lm, formula = y ~ x + I(x^2), se=TRUE, fullrange=FALSE, lty=1, linewidth=2, color="blue") +
    xlab("Negative emotion intensity") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)}

## GGPlot for multilevel model - negative emotion intensity - non-significant
ggplot_mlm_n_int_ns <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x + I(x^2), se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    #  geom_smooth(aes(group=1), method=lm, formula = y ~ x, se=TRUE, fullrange=FALSE, lty=2, linewidth=0.5, color="black") +
    xlab("Negative emotion intensity") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)}

## GGPlot for linear multilevel model - negative emotion controllability
ggplot_mlm_lin_n_cont <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    geom_smooth(aes(group=1), method=lm, formula = y ~ x, se=TRUE, fullrange=FALSE, lty=1, linewidth=2, color="blue") +
    xlab("Negative emotion controllability") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)}

## GGPlot for quadratic multilevel model - negative emotion controllability
ggplot_mlm_qua_n_cont <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x + I(x^2), se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    geom_smooth(aes(group=1), method=lm, formula = y ~ x + I(x^2), se=TRUE, fullrange=FALSE, lty=1, linewidth=2, color="blue") +
    xlab("Negative emotion controllability") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)}

## GGPlot for linear multilevel model - negative emotion controllability - non-significant
ggplot_mlm_n_cont_ns <- function(x, y,ylab) {
  ggplot(data=data_evening, aes(x=x, y=y, group=factor(participant.ID), colour="gray"), legend=FALSE) +
    geom_smooth(method=lm, formula = y ~ x + I(x^2), se=FALSE, fullrange=FALSE, lty=1, linewidth=.5, color="gray40") +
    #  geom_smooth(aes(group=1), method=lm, formula = y ~ x, se=TRUE, fullrange=FALSE, lty=2, linewidth=0.5, color="black") +
    xlab("Negative emotion controllability") + ylab(ylab) +
    papaja::theme_apa() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    ylim(0, 10) +
    xlim(-8, 8)}


# For models, with significant linear, but not significant quadratic effects, I am using a linear function for fitting the regression line

### Negative emotion intensity
plot.n.int.rel <- ggplot_mlm_lin_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.rel,"Relaxation")
plot.n.int.eng <- ggplot_mlm_lin_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.eng,"Expression")
plot.n.int.rum <- ggplot_mlm_lin_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.rum,"Rumination")
plot.n.int.reap <- ggplot_mlm_qua_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.reap,"Reappraisal")
plot.n.int.dis <- ggplot_mlm_n_int_ns(data_evening$n.em.int.c,data_evening$pred.n.int.dis,"Distraction")
plot.n.int.sup <- ggplot_mlm_qua_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.sup,"Suppression")
plot.n.int.dbc.swi <- ggplot_mlm_n_int_ns(data_evening$n.em.int.c,data_evening$pred.n.int.dbc.swi,"Strategy Switching")
#plot.n.int.dbc.end <- ggplot_mlm_qua_n_int(data_evening$n.em.int.c,data_evening$pred.n.int.dbc.end,"ER Endorsement change")

### Negative emotion Controllability
plot.n.cont.rel <- ggplot_mlm_n_cont_ns(data_evening$n.em.cont.c,data_evening$pred.n.cont.rel,"Relaxation")
plot.n.cont.eng <- ggplot_mlm_lin_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.eng,"Expression")
plot.n.cont.rum <- ggplot_mlm_qua_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.rum,"Rumination")
plot.n.cont.reap <- ggplot_mlm_qua_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.reap,"Reappraisal")
plot.n.cont.dis <- ggplot_mlm_n_cont_ns(data_evening$n.em.cont.c,data_evening$pred.n.cont.dis,"Distraction")
plot.n.cont.sup <- ggplot_mlm_qua_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.sup,"Suppression")
plot.n.cont.dbc.swi <- ggplot_mlm_qua_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.dbc.swi,"Strategy Switching")
#plot.n.cont.dbc.end <- ggplot_mlm_qua_n_cont(data_evening$n.em.cont.c,data_evening$pred.n.cont.dbc.end,"ER Endorsement Change")

## Arrange in one figure
mlm.plots.er.int <- ggarrange(plot.n.int.rel + theme(axis.title.x = element_blank()),
                              plot.n.int.eng + theme(axis.title.x = element_blank()),
                              plot.n.int.rum + theme(axis.title.x = element_blank()), 
                              plot.n.int.reap + theme(axis.title.x = element_blank()), 
                              plot.n.int.dis + theme(axis.title.x = element_blank()), 
                              plot.n.int.sup + theme(axis.title.x = element_blank()), 
                              plot.n.int.dbc.swi, 
                              #                              plot.n.int.dbc.end,
                              nrow = 4, ncol = 2,
                              widths = 10, heights = 10)

mlm.plots.er.cont <- ggarrange(plot.n.cont.rel + theme(axis.title.x = element_blank()),
                               plot.n.cont.eng + theme(axis.title.x = element_blank()),
                               plot.n.cont.rum + theme(axis.title.x = element_blank()), 
                               plot.n.cont.reap + theme(axis.title.x = element_blank()), 
                               plot.n.cont.dis + theme(axis.title.x = element_blank()),  
                               plot.n.cont.sup + theme(axis.title.x = element_blank()),
                               plot.n.cont.dbc.swi, 
                               #                               plot.n.cont.dbc.end,
                               nrow = 4, ncol = 2,
                               widths = 10, heights = 10)

ggsave("manuscript/mlm_plots_er_int.png", mlm.plots.er.int, width = 15, height = 15)
ggsave("manuscript/mlm_plots_er_cont.png", mlm.plots.er.cont, width = 15, height = 15)

# Tables ----
## Functions ----

### Bolding ----
bold_if <- function(x, sig) {
  ifelse(sig, paste0("\\textbf{", x, "}"), x)
}

### Format rows (bolding)
format_cols <- function(df) {
  df %>%
    mutate(
      est = bold_if(est_fmt, sig),
      SE  = bold_if(SE_fmt, sig),
      p   = bold_if(p_adj_fmt, sig)
    )
}

### Rounding estimates to 2 decimals ----
fmt_num <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

### Rounding p-values to 3 decimals without leading 0 and make everythign smaller <.001 into <.001 for table ----
fmt_p <- function(x) {
  ifelse(
    is.na(x),
    NA,
    ifelse(x < .001, "< .001", sub("^0", "", sprintf("%.3f", x)))
  )
}

## Table for multilevel results ----

### Format values ----
all_estimates <- all_estimates %>%
  mutate(
    sig = p.adj < 0.05,
    est_fmt = fmt_num(est, 2),
    SE_fmt = fmt_num(SE, 2),
    p_adj_fmt = fmt_p(p.adj)
  )

### Coefficients Negative Intensity ----
coefs.n.int.lin.adj <- all_estimates %>%
  slice(1:7) %>%
  dplyr::rename(c("lin.est"="est",
                  "lin.se"="SE", 
                  "lin.p.adj"="p.adj")) %>% 
  format_cols() %>%
  mutate(outcome = names$n.outcome.names[1:7]) %>%
  select(outcome, est, SE, p)

coefs.n.int.qua.adj <- all_estimates %>%
  slice(17:23) %>%
  dplyr::rename(c("qua.est"="est",
                  "qua.se"="SE", 
                  "qua.p.adj"="p.adj")) %>% 
  format_cols() %>%
  select(est, SE, p)

### Coefficients Negative controllability ----
coefs.n.cont.lin.adj <- all_estimates %>%
  slice(9:15) %>%
  dplyr::rename(c("lin.est"="est",
                  "lin.se"="SE", 
                  "lin.p.adj"="p.adj"))%>% 
  format_cols() %>%
  select(est, SE, p)

coefs.n.cont.qua.adj <- all_estimates %>%
  slice(25:31) %>%
  dplyr::rename(c("qua.est"="est",
                  "qua.se"="SE", 
                  "qua.p.adj"="p.adj"))%>% 
  format_cols() %>%
  select(est, SE, p)

### Bind intensity and controllability coefficients
coefs.n.adj <-cbind(coefs.n.int.lin.adj,coefs.n.int.qua.adj,coefs.n.cont.lin.adj,coefs.n.cont.qua.adj) 

### Rename rows
coefs.n.adj[ ,1] <- names$n.outcome.names[c(1:7)]

### Rename columns
colnames(coefs.n.adj) <- names$mlm.columns

### Coefficients Interaction ----
coefs.mod.adj <- all_estimates %>%
  slice(33:38) %>% 
  select("outcome", "est_fmt", "SE_fmt", "p_adj_fmt") 

# no significant interaction
sig_mod_adj<-min(coefs.mod.adj$p_adj_fmt)

# Rename rows
coefs.mod.adj[ ,1] <- names$n.outcome.names[1:6]

# Rename columns
colnames(coefs.mod.adj) <- names$mlm.columns[1:4]

## AIC/BIC table ----

### Format values ----
aic_bic_table <- results_n_er_anova %>%
  mutate(
    AIC_model1_ftm = fmt_num(AIC_model1, 2),
    AIC_model2_ftm = fmt_num(AIC_model2, 2),
    AIC_delta_ftm = fmt_num(delta_AIC, 2),
    BIC_model1_ftm = fmt_num(BIC_model1, 2),
    BIC_model2_ftm = fmt_num(BIC_model2, 2),
    BIC_delta_ftm = fmt_num(delta_BIC, 2),
    l_ratio_ftm = fmt_num(l_ratio, 2),
    p_value_ftm = fmt_p(p_value)
  )  %>%
  select("predictor", "AIC_model1_ftm":"p_value_ftm")


# Column names for ANOVA
anova.columns <- c("ER strategies","AIC linear", "AIC quadratic", "$\\Delta$ AIC", "BIC linear", "BIC quadratic", "$\\Delta$ BIC", "Likelihood ratio", "$p$")

# Rename rows
aic_bic_table[ ,1] <- names$n.outcome.names

# Rename columns
colnames(aic_bic_table) <- anova.columns

## Coefficients Bray Curtis Endorsement Change only ----
coefs.n.end.lin.adj <- all_estimates %>%
  slice(c(8, 16)) %>%
  format_cols() %>%
  mutate(Predictor = c("Negative emotion intensity",
                       "Negative emotion controllability")) %>%
  select(Predictor, est, SE, p)

coefs.n.end.qua.adj <- all_estimates %>%
  slice(c(24, 32)) %>%
  format_cols() %>%
  select(est, SE, p)

coefs.n.end.adj <- cbind(coefs.n.end.lin.adj, coefs.n.end.qua.adj)

### Rename columns
colnames(coefs.n.end.adj) <- names$mlm.columns[1:7]

## Predicted values quadratic models ----

### Format values
pred_quad_table <- pred_quad_models %>%
  mutate(low_fmt = fmt_num(Low),
         mean_fmt = fmt_num(Mean),
         high_fmt = fmt_num(High)) %>%
  select(model, low_fmt, mean_fmt, high_fmt)

### Rename columns
colnames(pred_quad_table) <- c("Model (Predictor - Outcome)", "Low (-2 SD)", "Mean", "High (+2 SD)")

# Save processed files and outputs ----

coefs.mod.adj <- list(
  coefs.mod.adj=coefs.mod.adj,
  sig_mod_adj=sig_mod_adj)

saveRDS(coefs.n.adj,  "manuscript/R/output/coefs.n.adj.rds")
saveRDS(coefs.mod.adj,  "manuscript/R/output/coefs.mod.adj.rds")
saveRDS(aic_bic_table,  "manuscript/R/output/aic_bic_table.rds")
saveRDS(coefs.n.end.adj, "manuscript/R/output/coefs.n.end.adj.rds")
saveRDS(pred_quad_table, "manuscript/R/output/pred_quad_table.rds")

