# ---------------------------
# Script name: 6. Manuscript - Supplementary analyses
# Description: Script for supplementary analyses
# Author: Dr. Dominique Maciejewski
#
# Date Created: 2025-04-10
#
# Copyright (c) Dominique Maciejewski
# Email: d.f.maciejewski@tilburguniversity.edu
#

# Setup ----

# load data and objects from manuscript
source("manuscript/R/1_Manuscript_setup.R")

here::i_am("manuscript/R/6_Manuscript_supplementary.R") #Set location of script

data_evening <- readRDS("manuscript/R/output/data_evening_clean.rds")

# Analyses positive emotion regulation ----

## Centering
data_evening$p.em.int.c<-calc.mcent(p.em.int, participant.ID, data = data_evening)

## Multilevel models

# Savoring
fit.p.er.sav.mlm <- lme(p.er.sav ~ 1 + p.em.int.c + I(p.em.int.c^2) + Age.c + gender.dum + micro + obs.evening,
                        random= ~ 1 + p.em.int.c + I(p.em.int.c^2) | participant.ID,
                        correlation = corAR1(),
                        data = data_evening, 
                        na.action = na.exclude, 
                        method = 'REML',
                        control = lmeControl(opt = 'optim'))

lin.est.sav <- coef(summary(fit.p.er.sav.mlm))[2, 1] 
lin.se.sav <- coef(summary(fit.p.er.sav.mlm))[2, 2] 
lin.p.sav <- coef(summary(fit.p.er.sav.mlm))[2, 5] 
qua.est.sav <- coef(summary(fit.p.er.sav.mlm))[3, 1] 
qua.se.sav <- coef(summary(fit.p.er.sav.mlm))[3, 2] 
qua.p.sav <- coef(summary(fit.p.er.sav.mlm))[3, 5] 

coefs.p.sav <- rbind(data.frame(outcome = "savoring", 
                                lin.est = lin.est.sav, 
                                lin.se = lin.se.sav, 
                                lin.p = lin.p.sav,
                                qua.est = qua.est.sav, 
                                qua.se = qua.se.sav, 
                                qua.p = qua.p.sav))

# Expression
fit.p.er.eng.mlm <- lme(p.er.eng ~ 1 + p.em.int.c + I(p.em.int.c^2) + Age.c + gender.dum + micro + obs.evening,
                        random= ~ 1 + p.em.int.c + I(p.em.int.c^2) | participant.ID,
                        correlation = corAR1(),
                        data = data_evening, 
                        na.action = na.exclude, 
                        method = 'REML',
                        control = lmeControl(opt = 'optim'))

lin.est.eng <- coef(summary(fit.p.er.eng.mlm))[2, 1] 
lin.se.eng <- coef(summary(fit.p.er.eng.mlm))[2, 2] 
lin.p.eng <- coef(summary(fit.p.er.eng.mlm))[2, 5] 
qua.est.eng <- coef(summary(fit.p.er.eng.mlm))[3, 1] 
qua.se.eng <- coef(summary(fit.p.er.eng.mlm))[3, 2] 
qua.p.eng <- coef(summary(fit.p.er.eng.mlm))[3, 5] 

coefs.p.eng <- rbind(data.frame(outcome = "expression", 
                                lin.est = lin.est.eng, 
                                lin.se = lin.se.eng, 
                                lin.p = lin.p.eng,
                                qua.est = qua.est.eng, 
                                qua.se = qua.se.eng, 
                                qua.p = qua.p.eng))

coefs.p <- rbind(coefs.p.sav,coefs.p.eng)

## Multiple Testing Adjustment ----------------------
## Transpose all dataframes, so that each effect is in one row

coefs.p.lin <- coefs.p %>%
  dplyr::select(c(1:4))

coefs.p.qua <- coefs.p %>%
  dplyr::select(c(1,5:7))

colnames(coefs.p.lin)<-names$names_est
colnames(coefs.p.qua)<-names$names_est

## Bind all coefficients 
all_estimates_pos <- rbind(coefs.p.lin,coefs.p.qua)

all_estimates_pos$p.adj<-p.adjust(all_estimates_pos$p, method = "holm")

## Coefficients Positive Intensity ----------------------
coefs.p.lin.adj <- all_estimates_pos %>%
  slice(1:2)  %>%
  dplyr::rename(c("lin.est"="est",
                  "lin.se"="SE", 
                  "lin.p.adj"="p.adj"))%>% 
  select(-4) 

coefs.p.qua.adj <- all_estimates_pos %>%
  slice(3:4) %>%
  dplyr::rename(c("qua.est"="est",
                  "qua.se"="SE", 
                  "qua.p.adj"="p.adj"))%>% 
  select(-c(1,4))

coefs.p.adj <-cbind(coefs.p.lin.adj,coefs.p.qua.adj) 

# Assign estimates to objects for printing in text
lin.eng.est<-coefs.p.adj$lin.est[1]
lin.eng.se<-coefs.p.adj$lin.se[1]
lin.eng.p<-coefs.p.adj$lin.p[1]

qua.eng.est<-coefs.p.adj$qua.est[1]
qua.eng.se<-coefs.p.adj$qua.se[1]
qua.eng.p<-coefs.p.adj$qua.p[1]

lin.sav.est<-coefs.p.adj$lin.est[2]
lin.sav.se<-coefs.p.adj$lin.se[2]
lin.sav.p<-coefs.p.adj$lin.p[2]

qua.sav.est<-coefs.p.adj$qua.est[2]
qua.sav.se<-coefs.p.adj$qua.se[2]
qua.sav.p<-coefs.p.adj$qua.p[2]


# 

# Save processed files and outputs ----
coefs.p.adj <- list(
  lin.eng.est=lin.eng.est,
  lin.eng.se=lin.eng.se,
  lin.eng.p=lin.eng.p,
  qua.eng.est=qua.eng.est,
  qua.eng.se=qua.eng.se,
  qua.eng.p=qua.eng.p,
  
  lin.sav.est=lin.sav.est,
  lin.sav.se=lin.sav.se,
  lin.sav.p=lin.sav.p,
  qua.sav.est=qua.sav.est,
  qua.sav.se=qua.sav.se,
  qua.sav.p=qua.sav.p
)

saveRDS(coefs.p.adj, "manuscript/R/output/coefs.p.adj.rds")
