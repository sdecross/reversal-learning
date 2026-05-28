# mixed-effects-models.R
# Written by Stephanie DeCross, March 2023
# Updated March 2026

# Models investigating if trauma predicts neural response to New CS+ and New CS-
# and testing findings for any interactions with age


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())
require(pacman)
p_load("lmerTest", "stats", "tidyverse")

# load demogs
load("data/RL_n100_demogs_psy.RData")
demogs <- df %>%
  rename (inc_needs = inc_needs_dm)

# load roi data
orig_acc_bl <- read.table("data/ACC.txt", header=T)
orig_amyg_lh <- read.table("data/lh-amyg.txt", header=T)
orig_amyg_rh<- read.table("data/rh-amyg.txt", header=T)
orig_caudate_bl <- read.table("data/caudate.txt", header=T)
orig_insula_bl <- read.table("data/insula.txt", header=T)
orig_vmpfc_bl <- read.table("data/vmPFC.txt", header=T)
orig_phg_bl <- read.table("data/PHG.txt", header=T)
orig_hipp_lh <- read.table("data/lh-hipp.txt", header=T)
orig_hipp_rh<- read.table("data/rh-hipp.txt", header=T)

roilist <- list(orig_acc_bl, orig_amyg_lh, orig_amyg_rh, orig_caudate_bl, orig_insula_bl, orig_vmpfc_bl, orig_phg_bl, orig_hipp_lh, orig_hipp_rh)
names(roilist) <- c("acc_bl", "amyg_lh", "amyg_rh", "caudate_bl", "insula_bl", "vmpfc_bl", "phg_bl", "hipp_lh", "hipp_rh")

roilist_modeling <- list() 

for (roi in seq_along(roilist)){
  roilist_modeling[[roi]] <- roilist[[roi]] %>%
    rename(subject = Subject) %>%
    dplyr::select(subject, 
                  NewCSP = B,
                  NewCSM = A) %>%
    gather(key = "stim", value = "response", 2:3) %>% 
    mutate(stimulus = if_else(substr(stim, 1, 6) == "NewCSM", "New CS-", "New CS+")) %>%
    left_join(demogs, by = "subject") %>%
    mutate(subject = as.factor(subject)) %>%
    dplyr::select(subject, trauma, stim, response, stimulus, inc_needs) 
  roilist_modeling[[roi]]$stimulus <- factor(roilist_modeling[[roi]]$stimulus, levels = c("New CS+", "New CS-")) 
}

names(roilist_modeling) <- c("acc_bl", "amyg_lh", "amyg_rh", "caudate_bl", "insula_bl", "vmpfc_bl", "phg_bl", "hipp_lh", "hipp_rh")

acc_bl <- roilist_modeling$acc_bl
amyg_lh <- roilist_modeling$amyg_lh
amyg_rh <- roilist_modeling$amyg_rh
caudate_bl <- roilist_modeling$caudate_bl
insula_bl <- roilist_modeling$insula_bl
vmpfc_bl <- roilist_modeling$vmpfc_bl
phg_bl <- roilist_modeling$phg_bl
hipp_lh <- roilist_modeling$hipp_lh
hipp_rh <- roilist_modeling$hipp_rh


# MODELS ---------------------------------------------------------------------------------

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = acc_bl) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = acc_bl) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = acc_bl)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = acc_bl, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.532917

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = amyg_lh) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = amyg_lh) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = amyg_lh)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = amyg_lh, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.09389

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = amyg_rh) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = amyg_rh) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = amyg_rh)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = amyg_rh, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.0149

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = caudate_bl) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = caudate_bl) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = caudate_bl)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = caudate_bl, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2)
anova(m2, type = "II") #0.655768

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = insula_bl) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = insula_bl) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = insula_bl)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = insula_bl, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2, m3) 
summary(m2)
anova(m2, type = "II") #0.37857

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = vmpfc_bl) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = vmpfc_bl) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = vmpfc_bl)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = vmpfc_bl, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.0003996

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = phg_bl)
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = phg_bl) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = phg_bl)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = phg_bl, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.01299

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = hipp_lh) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = hipp_lh) 
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = hipp_lh)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = hipp_lh, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.01233

m1 <- lm(response ~ stimulus*trauma + inc_needs, data = hipp_rh) 
m2 <- lmer(response ~ 1 + (1|subject) + stimulus*trauma + inc_needs, data = hipp_rh)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = hipp_rh)
m3 <- lmer(response ~ 1 + (1 + stimulus|subject) + stimulus*trauma + inc_needs, data = hipp_rh, control = lmerControl(check.nobs.vs.nRE = "ignore"))
AIC(m1, m2) 
summary(m2) 
anova(m2, type = "II") #0.01486

pvals_SAL <- c(0.532917,0.09389, 0.0149,0.37857) #acc, amyg lh, amyg rh, insula
round(stats::p.adjust(pvals_SAL, "fdr"), 6)
#0.532917 0.187780 0.059600 0.504760

pvals_DMN <- c(0.0003996,0.01299,0.01233,0.01486) #vmpfc, phg, hipp lh, hipp rh
round(stats::p.adjust(pvals_DMN, "fdr"), 6)
#0.001598 0.014860 0.014860 0.014860

# stim x group interaction,  corrected:
# amyg rh p=0.059600
# vmpfc p=0.001598
# phg p=0.014860
# hipp lh p=0.014860
# hipp rh p=0.014860


# RESHAPE DATA FOR AGE INTERACTIONS -----------------------------------------------------

rm(list=ls())
require(pacman)
p_load("lmerTest", "stats", "tidyverse")

# load demogs
load("data/RL_n100_demogs_psy.RData")
demogs <- df %>%
  rename (inc_needs = inc_needs_dm)

# load roi data
orig_acc_bl <- read.table("data/ACC.txt", header=T)
orig_amyg_lh <- read.table("data/lh-amyg.txt", header=T)
orig_amyg_rh<- read.table("data/rh-amyg.txt", header=T)
orig_caudate_bl <- read.table("data/caudate.txt", header=T)
orig_insula_bl <- read.table("data/insula.txt", header=T)
orig_vmpfc_bl <- read.table("data/vmPFC.txt", header=T)
orig_phg_bl <- read.table("data/PHG.txt", header=T)
orig_hipp_lh <- read.table("data/lh-hipp.txt", header=T)
orig_hipp_rh<- read.table("data/rh-hipp.txt", header=T)

roilist <- list(orig_acc_bl, orig_amyg_lh, orig_amyg_rh, orig_caudate_bl, orig_insula_bl, orig_vmpfc_bl, orig_phg_bl, orig_hipp_lh, orig_hipp_rh)
names(roilist) <- c("acc_bl", "amyg_lh", "amyg_rh", "caudate_bl", "insula_bl", "vmpfc_bl", "phg_bl", "hipp_lh", "hipp_rh")

roilist_modeling <- list() 

for (roi in seq_along(roilist)){
  roilist_modeling[[roi]] <- roilist[[roi]] %>%
    rename(subject = Subject) %>%
    dplyr::select(subject, 
                  NewCSP = B,
                  NewCSM = A) %>%
    gather(key = "stim", value = "response", 2:3) %>% 
    mutate(stimulus = if_else(substr(stim, 1, 6) == "NewCSM", "New CS-", "New CS+")) %>%
    left_join(demogs, by = "subject") %>%
    mutate(subject = as.factor(subject)) %>%
    dplyr::select(subject, trauma, stim, response, stimulus, inc_needs) 
  roilist_modeling[[roi]]$stimulus <- factor(roilist_modeling[[roi]]$stimulus, levels = c("New CS+", "New CS-")) 
}

names(roilist_modeling) <- c("acc_bl", "amyg_lh", "amyg_rh", "caudate_bl", "insula_bl", "vmpfc_bl", "phg_bl", "hipp_lh", "hipp_rh")

acc_bl <- roilist_modeling$acc_bl
amyg_lh <- roilist_modeling$amyg_lh
amyg_rh <- roilist_modeling$amyg_rh
caudate_bl <- roilist_modeling$caudate_bl
insula_bl <- roilist_modeling$insula_bl
vmpfc_bl <- roilist_modeling$vmpfc_bl
phg_bl <- roilist_modeling$phg_bl
hipp_lh <- roilist_modeling$hipp_lh
hipp_rh <- roilist_modeling$hipp_rh

load("data/RL_n100_demogs_psy.RData")
demogs$subject <- as.factor(demogs$subject)
demogs <- demogs %>% dplyr::select(subject, age, sex)

vmpfc_bl <- vmpfc_bl %>% left_join(demogs, by = "subject")
hipp_lh <- hipp_lh %>% left_join(demogs, by = "subject")
hipp_rh <- hipp_rh %>% left_join(demogs, by = "subject")
phg_bl <- phg_bl %>% left_join(demogs, by = "subject")
amyg_rh <- amyg_rh %>% left_join(demogs, by = "subject")

amyg_rh2 <- amyg_rh %>%
  dplyr::select(-stimulus) %>%
  pivot_wider(
    names_from = stim,
    values_from = response,
    id_cols = c(subject, trauma, age, sex, inc_needs)
  ) %>%
  mutate(NewCSPvCSM = NewCSP - NewCSM) %>%
  mutate(NewCSMvCSP = NewCSM - NewCSP)

vmpfc_bl2 <- vmpfc_bl %>%
  dplyr::select(-stimulus) %>%
  pivot_wider(
    names_from = stim,
    values_from = response,
    id_cols = c(subject, trauma, age, sex, inc_needs)
  ) %>%
  mutate(NewCSPvCSM = NewCSP - NewCSM) %>%
  mutate(NewCSMvCSP = NewCSM - NewCSP)

hipp_lh2 <- hipp_lh %>%
  dplyr::select(-stimulus) %>%
  pivot_wider(
    names_from = stim,
    values_from = response,
    id_cols = c(subject, trauma, age, sex, inc_needs)
  ) %>%
  mutate(NewCSPvCSM = NewCSP - NewCSM) %>%
  mutate(NewCSMvCSP = NewCSM - NewCSP)

hipp_rh2 <- hipp_rh %>%
  dplyr::select(-stimulus) %>%
  pivot_wider(
    names_from = stim,
    values_from = response,
    id_cols = c(subject, trauma, age, sex, inc_needs)
  ) %>%
  mutate(NewCSPvCSM = NewCSP - NewCSM) %>%
  mutate(NewCSMvCSP = NewCSM - NewCSP)

phg_bl2 <- phg_bl %>%
  dplyr::select(-stimulus) %>%
  pivot_wider(
    names_from = stim,
    values_from = response,
    id_cols = c(subject, trauma, age, sex, inc_needs)
  ) %>%
  mutate(NewCSPvCSM = NewCSP - NewCSM) %>%
  mutate(NewCSMvCSP = NewCSM - NewCSP)


# AGE INTERACTIONS -------------------------------------------------------

# Findings: 
# Trauma had less discrim to New CS->New CS+ in: vmpfc, lh hipp, rh hipp, phg
# Trauma had more response to New CS+>New CS- in: rh amyg
# Testing these findings for associations with age

p_load(sjPlot)

# amyg_rh2
agemod <- lm(NewCSPvCSM ~ trauma*age + inc_needs, data = amyg_rh2)
summary(agemod) # no
plot_model(agemod, type = "pred", terms = c("trauma", "age"))

# vmpfc bl
agemod <- lm(NewCSMvCSP ~ trauma*age + inc_needs, data = vmpfc_bl2)
summary(agemod) # no
plot_model(agemod, type = "pred", terms = c("trauma", "age"))

# hipp lh
agemod <- lm(NewCSMvCSP ~ trauma*age + inc_needs, data = hipp_lh2)
summary(agemod) # nothing
plot_model(agemod, type = "pred", terms = c("trauma", "age"))

# hipp rh
agemod <- lm(NewCSMvCSP ~ trauma*age + inc_needs, data = hipp_rh2)
summary(agemod) # no
plot_model(agemod, type = "pred", terms = c("trauma", "age"))

# phg bl
agemod <- lm(NewCSMvCSP ~ trauma*age + inc_needs, data = phg_bl2)
summary(agemod) # nothing
plot_model(agemod, type = "pred", terms = c("trauma", "age"))



