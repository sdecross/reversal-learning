# gPPI-analyses.R
# Written by Stephanie DeCross, April 2023
# Updated March 2026

# All gPPI-related models, including: models testing associations of 
# gPPI in a priori ROIs with trauma, models testing associations of
# gPPI with psy, and any mediation models meeting criteria for testing
# and testing findings for interactions with age


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())
require(pacman)
p_load("sjPlot", "stats", "MASS", "mediation", "tidyverse")

load("data/RL_n100_demogs_psy.RData")

amyg_blhipp <- read.table("data/gPPI-rh-amyg-hipp.txt", header=FALSE)       
amyg_blhipp <- amyg_blhipp %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
amyg_blhipp$subject <- factor(amyg_blhipp$subject)
str(amyg_blhipp)

amyg_blacc <- read.table("data/gPPI-rh-amyg-ACC.txt", header=FALSE)       
amyg_blacc <- amyg_blacc %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
amyg_blacc$subject <- factor(amyg_blacc$subject)
str(amyg_blacc)

amyg_blvmpfc <- read.table("data/gPPI-rh-amyg-vmPFC.txt", header=FALSE)       
amyg_blvmpfc <- amyg_blvmpfc %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
amyg_blvmpfc$subject <- factor(amyg_blvmpfc$subject)
str(amyg_blvmpfc)

hipp_blamyg <- read.table("data/gPPI-rh-hipp-amyg.txt", header=FALSE)       
hipp_blamyg <- hipp_blamyg %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
hipp_blamyg$subject <- factor(hipp_blamyg$subject)
str(hipp_blamyg)

hipp_blacc <- read.table("data/gPPI-rh-hipp-ACC.txt", header=FALSE)       
hipp_blacc <- hipp_blacc %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
hipp_blacc$subject <- factor(hipp_blacc$subject)
str(hipp_blacc)

hipp_blvmpfc <- read.table("data/gPPI-rh-hipp-vmPFC.txt", header=FALSE)       
hipp_blvmpfc <- hipp_blvmpfc %>%
  dplyr::select(subject = V1, gPPI_BvA = V2, gPPI_B = V3, gPPI_A = V4) %>%
  left_join(df, by = "subject") %>%
  rename(inc_needs = inc_needs_dm)
hipp_blvmpfc$subject <- factor(hipp_blvmpfc$subject)
str(hipp_blvmpfc)


# VISUALIZING gPPI --------------------------------------------------------------

hist(amyg_blhipp$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 
hist(amyg_blacc$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 
hist(amyg_blvmpfc$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 
hist(hipp_blamyg$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 
hist(hipp_blacc$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 
hist(hipp_blvmpfc$gPPI_BvA, breaks = 20, xlab = "gPPI", main = "Histogram") 

ggstatsplot::ggbetweenstats(data = amyg_blhipp, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, amygrh-hipp") 
ggstatsplot::ggbetweenstats(data = amyg_blacc, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, amygrh-ACC") 
ggstatsplot::ggbetweenstats(data = amyg_blvmpfc, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, amygrh-vmPFC") 
ggstatsplot::ggbetweenstats(data = hipp_blamyg, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, hipprh-amyg") 
ggstatsplot::ggbetweenstats(data = hipp_blacc, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, hipprh-ACC") 
ggstatsplot::ggbetweenstats(data = hipp_blvmpfc, x = trauma, y = gPPI_BvA, messages = FALSE, xlab = "Group", ylab = "gPPI", title = "Control v Trauma, hipprh-vmPFC") 


# A PRIORI ROI ANALYSES ------------------------------------------------------------

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = amyg_blhipp)
summary(model) 

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = amyg_blacc)
summary(model) 

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = amyg_blvmpfc)
summary(model) 

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = hipp_blamyg)
summary(model) 

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = hipp_blacc)
summary(model) 

model <- lm(gPPI_BvA ~ trauma + inc_needs, data = hipp_blvmpfc)
summary(model) 


# gPPI-PSY ASSOCIATIONS ------------------------------------------------------------

# amyg_blhipp ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blhipp)
AIC(m1, m2) 
summary(m2)#no


# amyg_blacc ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.152

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.285

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.346

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.1896

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.49249

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blacc)
AIC(m1, m2) 
summary(m2) #0.0939

# FDR correction: cdi, panic, gad, ext, ptsd, cape
pvals <- c(0.152,0.285,0.346,0.1896,0.49249,0.0939)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.37920 0.41520 0.41520 0.37920 0.49249 0.37920

# amyg_blvmpfc ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.4424

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.339

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.609

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.1063

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.65504

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = amyg_blvmpfc)
AIC(m1, m2) 
summary(m2) #0.0784

# FDR correction: cdi, panic, gad, ext, ptsd, cape
pvals <- c(0.4424,0.339,0.609,0.1063, 0.65504,0.0784)
round(stats::p.adjust(pvals, "fdr"), 6)
# 0.65504 0.65504 0.65504 0.31890 0.65504 0.31890

# hipp_blamyg ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.109

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.877

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.492

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.7221

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.00225

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blamyg)
AIC(m1, m2) 
summary(m2) #0.844

# FDR correction: cdi, panic, gad, ext, ptsd, cape
pvals <- c(0.109,0.877,0.492,0.7221,0.00225,0.844)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.3270 0.8770 0.8770 0.8770 0.0135 0.8770

ggplot(hipp_blamyg, aes(x=gPPI_BvA, y=ptsd_fu)) + 
  geom_point( size=2.5, colour="gray")+
  geom_smooth(method=lm, color="black") +
  theme_classic() +
  labs(title = "Association of right hippocampus-amygdala\nfunctional connectivity and PTSD symptoms",
       x = "gPPI, New CS+ > New CS-",
       y = "PTSD symptoms") +
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +#center title
  geom_point(shape=1, size=2.5, colour="black") +
  theme(title=element_text(size=14, colour="black"))

# hipp_blacc ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) #0.6592

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) #0.137

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) #0.385

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) #0.2593

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) #0.44806

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blacc)
AIC(m1, m2) 
summary(m2) # 0.0754

# FDR correction: cdi, panic, gad, ext, ptsd, cape
pvals <- c(0.6592,0.137,0.385,0.2593,0.44806,0.0754)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.659200 0.411000 0.537672 0.518600 0.537672 0.411000

# hipp_blvmpfc ----------------------------------------------------------------------

# cdi
m1 <- lm(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(cdi_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no

# scared_panic
m1 <- lm(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(scared_panic_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no

# scared_gad
m1 <- lm(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(scared_gad_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no

# external
m1 <- lm(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(external_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no

# ptsd
m1 <- lm(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(ptsd_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no

# cape
m1 <- lm(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
m2 <- glm.nb(cape_fu ~ gPPI_BvA + inc_needs, data = hipp_blvmpfc)
AIC(m1, m2) 
summary(m2) #no


# MEDIATION MODELS ---------------------------------------------------------

# No models meet criteria for testing 


# AGE INTERACTIONS ---------------------------------------------------------

# No findings in gPPI a priori ROI analyses; checking for interactions with age
# for all ROIs

# rh hipp and bl amyg 
hist(hipp_blamyg$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = hipp_blamyg)
summary(agemod) # no

# rh hipp and acc
hist(hipp_blacc$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = hipp_blacc)
summary(agemod) # no

# rh hipp and vmpfc
hist(hipp_blvmpfc$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = hipp_blvmpfc)
summary(agemod) # no

# rh amyg and hipp
hist(amyg_blhipp$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = amyg_blhipp)
summary(agemod) # no

# rh amyg and acc
hist(amyg_blacc$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = amyg_blacc)
summary(agemod) # no

# rh amyg and vmpfc
hist(amyg_blvmpfc$gPPI_BvA) 
agemod <- lm(gPPI_BvA ~ trauma*age +  inc_needs, data = amyg_blvmpfc)
summary(agemod) # no



