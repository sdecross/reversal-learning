# trauma-to-psy.R
# Written by Stephanie DeCross, January 2023

# Associations of trauma with psy


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())

# source packages and functions
require(pacman)
p_load("haven", "ltm", "tidyverse")

load("data/RL_n100_demogs_psy.RData")
str(df)
df <- df %>%
  rename(inc_needs = inc_needs_dm)
str(df)  


# HISTOGRAMS -----------------------------------------------------------------------

hist(df$scared_panic_fu)       # right skew
hist(df$scared_gad_fu)         # right skew
hist(df$ptsd_fu)               # right skew
hist(df$cdi_fu)                # right skew
hist(df$external_fu)           # normal-ish
hist(df$cape_fu)               # right skew


# REGRESSIONS --------------------------------------------------------------------

# scared_panic
plot(df$trauma, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ trauma + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ trauma + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) # 0.0299

# scared_gad
plot(df$trauma, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ trauma + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ trauma + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) # 0.734

# ptsd
plot(df$trauma, df$ptsd_fu)
modptsd <- lm(ptsd_fu ~ trauma + inc_needs, data = df)
modptsdnb <- glm.nb(ptsd_fu ~ trauma  + inc_needs, data = df)
AIC(modptsd, modptsdnb)
summary(modptsdnb) # <2e-16

# cdi
plot(df$trauma, df$cdi_fu)
modcdi <- lm(cdi_fu ~ trauma + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ trauma + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) # 0.0297

# external
plot(df$trauma, df$external_fu)
modexternal <- lm(external_fu ~ trauma  + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ trauma  + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) # 4.17e-06

# cape
plot(df$trauma, df$cape_fu)
modcape <- lm(cape_fu ~ trauma  + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ trauma  + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) # 0.00988

# FDR correction
#         panic,   gad,    ptsd,  cdi,    external,   cape
pvals <- c(0.0299, 0.734,  2e-16, 0.0297, 4.17e-06,   0.00988)
round(stats::p.adjust(pvals, "fdr"), 6)
#      0.035880 0.734000 0.000000 0.035880 0.000013 0.019760



