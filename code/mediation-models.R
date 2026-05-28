# mediation-models.R
# Written by Stephanie DeCross, March 2023

# Models testing whether trauma is indirectly linked with psy through RL neural measures,
# testing models where a and b branches are p<.1 after correction 


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())

# source packages and functions
require(pacman)
p_load("mediation", "tidyverse")

load("data/RL_n100_demogs_psy.RData")
df <- df %>% rename (inc_needs = inc_needs_dm)

load("data/differentials.RData")
diffdf <- diffdf[,-2] 
diffdf$subject <- as.character(diffdf$subject) 
diffdf$subject <- as.numeric(diffdf$subject) 
df <- df %>% left_join(diffdf, by = "subject")
str(df)


# MODELS ----------------------------------------------------------------------

# trauma >> phg >> gad (ACME p=.009)
set.seed(999)
xm <- lm(phg ~ trauma + inc_needs, data = df)  
xmy <- lm(scared_gad_fu ~ phg + trauma + inc_needs, data = df) 
m1 <- mediate(xm, xmy, treat = "trauma", mediator = "phg", 
              sims = 10000, boot = TRUE, boot.ci.type = "bca")    
summary(m1) 
plot(m1)

# Estimate 95% CI Lower 95% CI Upper p-value   
# ACME              -0.661       -1.501        -0.16  0.0088 **
# ADE                1.088       -1.108         3.26  0.3354   
# Total Effect       0.427       -1.958         2.58  0.6986   
# Prop. Mediated    -1.550    -1841.471        -0.14  0.7034 


# trauma >> hipp lh >> gad (ACME p=.01)
set.seed(999)
xm <- lm(hipplh ~ trauma + inc_needs, data = df)  
xmy <- lm(scared_gad_fu ~ hipplh + trauma + inc_needs, data = df) 
m1 <- mediate(xm, xmy, treat = "trauma", mediator = "hipplh", 
              sims = 10000, boot = TRUE, boot.ci.type = "bca")    
summary(m1) 
plot(m1)

#                 Estimate 95% CI Lower 95% CI Upper p-value  
# ACME             -0.831       -1.787        -0.21   0.012 *
# ADE               1.258       -0.977         3.38   0.271  
# Total Effect      0.427       -1.958         2.58   0.699  
# Prop. Mediated   -1.949     -847.226         0.67   0.709  


# trauma >> hipp rh >> gad (ACME p=.02)
set.seed(999)
xm <- lm(hipprh ~ trauma + inc_needs, data = df)  
xmy <- lm(scared_gad_fu ~ hipprh + trauma + inc_needs, data = df) 
m1 <- mediate(xm, xmy, treat = "trauma", mediator = "hipprh", 
              sims = 10000, boot = TRUE, boot.ci.type = "bca")    
summary(m1) 
plot(m1)

#                   Estimate 95% CI Lower 95% CI Upper p-value  
# ACME             -0.663       -1.641        -0.13   0.019 *
# ADE               1.090       -1.263         3.38   0.368  
# Total Effect      0.427       -1.958         2.58   0.699  
# Prop. Mediated   -1.555       -2.215        20.34   0.707 


# trauma >> hipp rh >> panic (ACME p=.02)
set.seed(999)
xm <- lm(hipprh ~ trauma + inc_needs, data = df)  
xmy <- lm(scared_panic_fu ~ hipprh + trauma + inc_needs, data = df) 
m1 <- mediate(xm, xmy, treat = "trauma", mediator = "hipprh", 
              sims = 10000, boot = TRUE, boot.ci.type = "bca")    
summary(m1) 
plot(m1)

# Estimate 95% CI Lower 95% CI Upper p-value   
# ACME             -0.708       -1.851        -0.15  0.0174 * 
#   ADE               2.773        0.802         5.19  0.0072 **
#   Total Effect      2.065        0.230         4.18  0.0288 * 
#   Prop. Mediated   -0.343      -18.477        -0.15  0.0462 * 
  

# trauma >> hipp rh >> ptsd
set.seed(999)
xm <- lm(hipprh ~ trauma + inc_needs, data = df)  
xmy <- lm(ptsd_fu ~ hipprh + trauma + inc_needs, data = df) 
m1 <- mediate(xm, xmy, treat = "trauma", mediator = "hipprh", 
              sims = 10000, boot = TRUE, boot.ci.type = "bca")    
summary(m1) 
plot(m1)

# Estimate 95% CI Lower 95% CI Upper p-value    
# ACME            -0.7719      -3.1311         0.42    0.28    
# ADE             19.9695      14.4558        25.55  <2e-16 ***
#   Total Effect    19.1976      14.0409        24.33  <2e-16 ***
#   Prop. Mediated  -0.0402      -0.1700         0.02    0.28 

 

 