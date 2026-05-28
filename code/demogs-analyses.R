# demogs-analyses.R
# Written by Stephanie DeCross, January 2023

# Descriptives and statistics related to demographics and psy for sociodem table


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())

require(pacman)
p_load("tidyverse")


load("data/RL_n100_demogs_psy.RData")
data <- df
str(data)
# trauma:0=control, 1=trauma
# sex: 0=male, 1=female
# race: 1=white, 2=black, 3=latino, 4=asian, 5=biracial/multiracial
# white: white=1, nonwhite=2


# COMPUTE -----------------------------------------------------------------------

# trauma
n <- data %>% 
  group_by(trauma) %>%
  summarise(Count = n())
n

#age
hist(data$age, breaks = 20, xlab = "Age", main = "Histogram")
min(data$age)
max(data$age)
mean(data$age)
sd(data$age)
df1 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(data$age, data$trauma, mean),
                  n=tapply(data$age, data$trauma, length),
                  sd=tapply(data$age, data$trauma, sd))
df1
t.test(age ~ trauma, data = data) 

# sex 
with(data, table(trauma, sex))               
sex_test <- table(data$trauma, data$sex) 
chisq.test(sex_test, correct=F)

# race 
with(data, table(trauma, race)) 
race_test <- table(data$trauma, data$race) 
chisq.test(race_test)

# inc_needs
df2 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(data$inc_needs_dm, data$trauma, mean),
                  n=tapply(data$inc_needs_dm, data$trauma, length),
                  sd=tapply(data$inc_needs_dm, data$trauma, sd))
df2
t.test(inc_needs_dm ~ trauma, data = data) 


# PSY ------------------------------------------------------------------------------

rm(list=ls())

# source packages and functions
require(pacman)
p_load("haven", "ltm", "tidyverse")

load("data/RL_n100_demogs_psy.RData")
str(df)

# HISTOGRAMS -----------------------------------------------------------------------

hist(df$scared_panic_fu)       
hist(df$scared_gad_fu)         
hist(df$ptsd_fu)               
hist(df$cdi_fu)                
hist(df$external_fu)          
hist(df$cape_fu)               

# ptsd
hist(df$ptsd_fu, breaks = 20, xlab = "ptsd_fu", main = "Histogram")
dfrm <- df %>%
  filter(!is.na(ptsd_fu))
df3 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(dfrm$ptsd_fu, dfrm$trauma, mean),
                  n=tapply(dfrm$ptsd_fu, dfrm$trauma, length),
                  sd=tapply(dfrm$ptsd_fu, dfrm$trauma, sd))
df3
t.test(ptsd_fu ~ trauma, data = dfrm) 

# cdi
hist(df$cdi_fu, breaks = 20, xlab = "cdi_fu", main = "Histogram")
dfrm <- df %>%
  filter(!is.na(cdi_fu))
df3 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(dfrm$cdi_fu, dfrm$trauma, mean),
                  n=tapply(dfrm$cdi_fu, dfrm$trauma, length),
                  sd=tapply(dfrm$cdi_fu, dfrm$trauma, sd))
df3
t.test(cdi_fu ~ trauma, data = dfrm) 

# external
hist(df$external_fu, breaks = 20, xlab = "external_fu", main = "Histogram")
dfrm <- df %>%
  filter(!is.na(external_fu))
df3 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(dfrm$external_fu, dfrm$trauma, mean),
                  n=tapply(dfrm$external_fu, dfrm$trauma, length),
                  sd=tapply(dfrm$external_fu, dfrm$trauma, sd))
df3
t.test(external_fu ~ trauma, data = dfrm) 

# cape
hist(df$cape_fu, breaks = 20, xlab = "cape_fu", main = "Histogram")
dfrm <- df %>%
  filter(!is.na(cape_fu))
df3 <- data.frame(trauma=c('control','trauma'),
                  mean=tapply(dfrm$cape_fu, dfrm$trauma, mean),
                  n=tapply(dfrm$cape_fu, dfrm$trauma, length),
                  sd=tapply(dfrm$cape_fu, dfrm$trauma, sd))
df3
t.test(cape_fu ~ trauma, data = dfrm) 

