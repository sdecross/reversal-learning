# fMRI-to-psy.R
# Written by Stephanie DeCross, January 2023

# Regressions testing whether RL neural activation predicts psy


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())

# source packages and functions
require(pacman)
p_load("haven", "ltm", "tidyverse")

# load demogs
load("data/RL_n100_demogs_psy.RData")
df <- df %>%
  rename (inc_needs = inc_needs_dm)

load("data/differentials.RData")
diffdf <- diffdf[,-2] 
diffdf$subject <- as.character(diffdf$subject) 
str(diffdf)
diffdf$subject <- as.numeric(diffdf$subject) 
str(diffdf)
str(df)

df <- df %>% left_join(diffdf, by = "subject")
dfptsd <- df %>% dplyr::filter(trauma=="trauma") 


# HISTOGRAMS ---------------------------------------------------------------------

hist(df$scared_panic_fu)       # right skew
hist(df$scared_gad_fu)         # right skew
hist(dfptsd$ptsd_fu)           # normal-ish
hist(df$cdi_fu)                # right skew
hist(df$external_fu)           # normal-ish
hist(df$cape_fu)               # right skew


# REGRESSIONS --------------------------------------------------------------------

# a note about regressions: these differentials are CS+ > CS-
# for safety regions, betas are reported with the opposite sign because that is the
# sign they are when differentials are calculated as CS- > CS+, and all plots and
# analyses are designed, interpreted, and discussed in this safety-learning way 
# throughout the rest of the manuscript

# acc ----------------------------------------------------------------------------

# scared_panic_fu
plot(df$acc, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ acc + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ acc + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.242

# scared_gad_fu
plot(df$acc, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ acc + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ acc + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.422

# ptsd_fu
plot(dfptsd$acc, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ acc + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ acc + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.429

# cdi_fu
plot(df$acc, df$cdi_fu)
modcdi <- lm(cdi_fu ~ acc + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ acc + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.2421

# external_fu
plot(df$acc, df$external_fu)
modexternal <- lm(external_fu ~ acc + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ acc + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.4639

# cape_fu
plot(df$acc, df$cape_fu)
modcape <- lm(cape_fu ~ acc + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ acc + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.767

# amyglh -------------------------------------------------------------------------

# scared_panic_fu
plot(df$amyglh, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ amyglh + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ amyglh + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.137

# scared_gad_fu
plot(df$amyglh, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ amyglh + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ amyglh + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.0466

# ptsd_fu
plot(dfptsd$amyglh, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ amyglh + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ amyglh + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0406

# cdi_fu
plot(df$amyglh, df$cdi_fu)
modcdi <- lm(cdi_fu ~ amyglh + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ amyglh + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.1429

# external_fu
plot(df$amyglh, df$external_fu)
modexternal <- lm(external_fu ~ amyglh + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ amyglh + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.3166

# cape_fu
plot(df$amyglh, df$cape_fu)
modcape <- lm(cape_fu ~ amyglh + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ amyglh + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.580

# amyglh FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c(0.137, 0.0466, 0.0406, 0.1429, 0.3166 ,0.580 )
round(stats::p.adjust(pvals, "fdr"), 6)
#0.21435 0.13980 0.13980 0.21435 0.37992 0.58000

# amygrh -------------------------------------------------------------------------

# scared_panic_fu
plot(df$amygrh, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ amygrh + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ amygrh + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.0853

# scared_gad_fu
plot(df$amygrh, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ amygrh + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ amygrh + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.242

# ptsd_fu
plot(dfptsd$amygrh, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ amygrh + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ amygrh + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0842

# cdi_fu
plot(df$amygrh, df$cdi_fu)
modcdi <- lm(cdi_fu ~ amygrh + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ amygrh + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.2504

# external_fu
plot(df$amygrh, df$external_fu)
modexternal <- lm(external_fu ~ amygrh + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ amygrh + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.900

# cape_fu
plot(df$amygrh, df$cape_fu)
modcape <- lm(cape_fu ~ amygrh + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ amygrh + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.316

# amygrh FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.0853, 0.242, 0.0842, 0.2504,0.900,0.316 )
round(stats::p.adjust(pvals, "fdr"), 6)
#0.2559 0.3756 0.2559 0.3756 0.9000 0.3792

# caudate ------------------------------------------------------------------------

# scared_panic_fu
plot(df$caudate, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ caudate + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ caudate + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.753

# scared_gad_fu
plot(df$caudate, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ caudate + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ caudate + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.676

# ptsd_fu
plot(dfptsd$caudate, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ caudate + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ caudate + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0577

# cdi_fu
plot(df$caudate, df$cdi_fu)
modcdi <- lm(cdi_fu ~ caudate + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ caudate + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.4250

# external_fu
plot(df$caudate, df$external_fu)
modexternal <- lm(external_fu ~ caudate + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ caudate + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.6501

# cape_fu
plot(df$caudate, df$cape_fu)
modcape <- lm(cape_fu ~ caudate + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ caudate + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.193

# caudate FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.753, 0.676, 0.0577, 0.4250,0.6501,0.193 )
round(stats::p.adjust(pvals, "fdr"), 6)
#0.7530 0.7530 0.3462 0.7530 0.7530 0.5790

# insula -------------------------------------------------------------------------

# scared_panic_fu
plot(df$insula, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ insula + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ insula + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.180

# scared_gad_fu
plot(df$insula, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ insula + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ insula + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.259

# ptsd_fu
plot(dfptsd$insula, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ insula + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ insula + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.762

# cdi_fu
plot(df$insula, df$cdi_fu)
modcdi <- lm(cdi_fu ~ insula + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ insula + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb)# 0.0242

# external_fu
plot(df$insula, df$external_fu)
modexternal <- lm(external_fu ~ insula + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ insula + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.1978

# cape_fu
plot(df$insula, df$cape_fu)
modcape <- lm(cape_fu ~ insula + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ insula + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb)#0.953

# insula FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.180, 0.259, 0.762, 0.0242,0.1978, 0.953)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.3885 0.3885 0.9144 0.1452 0.3885 0.9530
 
# vmpfc --------------------------------------------------------------------------

# scared_panic_fu
plot(df$vmpfc, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ vmpfc + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ vmpfc + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.215

# scared_gad_fu
plot(df$vmpfc, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ vmpfc + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ vmpfc + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.0883

# ptsd_fu
plot(dfptsd$vmpfc, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ vmpfc + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ vmpfc + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0901

# cdi_fu
plot(df$vmpfc, df$cdi_fu)
modcdi <- lm(cdi_fu ~ vmpfc + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ vmpfc + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.5187

# external_fu
plot(df$vmpfc, df$external_fu)
modexternal <- lm(external_fu ~ vmpfc + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ vmpfc + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.288

# cape_fu
plot(df$vmpfc, df$cape_fu)
modcape <- lm(cape_fu ~ vmpfc + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ vmpfc + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb)#0.757

# vmpfc FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.215, 0.0883,0.0901, 0.5187, 0.288, 0.757)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.43000 0.27030 0.27030 0.62244 0.43200 0.75700

# phg ----------------------------------------------------------------------------

# scared_panic_fu
plot(df$phg, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ phg + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ phg + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.0794

# scared_gad_fu
plot(df$phg, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ phg + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ phg + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.00809

# ptsd_fu
plot(dfptsd$phg, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ phg + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ phg + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0852

# cdi_fu
plot(df$phg, df$cdi_fu)
modcdi <- lm(cdi_fu ~ phg + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ phg + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.2225

# external_fu
plot(df$phg, df$external_fu)
modexternal <- lm(external_fu ~ phg + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ phg + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.743

# cape_fu
plot(df$phg, df$cape_fu)
modcape <- lm(cape_fu ~ phg + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ phg + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.842

# phg FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c(0.0794, 0.00809, 0.0852, 0.2225, 0.743,0.842 )
round(stats::p.adjust(pvals, "fdr"), 6)
#0.17040 0.04854 0.17040 0.33375 0.84200 0.84200

# hipplh -------------------------------------------------------------------------

# scared_panic_fu
plot(df$hipplh, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ hipplh + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ hipplh + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.0598

# scared_gad_fu
plot(df$hipplh, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ hipplh + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ hipplh + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.00381

# ptsd_fu
plot(dfptsd$hipplh, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ hipplh + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ hipplh + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0689

# cdi_fu
plot(df$hipplh, df$cdi_fu)
modcdi <- lm(cdi_fu ~ hipplh + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ hipplh + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.103

# external_fu
plot(df$hipplh, df$external_fu)
modexternal <- lm(external_fu ~ hipplh + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ hipplh + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.3543

# cape_fu
plot(df$hipplh, df$cape_fu)
modcape <- lm(cape_fu ~ hipplh + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ hipplh + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.491

# hipplh FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.0598, 0.00381, 0.0689, 0.103, 0.3543,0.491)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.13780 0.02286 0.13780 0.15450 0.42516 0.49100

# hipprh -------------------------------------------------------------------------

# scared_panic_fu
plot(df$hipprh, df$scared_panic_fu)
modpanic <- lm(scared_panic_fu ~ hipprh + inc_needs, data = df)
modpanicnb <- glm.nb(scared_panic_fu ~ hipprh + inc_needs, data = df)
AIC(modpanic, modpanicnb)
summary(modpanicnb) #0.0351

# scared_gad_fu
plot(df$hipprh, df$scared_gad_fu)
modgad <- lm(scared_gad_fu ~ hipprh + inc_needs, data = df)
modgadnb <- glm.nb(scared_gad_fu ~ hipprh + inc_needs, data = df)
AIC(modgad, modgadnb)
summary(modgadnb) #0.022

# ptsd_fu
plot(dfptsd$hipprh, dfptsd$ptsd_fu)
modptsd <- lm(ptsd_fu ~ hipprh + inc_needs, data = dfptsd)
modptsdnb <- glm.nb(ptsd_fu ~ hipprh + inc_needs, data = dfptsd)
AIC(modptsd, modptsdnb)
summary(modptsd) #0.0268

# cdi_fu
plot(df$hipprh, df$cdi_fu)
modcdi <- lm(cdi_fu ~ hipprh + inc_needs, data = df)
modcdinb <- glm.nb(cdi_fu ~ hipprh + inc_needs, data = df)
AIC(modcdi, modcdinb)
summary(modcdinb) #0.0716

# external_fu
plot(df$hipprh, df$external_fu)
modexternal <- lm(external_fu ~ hipprh + inc_needs, data = df)
modexternalnb <- glm.nb(external_fu ~ hipprh + inc_needs, data = df)
AIC(modexternal, modexternalnb)
summary(modexternalnb) #0.8901

# cape_fu
plot(df$hipprh, df$cape_fu)
modcape <- lm(cape_fu ~ hipprh + inc_needs, data = df)
modcapenb <- glm.nb(cape_fu ~ hipprh + inc_needs, data = df)
AIC(modcape, modcapenb)
summary(modcapenb) #0.178

# hipprh FDR correction
# panic, gad, ptsd, cdi, external, cape
pvals <- c( 0.0351, 0.022, 0.0268, 0.0716,0.8901, 0.178)
round(stats::p.adjust(pvals, "fdr"), 6)
#0.0702 0.0702 0.0702 0.1074 0.8901 0.2136

# removed ptsd finding and subsequent exploratory analysis from final published 
# manuscript at reviewer request due to corrected p-value being a trend; justified 
# retention of panic and gad trends due to importance in interpreting significant 
# mediation models


# EXPLORATORY --------------------------------------------------------------------

p_load("reghelper")

# phg and gad
df <- df %>% mutate(phgSS = phg*-1) # multiplied by -1 to flip CS+ > CS- to CS- > CS+ for safety-signaling
ggplot(data = df, aes(x=phgSS, y=scared_gad_fu, color=trauma)) +
  theme_classic() +
  geom_point(size=2.5) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Association of differential activation in\nPHG and generalized anxiety symptoms",
       x = "PHG, New CS- > New CS+",
       y = "Generalized anxiety symptoms") +
  scale_color_manual(name = "Group", values = c("control" = "#FFFFFF", "trauma" = "#000000"), labels=c("Control", "Trauma")) + 
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_point(shape = 1,size = 2.5,colour = "black") +
  theme(title=element_text(size=14, colour="black")) +
  theme(legend.position="none") 

#ggsave(filename=paste("plots/", "phg_gad.pdf", sep=""), 
#       width=5, height=5, units="in", dpi=600)

mod2 <- glm.nb(scared_gad_fu ~ phg*trauma + inc_needs, data = df)
summary(mod2) 
simple_slopes(mod2)
#4    sstest control       -0.1550     0.1511 -1.0261 95 0.304836     
#5    sstest  trauma       -0.3615     0.1263 -2.8628 95 0.004199   **

# lh hipp and gad
df <- df %>% mutate(hipplhSS = hipplh*-1)
ggplot(data = df, aes(x=hipplhSS, y=scared_gad_fu, color=trauma)) +
  theme_classic() +
  geom_point(size=2.5) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Association of differential activation in left\nhippocampus and generalized anxiety symptoms",
       x = "Left hippocampus, New CS- > New CS+",
       y = "Generalized anxiety symptoms") +
  scale_color_manual(name = "Group", values = c("control" = "#FFFFFF", "trauma" = "#000000"), labels=c("Control", "Trauma")) + 
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_point(shape = 1,size = 2.5,colour = "black") +
  theme(title=element_text(size=14, colour="black")) +
  theme(legend.position="none") 

#ggsave(filename=paste("plots/", "hipplh_gad.pdf", sep=""), 
#       width=5, height=5, units="in", dpi=600)

mod1 <- glm.nb(scared_gad_fu ~ hipplh*trauma + inc_needs, data = df)
summary(mod1) 
simple_slopes(mod1)
#4    sstest control       -0.1688     0.1511 -1.1171 95 0.263968     
#5    sstest  trauma       -0.3664     0.1215 -3.0165 95 0.002557   **

# rh hipp and panic
df <- df %>% mutate(hipprhSS = hipprh*-1)
ggplot(data = df, aes(x=hipprhSS, y=scared_panic_fu, color=trauma)) +
  theme_classic() +
  geom_point(size=2.5) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Association of differential activation in\nright hippocampus and panic symptoms",
       x = "Right hippocampus, New CS- > New CS+",
       y = "Panic symptoms") +
  scale_color_manual(name = "Group", values = c("control" = "#FFFFFF", "trauma" = "#000000"), labels=c("Control", "Trauma")) + 
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_point(shape = 1,size = 2.5,colour = "black") +
  theme(title=element_text(size=14, colour="black")) +
  theme(legend.position="none") 

#ggsave(filename=paste("plots/", "hipprh_panic.pdf", sep=""), 
#       width=5, height=5, units="in", dpi=600)

mod3 <- glm.nb(scared_panic_fu ~ hipprh*trauma + inc_needs, data = df)
summary(mod3) 
simple_slopes(mod3) # trauma sig; control not
#     rhhipp  trauma      Test Estimate   Std. Error    t value   df    Pr(>|t|) Sig.
#4    sstest control       -0.1655        0.1856        -0.8914   95    0.372725     
#5    sstest  trauma       -0.6038        0.2107        -2.8654   95    0.004164   **

# rh hipp and gad
ggplot(data = df, aes(x=hipprhSS, y=scared_gad_fu, color=trauma)) +
  theme_classic() +
  geom_point(size=2.5) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Association of differential activation in right\nhippocampus and generalized anxiety symptoms",
       x = "Right hippocampus, New CS- > New CS+",
       y = "Generalized anxiety symptoms") +
  scale_color_manual(name = "Group", values = c("control" = "#FFFFFF", "trauma" = "#000000"), labels=c("Control", "Trauma")) +  
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_point(shape = 1,size = 2.5,colour = "black") +
  theme(title=element_text(size=14, colour="black")) +
  theme(legend.position="none") 

#ggsave(filename=paste("plots/", "hipprh_gad.pdf", sep=""), 
#       width=5, height=5, units="in", dpi=600)

mod4 <- glm.nb(scared_gad_fu ~ hipprh*trauma + inc_needs, data = df)
summary(mod4) # hipprh x trauma p=0.0605 
simple_slopes(mod4) # trauma sig; control not
#4    sstest control       -0.0968     0.1463 -0.6614 95 0.508340     
#5    sstest  trauma       -0.5238     0.1749 -2.9943 95 0.002751   **

# rh hipp and ptsd
ggplot(data = df, aes(x=hipprhSS, y=ptsd_fu, color=trauma)) +
  theme_classic() +
  geom_point(size=2.5) +
  geom_smooth(method="lm", se=T) +
  labs(title = "Association of differential activation in\nright hippocampus and PTSD symptoms",
       x = "Right hippocampus, New CS- > New CS+",
       y = "PTSD symptoms") +
  scale_color_manual(name = "Group", values = c("control" = "#FFFFFF", "trauma" = "#000000"), labels=c("Control", "Trauma")) +  
  theme(axis.text.x=element_text(size=12, colour="black")) +
  theme(axis.text.y=element_text(size=12, colour="black")) +
  theme(axis.title.x=element_text(size=14, colour="black")) +
  theme(axis.title.y=element_text(size=14, colour="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_point(shape = 1,size = 2.5,colour = "black") +
  theme(title=element_text(size=14, colour="black")) +
  theme(legend.position="none") 

#ggsave(filename=paste("plots/, "hipprh_ptsd.pdf", sep=""), 
#       width=5, height=5, units="in", dpi=600)

mod5 <- lm(ptsd_fu ~ hipprh*trauma + inc_needs, data = df)
summary(mod5) # hipprh x trauma p=0.00406 ** 
simple_slopes(mod5) 
#4    sstest control        1.7910     1.9191  0.9333 95  0.353053     
#5    sstest  trauma       -7.0258     2.3054 -3.0476 95  0.002987   **

# removed ptsd finding and subsequent exploratory analysis from final published 
# manuscript at reviewer request due to corrected p-value being a trend; justified 
# retention of panic and gad trends due to importance in interpreting significant 
# mediation models



