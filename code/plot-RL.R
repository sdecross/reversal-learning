# plot-RL.R
# Written by Stephanie DeCross, June 2023

# Generate plots for RL in ROIs: rh amyg, lh amyg, ACC, insula, caudate, rh hipp, lh hipp, PHG, vmPFC


# SOURCE & LOAD DATA -----------------------------------------------------------

rm(list=ls())

# source packages and functions
require(pacman)
p_load("data.table", "forcats", "tidyverse")
source("code/plot-RL-functions.R")

# load trauma grouping
subjectgroups <- read.csv("data/RL_n100_trauma.csv")
subjectgroups[ , "trauma"] <- factor(subjectgroups[ , "trauma"]) # convert to factor

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

for (roi in seq_along(roilist)){
  roilist[[roi]] <- roilist[[roi]] %>%
    rename(subject = Subject) %>%
    left_join(subjectgroups, by = "subject") %>% 
    mutate(subject = as.factor(subject)) %>%
    dplyr::select(subject, trauma, 
                  NewCSP = B,
                  NewCSM = A,
                  NewCSPvNewCSM = BgtA,
                  NewCSMvNewCSP = AgtB,
                  PRE,
                  NewCSPvPRE = BgtPRE,
                  PREvNewCSP = PREgtB,
                  NewCSMvPRE = AgtPRE,
                  PREvNewCSM = PREgtA) 
}

# titles that go in same order as roilist
titles <- c("Bilateral anterior cingulate cortex", "Left amygdala", "Right amygdala", "Bilateral caudate", "Bilateral insula", 
            "Bilateral ventromedial prefrontal cortex", "Bilateral posterior parahippocampal gyrus", "Left hippocampus", "Right hippocampus") 
trauma_names <- c(`0` = "Control", `1` = "Trauma")  
stimuli_names <- c("New CS+", "New CS-")


# By stimulus --------------------------------------------

# plot prep and creation: stim
stimulus_list_tr <- lapply(roilist, stimulus_tr)
stimulus_summary_tr <- lapply(stimulus_list_tr, stimulus_sum_tr)
stimulus_info_tr <- lapply(stimulus_summary_tr, add_info_tr)
stimulus_final_tr <- Map(list, stimulus_info_tr, titles) 
plots_tr <- lapply(stimulus_final_tr, plot_RL_tr)
plots_tr

# save stim plots
#lapply(names(plots_tr),
#       function(x) ggsave(filename = paste("plots/", x, "_stim.pdf", sep=""), 
#                          plot = plots_tr[[x]], width = 5.5, height = 4.5, units = "in", dpi = 600))


# Difference --------------------------------------------

# plot prep and creation: differential, safety learning version (CS->CS+)
diff_list_tr_safety <- lapply(roilist, diff_tr_safety)
diff_summary_tr_safety <- lapply(diff_list_tr_safety, diff_sum_tr)
diff_info_tr_safety <- lapply(diff_summary_tr_safety, diff_add_info_tr_safety)
diff_final_tr_safety <- Map(list, diff_info_tr_safety, titles)
plots_tr_diff_safety <- lapply(diff_final_tr_safety, plot_RL_tr_diff_safety)
plots_tr_diff_safety

# save safety diff plots
#lapply(names(plots_tr_diff_safety),
#       function(x) ggsave(filename = paste("plots/", x, "_diff_safety.pdf", sep=""), 
#                          plot = plots_tr_diff_safety[[x]], width = 5, height = 4, units = "in", dpi = 600))

# plot prep and creation: differential, threat learning version (CS+>CS-) 
diff_list_tr <- lapply(roilist, diff_tr)
diff_summary_tr <- lapply(diff_list_tr, diff_sum_tr)
diff_info_tr <- lapply(diff_summary_tr, diff_add_info_tr)
diff_final_tr <- Map(list, diff_info_tr, titles)
plots_tr_diff <- lapply(diff_final_tr, plot_RL_tr_diff)
plots_tr_diff

# save threat diff plots
#lapply(names(plots_tr_diff),
#       function(x) ggsave(filename = paste("plots/", x, "_diff_threat.pdf", sep=""), 
#                          plot = plots_tr_diff[[x]], width = 5, height = 4, units = "in", dpi = 600))

