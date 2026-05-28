# plot-RL-functions.R
# Written by Stephanie DeCross, June 2023

# Helper functions for plotting RL


# By stimulus -------------------------------------------------------------------------------------

# get stimuli
stimulus_tr <- function(x){
  output <- x %>%
    dplyr::select(subject, trauma, 
                  NewCSP, NewCSM) %>%
    gather(key = "stimulus", value = "response", 3:4)
  return(output)
}

# summarize control and trauma separately and stick in same dataframe
stimulus_sum_tr <- function(x){
  
  tmp1 <- x %>% filter(trauma == 0) 
  output_ctl <- data.frame(mean <- tapply(tmp1$response, tmp1$stimulus, mean),
                           n <- tapply(tmp1$response, tmp1$stimulus, length), 
                           sd <- tapply(tmp1$response, tmp1$stimulus, sd))
  output_ctl$se <- output_ctl$sd/sqrt(output_ctl$n)
  output_ctl["trauma"] <- 0 
  output_ctl2 <- setDT(output_ctl, keep.rownames = TRUE)[] 
  colnames(output_ctl2) = c("stimulus", "mean", "n", "sd", "se", "trauma")
  
  tmp2 <- x %>% filter(trauma == 1) 
  output_tr <- data.frame(mean <- tapply(tmp2$response, tmp2$stimulus, mean),
                          n <- tapply(tmp2$response, tmp2$stimulus, length), 
                          sd <- tapply(tmp2$response, tmp2$stimulus, sd))
  output_tr$se <- output_tr$sd/sqrt(output_tr$n)
  output_tr["trauma"] <- 1 
  output_tr2 <- setDT(output_tr, keep.rownames = TRUE)[] 
  colnames(output_tr2) = c("stimulus", "mean", "n", "sd", "se", "trauma")
  
  output_combined <- rbind(output_ctl2, output_tr2)
  
  return(output_combined)
  
}

# add info
add_info_tr <- function(x){
  output <- x %>%
    mutate(stim = 
             if_else(substr(stimulus, 1, 6) == "NewCSM", "New CS-", "New CS+"))
  return(output)
}

# plot function
plot_RL_tr <- function(x){
  ggplot(data = x[[1]], aes(x = factor(stimulus, levels=c("NewCSP", "NewCSM")), y = mean, color = stim)) + 
    theme_classic(base_size = 14) +
    theme(panel.border = element_rect(fill = NA)) +
    geom_point(size = 4.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3, size = 1.5, show.legend = FALSE, position = position_dodge(width = 0.25)) +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)) +
    ylab("BOLD response") +
    ggtitle(paste0(x[[2]],":\nNew CS+ and New CS-")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(name = "Stimuli", values = c("New CS+" = "#de4c3f", "New CS-" = "#386ce5")) + 
    guides(color = guide_legend(override.aes = list(size = 2, shape = 15))) + 
    theme(legend.title = element_text(size = 12)) + 
    theme(legend.text = element_text(size = 12)) + 
    scale_x_discrete(labels = stimuli_names) + 
    facet_grid(~trauma, labeller = as_labeller(trauma_names)) + 
    theme(strip.text =  element_text(size = 14)) + 
    theme(legend.position = "none") + 
    ylim(-.6, .6) +
    theme(axis.text.x = element_text(color="black"),
          axis.text.y = element_text(color="black"))  + 
    theme(axis.title.x=element_blank()) 
}


# By difference ---------------------------------------------------------------------

# get stimuli, threat learning
diff_tr <- function(x){
  output <- x %>%
    dplyr::select(subject, trauma, 
                  NewCSPvNewCSM) %>%
    gather(key = "stimulus", value = "response", 3:3)
  return(output)
}

# get stimuli, safety learning
diff_tr_safety <- function(x){
  output <- x %>%
    dplyr::select(subject, trauma, 
                  NewCSMvNewCSP) %>%
    gather(key = "stimulus", value = "response", 3:3)
  return(output)
}

# summarize control and trauma separately and stick in same dataframe
diff_sum_tr <- function(x){
  
  tmp1 <- x %>% filter(trauma == 0) 
  output_ctl <- data.frame(mean <- tapply(tmp1$response, tmp1$stimulus, mean),
                           n <- tapply(tmp1$response, tmp1$stimulus, length), 
                           sd <- tapply(tmp1$response, tmp1$stimulus, sd))
  output_ctl$se <- output_ctl$sd/sqrt(output_ctl$n)
  output_ctl["trauma"] <- 0 # adding trauma group col
  output_ctl2 <- setDT(output_ctl, keep.rownames = TRUE)[] 
  colnames(output_ctl2) = c("stimulus", "mean", "n", "sd", "se", "trauma")
  
  tmp2 <- x %>% filter(trauma == 1) 
  output_tr <- data.frame(mean <- tapply(tmp2$response, tmp2$stimulus, mean),
                          n <- tapply(tmp2$response, tmp2$stimulus, length), 
                          sd <- tapply(tmp2$response, tmp2$stimulus, sd))
  output_tr$se <- output_tr$sd/sqrt(output_tr$n)
  output_tr["trauma"] <- 1 
  output_tr2 <- setDT(output_tr, keep.rownames = TRUE)[] 
  colnames(output_tr2) = c("stimulus", "mean", "n", "sd", "se", "trauma")
  
  output_combined <- rbind(output_ctl2, output_tr2)
  
  return(output_combined)
  
}

# add info, threat learning
diff_add_info_tr <- function(x){
  output <- x %>%
    mutate(stim = 
             if_else(substr(stimulus, 1, 3) == "New", "New CS+ vs. New CS-", "New CS+ vs. New CS-"))
  return(output)
}

# add info, safety learning
diff_add_info_tr_safety <- function(x){
  output <- x %>%
    mutate(stim = 
             if_else(substr(stimulus, 1, 3) == "New", "New CS- vs. New CS+", "New CS- vs. New CS+"))
  return(output)
}

# plot diff, threat learning (CS+>CS-)
plot_RL_tr_diff <- function(x){
  ggplot(data = x[[1]], aes(x = factor(stimulus), y = mean)) + 
    theme_classic(base_size = 14) +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    theme(panel.border = element_rect(fill = NA)) +
    geom_point(size = 4.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, size = 1.5, show.legend = FALSE, position = position_dodge(width = 0.25)) +
    ylab("BOLD response") +
    ggtitle(paste0(x[[2]],":\nNew CS+ vs. New CS-")) +
    theme(plot.title = element_text(hjust = 0.5)) + #
    guides(color = guide_legend(override.aes = list(size = 2, shape = 15))) + 
    theme(legend.title = element_text(size = 12)) + 
    theme(legend.text = element_text(size = 12)) + 
    facet_grid(~trauma, labeller = as_labeller(trauma_names)) + 
    theme(strip.text =  element_text(size = 14)) + 
    theme(legend.position = "none") + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_text(size=14),
          axis.text.y = element_text(size=12)) +
    ylim(-.8, .8) +
    theme(axis.text.y = element_text(color="black"))  
}

# plot diff, safety learning (CS->CS+) 
plot_RL_tr_diff_safety <- function(x){
  ggplot(data = x[[1]], aes(x = factor(stimulus), y = mean)) + 
    theme_classic(base_size = 14) +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    theme(panel.border = element_rect(fill = NA)) +
    geom_point(size = 4.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, size = 1.5, show.legend = FALSE, position = position_dodge(width = 0.25)) +
    ylab("BOLD response") +
    ggtitle(paste0(x[[2]],":\nNew CS- vs. New CS+")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(color = guide_legend(override.aes = list(size = 2, shape = 15))) + 
    theme(legend.title = element_text(size = 12)) + 
    theme(legend.text = element_text(size = 12)) + 
    facet_grid(~trauma, labeller = as_labeller(trauma_names)) + 
    theme(strip.text =  element_text(size = 14)) + 
    theme(legend.position = "none") + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_text(size=14),
          axis.text.y = element_text(size=12)) +
    ylim(-.8, .8) +
    theme(axis.text.y = element_text(color="black"))  
}

