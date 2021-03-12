# Mock data wrangling
# Created by Mike Carter
# Date: March 4 2021
# Last modified: March 4 2021

# libraries needed
library(tidyverse)

# load data
expt1_data <- readr::read_csv("data/3rp3-tutorial-data.csv")

# function to calculate variable error (ve)
ve <- function(x) {
  sqrt(sum((x - mean(x, na.rm = TRUE))^2,
       na.rm = TRUE)/sum(!is.na(x)))
}

# Performance data
# Add columns for choice, feedback, collapse pre-test
# Add columns for constant error (ce_ms), absolute error (ae_ms)
# Target = 265 ms in all phases except transfer
# Target = 300 ms in transfer
# ce = participant response - target
# ae = |participant response - target|
expt1_data <- expt1_data %>%
  dplyr::mutate(choice_id = dplyr::if_else(group_id <= 2, 1, 2)) %>%
  dplyr::mutate(fb_id = dplyr::if_else(group_id %in% c(1, 3), 1, 2)) %>%
  dplyr::mutate(phase_id_4 = dplyr::if_else(phase_id <= 1, 1,
                                            expt1_data$phase_id)) %>%
  dplyr::mutate(block_id = rep(rep(1:9, each = 12), 152)) %>%
  dplyr::mutate(ce_ms = dplyr::if_else(phase_id != 4, expt1_data$rev_ms - 265, expt1_data$rev_ms - 300)) %>%
  dplyr::mutate(ae_ms = dplyr::if_else(phase_id != 4, abs(expt1_data$rev_ms - 265), abs(expt1_data$rev_ms - 300)))

# Re-order columns because I'm crazy
expt1_data <- expt1_data %>%
  dplyr::select(1:3, choice_id, fb_id, phase_id_4, phase_id, trial_phase,
                trial_expt, block_id, everything())

# Make factors
expt1_data <- expt1_data %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    choice_id = forcats::as_factor(choice_id),
    fb_id = forcats::as_factor(fb_id),
    phase_id_4 = forcats::as_factor(phase_id_4),
    phase_id = forcats::as_factor(phase_id),
    block_id = forcats::as_factor(block_id)
  )
expt1_data

# Calculate mean error scores for each participant for blocks of 12 trials
# 9 blocks for 108 trials
expt1_9blocks <- expt1_data %>%
  dplyr::group_by(id, group_id, choice_id, fb_id, phase_id_4, block_id) %>%
  dplyr::summarize(
    n = n(),
    mean_ce = mean(ce_ms, na.rm = TRUE),
    mean_ae = mean(ae_ms, na.rm = TRUE),
    mean_ve1 = ve(rev_ms),
    mean_ve2 = ve(ce_ms)
  )

# Create a tibble with pre-test, retention, and transfer
expt1_prt <- dplyr::filter(expt1_9blocks, phase_id_4 != 2) %>%
  dplyr::rename(phase_id_3 = phase_id_4)


