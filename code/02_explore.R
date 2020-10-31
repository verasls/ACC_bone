# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
source(here("code", "functions", "plot_functions.R"))

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data", "running_data.csv")) %>% 
  mutate(
    acc_placement = as_factor(acc_placement),
    vector = as_factor(vector),
    speed = as_factor(speed)
  )

jumping_df <- read_csv(here("data", "jumping_data.csv")) %>% 
  mutate(
    acc_placement = as_factor(acc_placement),
    vector = as_factor(vector),
    jump_type = as_factor(jump_type),
    jump_height = as_factor(jump_height)
  )

# Sample size per activity ------------------------------------------------

running_df %>%
  group_by(acc_placement, vector, speed) %>%
  select(acc_placement, vector, speed, pGRF_N, pLR_Ns) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  knitr::kable()

jumping_df %>% 
  group_by(acc_placement, vector, jump_type, jump_height) %>% 
  select(acc_placement, vector, jump_type, jump_height, pGRF_N, pLR_Ns) %>% 
  summarise_all(~ sum(!is.na(.))) %>% 
  knitr::kable()

# Number of peaks median and IQR ------------------------------------------

running_df %>% 
  summarise(
    n_peaks_median = median(n_peaks), n_peaks_iqr = IQR(n_peaks)
  )

jumping_df %>% 
  group_by(jump_type) %>% 
  summarise(
    n_peaks_median = median(n_peaks), n_peaks_iqr = IQR(n_peaks),
    .groups = "drop"
  )

# Histograms --------------------------------------------------------------

# Vectors with variables to plot in histograms
hist_df <- tibble(
  vectors = c(rep("resultant", 4), rep("vertical", 4)),
  hist_vars = rep(c("pGRF_N", "pACC_g", "pLR_Ns", "pATR_gs"), 2)
)

# Running 
map2(
  hist_df$vectors, hist_df$hist_vars,
  ~ histogram(running_df, .x, .y, "acc_placement", "speed")
)

# Jumping
drop_jumps <- filter(jumping_df, jump_type == "drop jumps")
box_jumps <- filter(jumping_df, jump_type == "box jumps")
continuous_jumps <- filter(jumping_df, jump_type == "continuous jumps")

map2(
  hist_df$vectors, hist_df$hist_vars,
  ~ histogram(drop_jumps, .x, .y, "acc_placement", "jump_height")
)
map2(
  hist_df$vectors, hist_df$hist_vars,
  ~ histogram(box_jumps, .x, .y, "acc_placement", "jump_height")
)
map2(
  hist_df$vectors, hist_df$hist_vars,
  ~ histogram(continuous_jumps, .x, .y, "acc_placement", "jump_height")
)
