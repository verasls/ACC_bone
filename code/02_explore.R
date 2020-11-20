# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
source(here("code/functions/plot_functions.R"))
source(here("code/functions/utils.R"))

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/processed/running_data.csv")) %>%
  mutate(
    acc_placement = as_factor(acc_placement),
    vector = as_factor(vector),
    speed = as_factor(speed),
    run = as_factor(paste0("running ", speed, "km/h")),
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I"
      )
    )
  )

jumping_df <- read_csv(here("data/processed/jumping_data.csv")) %>%
  mutate(
    acc_placement = as_factor(acc_placement),
    vector = as_factor(vector),
    jump_type = as_factor(jump_type),
    jump_height = as_factor(jump_height),
    jump = as_factor(paste0(jump_type, " ", jump_height, "cm")),
    jump = fct_relevel(jump, "drop jumps 40cm", after = 7),
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I",
        "Obesity class II", "Obesity class III"
      )
    )
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

# Ground reaction force and acceleration magnitude per activity -----------

# Ground reaction force
box_plot(running_df, run, pGRF_BW, "lower_back")
box_plot(jumping_df, jump, pGRF_BW, "lower_back")

# Loading rate
box_plot(running_df, run, pLR_BWs, "lower_back")
box_plot(jumping_df, jump, pLR_BWs, "lower_back")

# Acceleration
map(
  c("ankle", "lower_back", "hip"),
  ~ box_plot(running_df, run, pACC_g, .x)
)
map(
  c("ankle", "lower_back", "hip"),
  ~ box_plot(jumping_df, jump, pACC_g, .x)
)

# Acceleration transient rate
map(
  c("ankle", "lower_back", "hip"),
  ~ box_plot(running_df, run, pATR_gs, .x)
)
map(
  c("ankle", "lower_back", "hip"),
  ~ box_plot(jumping_df, jump, pATR_gs, .x)
)

# Correlations ------------------------------------------------------------

corr_df <- tibble(
  vectors = c(rep("resultant", 3), rep("vertical", 3)),
  placement = rep(c("ankle", "lower_back", "hip"), 2)
)

# Running
map2(
  corr_df$vectors, corr_df$placement,
  ~ my_correlate(running_df, .x, .y)
)

# Jumping
map2(
  corr_df$vectors, corr_df$placement,
  ~ my_correlate(jumping_df, .x, .y)
)

# Scatterplot -------------------------------------------------------------

placements <- c("ankle", "lower_back", "hip")
# Running
map(
  placements,
  ~ scatterplot(running_df, pACC_g, pGRF_N, .x)
)
map(
  placements,
  ~ scatterplot(running_df, pATR_gs, pLR_Ns, .x)
)

# Jumping
map(
  placements,
  ~ scatterplot(jumping_df, pACC_g, pGRF_N, .x)
)
map(
  placements,
  ~ scatterplot(jumping_df, pATR_gs, pLR_Ns, .x)
)
