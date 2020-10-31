# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data", "running_data.csv"))
jumping_df <- read_csv(here("data", "jumping_data.csv"))

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
