# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)

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

# Running - resultant
running_df %>% 
  filter(vector == "resultant") %>% 
  ggplot() +
  geom_histogram(aes(x = pGRF_N)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "resultant") %>% 
  ggplot() +
  geom_histogram(aes(x = pACC_g)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "resultant") %>% 
  ggplot() +
  geom_histogram(aes(x = pLR_Ns)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "resultant") %>% 
  ggplot() +
  geom_histogram(aes(x = pATR_gs)) +
  facet_grid(acc_placement ~ speed)

# Running - vertical
running_df %>% 
  filter(vector == "vertical") %>% 
  ggplot() +
  geom_histogram(aes(x = pGRF_N)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "vertical") %>% 
  ggplot() +
  geom_histogram(aes(x = pACC_g)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "vertical") %>% 
  ggplot() +
  geom_histogram(aes(x = pLR_Ns)) +
  facet_grid(acc_placement ~ speed)

running_df %>% 
  filter(vector == "vertical") %>% 
  ggplot() +
  geom_histogram(aes(x = pATR_gs)) +
  facet_grid(acc_placement ~ speed)

# Jumping - resultant - drop jumps
jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "drop jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pGRF_N)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "drop jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pACC_g)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "drop jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pLR_Ns)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "drop jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pATR_gs)) +
  facet_grid(acc_placement ~ jump_height)

# Jumping - resultant - box jumps
jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "box jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pGRF_N)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "box jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pACC_g)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "box jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pLR_Ns)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "box jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pATR_gs)) +
  facet_grid(acc_placement ~ jump_height)

# Jumping - resultant - continuous jumps
jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "continuous jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pGRF_N)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "continuous jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pACC_g)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "continuous jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pLR_Ns)) +
  facet_grid(acc_placement ~ jump_height)

jumping_df %>% 
  filter(
    vector == "resultant" & jump_type == "continuous jumps"
  ) %>% 
  ggplot() + 
  geom_histogram(aes(x = pATR_gs)) +
  facet_grid(acc_placement ~ jump_height)
