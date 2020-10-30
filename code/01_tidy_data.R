# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lvmisc)

# Read anthropometric data ------------------------------------------------

anthropometric <- read_csv("data/anthropometric_data.csv") %>%
  select(id = ID, everything()) %>% 
  mutate(
    BMI = bmi(body_mass, height / 100),
    BMI_cat = bmi_cat(BMI)
  )

# Obtain jumping data -----------------------------------------------------

data_dir <- "/Volumes/LV_HD_2/Accelerometry/GRF_ACC/"
data_dir <- list.files(
  paste0(data_dir, list.files(data_dir)),
  pattern = "*Plataforma*",
  full.names = TRUE
)

files <- list.files(
  data_dir,
  pattern = "*extracted*",
  full.names = TRUE
)

jumping_df <- files %>% 
  map_dfr(read_csv) %>% 
  clean_names() %>% 
  mutate(
    # Extract trial number from filename
    trial = str_extract(filename, "_m\\d_"),
    trial = as.double(str_extract(trial, "\\d")),
    # Create another subj identifier
    subj = paste0(id, "m", trial),
    subj = ifelse(str_length(subj) == 3, paste0("00", subj), subj),
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj),
  ) %>% 
  left_join(
    select(anthropometric, id, trial, BMI, BMI_cat), 
    by = c("id", "trial")
  ) %>% 
  select(
    id, trial, filename, subj, body_mass, BMI, BMI_cat, acc_placement, vector,
    jump_type, jump_height, n_peaks,
    pACC_g = p_acc_g_mean, pGRF_N = p_grf_n_mean, pGRF_BW = p_grf_bw_mean,
    pATR_gs = p_atr_gs_mean, pLR_Ns = p_lr_ns_mean, pLR_BWs = p_lr_bws_mean
  ) %>% 
  arrange(subj)

# Write jumping data
write_csv(jumping_df, "data/jumping_data.csv")
