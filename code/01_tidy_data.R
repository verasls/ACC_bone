# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lvmisc)

# Read anthropometric data ------------------------------------------------

anthropometric <- read_csv(here("data", "anthropometric_data.csv")) %>%
  select(id = ID, everything()) %>% 
  mutate(
    BMI = bmi(body_mass, height / 100),
    BMI_cat = bmi_cat(BMI)
  )

# Obtain running data -----------------------------------------------------

# Data with force and acceleration
running_df_wide <- read_xlsx(
  here("data", "GRF_ACC_data_all.xlsx"), sheet = "all"
) %>% 
  clean_names() %>% 
  select(
    raw_imu, id, trial = visita, filename = nome,
    acc_placement = local_acelerometro,
    speed = velocidade_km_h, 
    res_n_peaks = ftot_numero_picos_ftot,
    ver_n_peaks = fvt_numero_picos_fvt,
    pRACC_g = atot_med_picos_atot_g,
    pRGRF_N = ftot_med_picos_ftot_n,
    pRGRF_BW = ftot_med_picos_ftot_bw,
    pVACC_g = avt_med_picos_avt_g,
    pVGRF_N = fvt_med_picos_fvt_n,
    pVGRF_BW = fvt_med_picos_fvt_bw
  ) %>% 
  filter(
    raw_imu == "imu" &
    speed > 6 &
    acc_placement %in% c("ankle", "back", "waist")
  ) %>% 
  select(id, everything())

running_df_res <- running_df_wide %>% 
  select(
    id, trial, filename, acc_placement, speed,
    n_peaks = res_n_peaks, pACC_g = pRACC_g, 
    pGRF_N = pRGRF_N, pGRF_BW = pRGRF_BW
  ) %>% 
  mutate(
    vector = "resultant",
    .after = acc_placement
  )

running_df_ver <- running_df_wide %>% 
  select(
    id, trial, filename, acc_placement, speed,
    n_peaks = ver_n_peaks, pACC_g = pVACC_g, 
    pGRF_N = pVGRF_N, pGRF_BW = pVGRF_BW
  ) %>% 
  mutate(
    vector = "vertical",
    .after = acc_placement
  )

running_df <- rbind(running_df_res, running_df_ver) %>% 
  mutate(
    # Create another subj identifier
    subj = paste0(id, "m", trial),
    subj = ifelse(str_length(subj) == 3, paste0("00", subj), subj),
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj)
  ) %>% 
  left_join(
    select(anthropometric, id, trial, body_mass, BMI, BMI_cat),
    by = c("id", "trial")
  ) %>% 
  select(
    id, trial, filename, subj, body_mass, BMI, BMI_cat, everything()
  )

# Data with force and acceleration rates
running_rates_df_wide <- read_csv(
  here("data", "max_rates_IMU_running.csv")
) %>% 
  clean_names() %>% 
  select(
    id, trial = visita, filename = nome, acc_placement = local_acelerometro,
    speed = velocidade_km_h, 
    pRATR_gs = max_rates_atot_med_atot_g_s,
    pRLR_Ns = max_rates_ftot_med_ftot_n_s,
    pRLR_BWs = max_rates_ftot_med_ftot_bw,
    pVATR_gs = max_rates_avt_med_avt_g_s,
    pVLR_Ns = max_rates_fvt_med_fvt_n_s,
    pVLR_BWs = max_rates_fvt_med_fvt_bw
  ) %>% 
  filter(speed > 6 & acc_placement %in% c("ankle", "back", "waist"))

running_rates_df_res <- running_rates_df_wide %>% 
  select(
    id, trial, filename, acc_placement, speed,
    pATR_gs = pRATR_gs, 
    pLR_Ns = pRLR_Ns, pLR_BWs = pRLR_BWs
  ) %>% 
  mutate(
    vector = "resultant",
    .after = acc_placement
  )

running_rates_df_ver <- running_rates_df_wide %>% 
  select(
    id, trial, filename, acc_placement, speed,
    pATR_gs = pVATR_gs, 
    pLR_Ns = pVLR_Ns, pLR_BWs = pVLR_BWs
  ) %>% 
  mutate(
    vector = "vertical",
    .after = acc_placement
  )

running_rates_df <- rbind(running_rates_df_res, running_rates_df_ver) %>% 
  mutate(
    # Create another subj identifier
    subj = paste0(id, "m", trial),
    subj = ifelse(str_length(subj) == 3, paste0("00", subj), subj),
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj)
  ) %>% 
  left_join(
    select(anthropometric, id, trial, body_mass, BMI, BMI_cat),
    by = c("id", "trial")
  ) %>% 
  select(
    id, trial, filename, subj, body_mass, BMI, BMI_cat, everything()
  )

# Join both running data frames
running_df <- running_df %>% 
  left_join(running_rates_df)

write_csv(running_df, here("data", "running_data.csv"))

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
    subj = ifelse(str_length(subj) == 4, paste0("0", subj), subj)
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
write_csv(jumping_df, here("data", "jumping_data.csv"))
