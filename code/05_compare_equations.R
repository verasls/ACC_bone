# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("data", "mechanical_load_data.rda"))

lower_back_res_data <- mechanical_load_data |>
  filter(acc_placement == "lower_back" & vector == "resultant")
hip_res_data <- mechanical_load_data |>
  filter(acc_placement == "hip" & vector == "resultant")

lower_back_ver_data <- mechanical_load_data |>
  filter(acc_placement == "lower_back" & vector == "vertical")
hip_ver_data <- mechanical_load_data |>
  filter(acc_placement == "hip" & vector == "vertical")

# Predict using Veras et al. 2020 -----------------------------------------

# GRF
lower_back_res_GRF_df <- predict_Veras2020(
  lower_back_res_data,
  outcome = "GRF", acc_placement = "lower_back", vector = "resultant"
)
hip_res_GRF_df <- predict_Veras2020(
  hip_res_data,
  outcome = "GRF", acc_placement = "hip", vector = "resultant"
)

lower_back_ver_GRF_df <- predict_Veras2020(
  lower_back_ver_data,
  outcome = "GRF", acc_placement = "lower_back", vector = "vertical"
)
hip_ver_GRF_df <- predict_Veras2020(
  hip_ver_data,
  outcome = "GRF", acc_placement = "hip", vector = "vertical"
)

# LR
lower_back_res_LR_df <- predict_Veras2020(
  lower_back_res_data,
  outcome = "LR", acc_placement = "lower_back", vector = "resultant"
)
hip_res_LR_df <- predict_Veras2020(
  hip_res_data,
  outcome = "LR", acc_placement = "hip", vector = "resultant"
)

lower_back_ver_LR_df <- predict_Veras2020(
  lower_back_ver_data,
  outcome = "LR", acc_placement = "lower_back", vector = "vertical"
)
hip_ver_LR_df <- predict_Veras2020(
  hip_ver_data,
  outcome = "LR", acc_placement = "hip", vector = "vertical"
)

# Compute accuracy --------------------------------------------------------

# GRF
compute_accuracy(
  lower_back_res_GRF_df$pGRF_N, lower_back_res_GRF_df$.predicted_Veras2020
)
compute_accuracy(
  hip_res_GRF_df$pGRF_N, hip_res_GRF_df$.predicted_Veras2020
)

compute_accuracy(
  lower_back_ver_GRF_df$pGRF_N, lower_back_ver_GRF_df$.predicted_Veras2020
)
compute_accuracy(
  hip_ver_GRF_df$pGRF_N, hip_ver_GRF_df$.predicted_Veras2020
)

# LR
compute_accuracy(
  lower_back_res_LR_df$pLR_Ns, lower_back_res_LR_df$.predicted_Veras2020
)
compute_accuracy(
  hip_res_LR_df$pLR_Ns, hip_res_LR_df$.predicted_Veras2020
)

compute_accuracy(
  lower_back_ver_LR_df$pLR_Ns, lower_back_ver_LR_df$.predicted_Veras2020
)
compute_accuracy(
  hip_ver_LR_df$pLR_Ns, hip_ver_LR_df$.predicted_Veras2020
)
