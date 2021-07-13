# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "loocv_data.rda"))

# Bland-Altman plots tests ------------------------------------------------

# Linear regression diff ~ mean
ba_regression_res_GRF <- map(cv_res_GRF_models, bland_altman_regression)
ba_regression_ver_GRF <- map(cv_ver_GRF_models, bland_altman_regression)
ba_regression_res_LR <- map(cv_res_LR_models, bland_altman_regression)
ba_regression_ver_LR <- map(cv_ver_LR_models, bland_altman_regression)

# Check if bias differs from 0
ba_bias_test_res_GRF <- map(cv_res_GRF_models, bland_altman_t_test)
ba_bias_test_ver_GRF <- map(cv_ver_GRF_models, bland_altman_t_test)
ba_bias_test_res_LR <- map(cv_res_LR_models, bland_altman_t_test)
ba_bias_test_ver_LR <- map(cv_ver_LR_models, bland_altman_t_test)

# Accuracy per activity ---------------------------------------------------

# Drop jumps
drop_jumps_res_GRF_accuracy <- cv_res_GRF_models %>%
  map(~ filter(.x, jump_type == "drop jumps")) %>%
  map(accuracy, na.rm = TRUE)
drop_jumps_ver_GRF_accuracy <- cv_ver_GRF_models %>%
  map(~ filter(.x, jump_type == "drop jumps")) %>%
  map(accuracy, na.rm = TRUE)
drop_jumps_res_LR_accuracy <- cv_res_LR_models %>%
  map(~ filter(.x, jump_type == "drop jumps")) %>%
  map(accuracy, na.rm = TRUE)
drop_jumps_ver_LR_accuracy <- cv_ver_LR_models %>%
  map(~ filter(.x, jump_type == "drop jumps")) %>%
  map(accuracy, na.rm = TRUE)
# Box jumps
box_jumps_res_GRF_accuracy <- cv_res_GRF_models %>%
  map(~ filter(.x, jump_type == "box jumps")) %>%
  map(accuracy, na.rm = TRUE)
box_jumps_ver_GRF_accuracy <- cv_ver_GRF_models %>%
  map(~ filter(.x, jump_type == "box jumps")) %>%
  map(accuracy, na.rm = TRUE)
box_jumps_res_LR_accuracy <- cv_res_LR_models %>%
  map(~ filter(.x, jump_type == "box jumps")) %>%
  map(accuracy, na.rm = TRUE)
box_jumps_ver_LR_accuracy <- cv_ver_LR_models %>%
  map(~ filter(.x, jump_type == "box jumps")) %>%
  map(accuracy, na.rm = TRUE)
# Continuous jumps
continuous_jumps_res_GRF_accuracy <- cv_res_GRF_models %>%
  map(~ filter(.x, jump_type == "continuous jumps")) %>%
  map(accuracy, na.rm = TRUE)
continuous_jumps_ver_GRF_accuracy <- cv_ver_GRF_models %>%
  map(~ filter(.x, jump_type == "continuous jumps")) %>%
  map(accuracy, na.rm = TRUE)
continuous_jumps_res_LR_accuracy <- cv_res_LR_models %>%
  map(~ filter(.x, jump_type == "continuous jumps")) %>%
  map(accuracy, na.rm = TRUE)
continuous_jumps_ver_LR_accuracy <- cv_ver_LR_models %>%
  map(~ filter(.x, jump_type == "continuous jumps")) %>%
  map(accuracy, na.rm = TRUE)

# Save accuracy per activity ----------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  drop_jumps_ver_GRF_accuracy, drop_jumps_ver_GRF_accuracy,
  drop_jumps_res_LR_accuracy, drop_jumps_ver_LR_accuracy,
  box_jumps_ver_GRF_accuracy, box_jumps_ver_GRF_accuracy,
  box_jumps_res_LR_accuracy, box_jumps_ver_LR_accuracy,
  continuous_jumps_ver_GRF_accuracy, continuous_jumps_ver_GRF_accuracy,
  continuous_jumps_res_LR_accuracy, continuous_jumps_ver_LR_accuracy,
  file = here("output", "jump_type_accuracy.rda")
)
