# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lvmisc)
library(emmeans)

# Load and prepare data ---------------------------------------------------

load(here("data", "mechanical_load_data.rda"))

# Create a list of 3 data frames: one for each accelerometer placement
mechanical_load_by_placements <- map(
  c("ankle", "lower_back", "hip"),
  ~ filter(mechanical_load_data, acc_placement == .x)
) %>% set_names(c("ankle", "lower_back", "hip"))

# Filter the data frames in the list, separating the vertical and resultant
# vectors
res_mechanical_load_data <- map(
  mechanical_load_by_placements, filter, vector == "resultant"
)
ver_mechanical_load_data <- map(
  mechanical_load_by_placements, filter, vector == "vertical"
)

# Build GRF models --------------------------------------------------------

GRF_formula <- as.formula(
  "pGRF_N ~ pACC_g + body_mass + pACC_g:body_mass +
    (1 | subj) + (1 | jump_height)"
)

# Resultant vector
# Build models
res_GRF_models <- map(
  res_mechanical_load_data,
  ~ lmer(GRF_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_res_GRF_models <- map2(
  res_GRF_models, res_mechanical_load_data,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_res_GRF_models <- map(cv_res_GRF_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_res_GRF_models <- map(
  cv_res_GRF_models, plot_bland_altman, color = BMI_cat
)

# Vertical vector
# Build models
ver_GRF_models <- map(
  ver_mechanical_load_data,
  ~ lmer(GRF_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_ver_GRF_models <- map2(
  ver_GRF_models, ver_mechanical_load_data,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_ver_GRF_models <- map(cv_ver_GRF_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_ver_GRF_models <- map(
  cv_ver_GRF_models, plot_bland_altman, color = BMI_cat
)

# Build LR models ---------------------------------------------------------

LR_formula <- as.formula(
  "pLR_Ns ~ pAR_gs + body_mass + pAR_gs:body_mass +
    (1 | subj) + (1 | jump_height)"
)

# Resultant vector
# Build models
res_LR_models <- map(
  res_mechanical_load_data,
  ~ lmer(LR_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_res_LR_models <- map2(
  res_LR_models, res_mechanical_load_data,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_res_LR_models <- map(cv_res_LR_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_res_LR_models <- map(
  cv_res_LR_models, plot_bland_altman, color = BMI_cat
)

# Vertical vector
# Build models
ver_LR_models <- map(
  ver_mechanical_load_data,
  ~ lmer(LR_formula, data = .x)
)
# Cross-validate (leave-one-out cross-validation)
cv_ver_LR_models <- map2(
  ver_LR_models, ver_mechanical_load_data,
  ~ loo_cv(.x, .y, id = subj)
)
# Compute accuracy indices
accuracy_ver_LR_models <- map(cv_ver_LR_models, accuracy, na.rm = TRUE)
# Bland-Altman plots
plot_ver_LR_models <- map(
  cv_ver_LR_models, plot_bland_altman, color = BMI_cat
)

# Save models and leave-one-out cross-validation data ---------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  cv_res_GRF_models, cv_ver_GRF_models,
  cv_res_LR_models, cv_ver_LR_models,
  file = here("output", "loocv_data.rda")
)
save(
  res_GRF_models, ver_GRF_models,
  res_LR_models, ver_LR_models,
  accuracy_res_GRF_models,
  accuracy_ver_GRF_models,
  accuracy_res_LR_models,
  accuracy_ver_LR_models,
  file = here("output", "prediction_models.rda")
)
