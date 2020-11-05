# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
source(here("code", "functions", "utils.R"))
source(here("code", "functions", "model_functions.R"))

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data", "running_data.csv"))
jumping_df <- read_csv(here("data", "jumping_data.csv"))

# Build models ------------------------------------------------------------

model_df <- data.frame(
  vector = c(rep("resultant", 3), rep("vertical", 3)),
  placement = rep(c("ankle", "lower_back", "hip"), 2)
)

running_data <- map2(
  model_df$vector, model_df$placement, 
  ~ filter_data(running_df, .x, .y)
)

jumping_data <- map2(
  model_df$vector, model_df$placement, 
  ~ filter_data(jumping_df, .x, .y)
)

GRF_formula <- as.formula(
  "pGRF_N ~ pACC_g + I(pACC_g^2) + body_mass + body_mass:pACC_g + (1 | subj)"
)
LR_formula <- as.formula(
  "pLR_Ns ~ pATR_gs + I(pATR_gs^2) + body_mass + body_mass:pATR_gs + (1 | subj)"
)

# Running GRF
# Build models
GRF_models_running <- map(running_data, ~ lmer(GRF_formula, .x)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Cross validate (leave-one-out cross-validation)
GRF_loocv_running <- map(running_data, ~ loocv(.x, GRF_formula)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Compute accuracy indices
GRF_accuracy_running <- map2(
  running_data, GRF_loocv_running, 
  ~ accuracy(.x, GRF_formula, .y$actual, .y$predicted)
) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Extract coefficients
GRF_coefficients_running <- map(GRF_models_running, get_coefficients)

# Running LR
# Build models
LR_models_running <- map(running_data, ~ lmer(LR_formula, .x)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Cross validate (leave-one-out cross-validation)
LR_loocv_running <- map(running_data, ~ loocv(.x, LR_formula)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Compute accuracy indices
LR_accuracy_running <- map2(
  running_data, LR_loocv_running, 
  ~ accuracy(.x, LR_formula, .y$actual, .y$predicted)
) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Extract coefficients
LR_coefficients_running <- map(LR_models_running, get_coefficients)

# Jumping GRF
# Build models
GRF_models_jumping <- map(jumping_data, ~ lmer(GRF_formula, .x)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Cross validate (leave-one-out cross-validation)
GRF_loocv_jumping <- map(jumping_data, ~ loocv(.x, GRF_formula)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Compute accuracy indices
GRF_accuracy_jumping <- map2(
  running_data, GRF_loocv_jumping, 
  ~ accuracy(.x, GRF_formula, .y$actual, .y$predicted)
) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Extract coefficients
GRF_coefficients_jumping <- map(GRF_models_jumping, get_coefficients)

# Jumping LR
# Build models
LR_models_jumping <- map(jumping_data, ~ lmer(LR_formula, .x)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Cross validate (leave-one-out cross-validation)
LR_loocv_jumping <- map(jumping_data, ~ loocv(.x, LR_formula)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Compute accuracy indices
LR_accuracy_jumping <- map2(
  running_data, LR_loocv_jumping, 
  ~ accuracy(.x, LR_formula, .y$actual, .y$predicted)
) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Extract coefficients
LR_coefficients_jumping <- map(LR_models_jumping, get_coefficients)
