# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
source(here("code", "functions", "utils.R"))
source(here("code", "functions", "model_functions.R"))

# Load data ---------------------------------------------------------------

jumping_df <- read_csv(here("data", "jumping_data.csv"))

# Build models ------------------------------------------------------------

model_df <- data.frame(
  vector = c(rep("resultant", 3), rep("vertical", 3)),
  placement = rep(c("ankle", "lower_back", "hip"), 2)
)
GRF_formula <- as.formula(
  "pGRF_N ~ pACC_g + I(pACC_g^2) + body_mass + body_mass:pACC_g + (1 | subj)"
)

# Jumping
# List of filtered data frames
jump_data <- map2(
  model_df$vector, model_df$placement, 
  ~ filter_data(jumping_df, .x, .y)
)
# Build models
GRF_models_jumping <- map(jump_data, ~ lmer(GRF_formula, .x)) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Compute accuracy indices
GRF_accuracy_jumping <- map2(jump_data, GRF_models_jumping, accuracy) %>% 
  set_names(paste0(model_df$placement, "_", model_df$vector))
# Extract coefficients
GRF_model_coefficients_jumping <- map(GRF_models_jumping, get_coefficients)
