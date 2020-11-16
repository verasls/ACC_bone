# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
source(here("code/functions/utils.R"))
source(here("code/functions/model_functions.R"))
source(here("code/functions/plot_functions.R"))

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/running_data.csv"))
jumping_df <- read_csv(here("data/jumping_data.csv"))

# Build models ------------------------------------------------------------

model_df <- data.frame(
  vector = c(rep("resultant", 3), rep("vertical", 3)),
  placement = rep(c("ankle", "lower_back", "hip"), 2)
)

names <- paste0(model_df$placement, "_", model_df$vector)

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
  set_names(names)
# Cross validate (leave-one-out cross-validation)
GRF_loocv_running <- map(running_data, ~ loocv(.x, GRF_formula)) %>%
  set_names(names)
# Compute accuracy indices
GRF_accuracy_running <- map2(
  running_data, GRF_loocv_running,
  ~ accuracy(.x, GRF_formula, .y$actual, .y$predicted)
) %>%
  set_names(names)
# Bland-Altman plots
GRF_plots_running <- map2(GRF_loocv_running, names, bland_altman)
# Extract coefficients
GRF_coefficients_running <- map(GRF_models_running, get_coefficients)

# Running LR
# Build models
LR_models_running <- map(running_data, ~ lmer(LR_formula, .x)) %>%
  set_names(names)
# Cross validate (leave-one-out cross-validation)
LR_loocv_running <- map(running_data, ~ loocv(.x, LR_formula)) %>%
  set_names(names)
# Compute accuracy indices
LR_accuracy_running <- map2(
  running_data, LR_loocv_running,
  ~ accuracy(.x, LR_formula, .y$actual, .y$predicted)
) %>%
  set_names(names)
# Bland-Altman plots
LR_plots_running <- map2(LR_loocv_running, names, bland_altman)
# Extract coefficients
LR_coefficients_running <- map(LR_models_running, get_coefficients)

# Jumping GRF
# Build models
GRF_models_jumping <- map(jumping_data, ~ lmer(GRF_formula, .x)) %>%
  set_names(names)
# Cross validate (leave-one-out cross-validation)
GRF_loocv_jumping <- map(jumping_data, ~ loocv(.x, GRF_formula)) %>%
  set_names(names)
# Compute accuracy indices
GRF_accuracy_jumping <- map2(
  running_data, GRF_loocv_jumping,
  ~ accuracy(.x, GRF_formula, .y$actual, .y$predicted)
) %>%
  set_names(names)
# Bland-Altman plots
GRF_plots_jumping <- map2(GRF_loocv_jumping, names, bland_altman)
# Extract coefficients
GRF_coefficients_jumping <- map(GRF_models_jumping, get_coefficients)

# Jumping LR
# Build models
LR_models_jumping <- map(jumping_data, ~ lmer(LR_formula, .x)) %>%
  set_names(names)
# Cross validate (leave-one-out cross-validation)
LR_loocv_jumping <- map(jumping_data, ~ loocv(.x, LR_formula)) %>%
  set_names(names)
# Compute accuracy indices
LR_accuracy_jumping <- map2(
  running_data, LR_loocv_jumping,
  ~ accuracy(.x, LR_formula, .y$actual, .y$predicted)
) %>%
  set_names(names)
# Bland-Altman plots
LR_plots_jumping <- map2(LR_loocv_jumping, names, bland_altman)
# Extract coefficients
LR_coefficients_jumping <- map(LR_models_jumping, get_coefficients)

# Test differences between actual and predicted values --------------------

vectors <- c("resultant", "vertical")
test_formula <- as.formula("value ~ value_type * activity_type + (1 | subj)")

# Running GRF
GRF_test_running <- map_dfr(GRF_loocv_running, rbind) %>%
  prepare_data()
GRF_test_models_running <- map(
  vectors,
  ~ lmer(
    test_formula,
    data = GRF_test_running %>% filter(vector == .x)
  )
) %>%
  map(
    ~ anova(.x, type = 3, test = "F")
  ) %>%
  set_names(vectors)

# Running LR
LR_test_running <- map_dfr(LR_loocv_running, rbind) %>%
  prepare_data()
LR_test_models_running <- map(
  vectors,
  ~ lmer(
    test_formula,
    data = LR_test_running %>% filter(vector == .x)
  )
) %>%
  map(
    ~ anova(.x, type = 3, test = "F")
  ) %>%
  set_names(vectors)

# Jumping GRF
GRF_test_jumping <- map_dfr(GRF_loocv_jumping, rbind) %>%
  prepare_data()
GRF_test_models_jumping <- map(
  vectors,
  ~ lmer(
    test_formula,
    data = GRF_test_jumping %>% filter(vector == .x)
  )
) %>%
  map(
  ~ anova(.x, type = 3, test = "F")
) %>%
  set_names(vectors)

# Jumping LR
LR_test_jumping <- map_dfr(LR_loocv_jumping, rbind) %>%
  prepare_data()
LR_test_models_jumping <- map(
  vectors,
  ~ lmer(
    test_formula,
    data = LR_test_jumping %>% filter(vector == .x)
  )
) %>%
  map(
    ~ anova(.x, type = 3, test = "F")
  ) %>%
  set_names(vectors)
