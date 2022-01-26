# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(broman)
library(kableExtra)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "veras2020_accuracy.rda"))

# Build table -------------------------------------------------------------

tab2_df <- list(
  lower_back_res_GRF_accuracy,
  hip_res_GRF_accuracy,
  lower_back_ver_GRF_accuracy,
  hip_ver_GRF_accuracy,
  lower_back_res_LR_accuracy,
  hip_res_LR_accuracy,
  lower_back_ver_LR_accuracy,
  hip_ver_LR_accuracy
) |>
  map_dfr(~ mutate(.x, MAPE = as.numeric(MAPE) * 100)) |>
  mutate(
    across(where(is.numeric), myround, 1),
    MAPE = paste0(MAPE, "\\%"),
    Vector = rep(c("Resultant", "", "Vertical", ""), 2),
    `Accelerometer placement` = rep(c("Lower back", "Hip"), 4),
    .before = MAE
  )

tab2 <- knitr::kable(
  tab2_df, booktabs = TRUE, escape = FALSE, linesep = "",
  label = "none",
  caption = "Regression equations, $R^2$ and accuracy indices",
  col.names = c(
    "Vector",
    linebreak("Accelerometer\nplacement"),
    "MAE",
    "MAPE",
    "RMSE"
  ),
  align = c(rep("l", 2), rep("r", 3))
) %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; RMSE, root mean square error",
    general_title = "",
    threeparttable = TRUE
  ) %>%
  # kable_styling(latex_options = "scale_down") %>%
  pack_rows(
    "Peak ground reaction force prediction", 1, 4,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  pack_rows(
    "Peak loading rate prediction", 5, 8,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  landscape()
