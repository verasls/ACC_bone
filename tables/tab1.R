# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(broman)
library(kableExtra)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "prediction_models.rda"))
load(here("output", "loocv_data.rda"))

# Build table -------------------------------------------------------------

res_GRF_tb <- build_formula_table(
  res_GRF_models, cv_res_GRF_models, "pGRF", "resultant"
)
ver_GRF_tb <- build_formula_table(
  ver_GRF_models, cv_ver_GRF_models, "pGRF", "vertical"
)
res_LR_tb <- build_formula_table(
  res_LR_models, cv_res_LR_models, "pLR", "resultant"
) %>%
  mutate(MAE = round(as.numeric(MAE), 0), RMSE = round(as.numeric(RMSE), 0))
ver_LR_tb <- build_formula_table(
  ver_LR_models, cv_ver_LR_models, "pLR", "vertical"
) %>%
  mutate(MAE = round(as.numeric(MAE), 0), RMSE = round(as.numeric(RMSE), 0))

tab1_df <- rbind(res_GRF_tb, ver_GRF_tb, res_LR_tb, ver_LR_tb)
# Remove vector name duplicates
tab1_df[c(2, 3, 5, 6, 8, 9, 11, 12), 1] <- ""

tab1 <- knitr::kable(
  tab1_df, booktabs = TRUE, escape = FALSE, linesep = "",
  label = "none",
  caption = "Regression equations, $R^2$ and accuracy indices",
  col.names = c(
    "Vector",
    linebreak("Accelerometer\nplacement"),
    "Regression equations",
    "$R^2$",
    "MAE",
    "MAPE",
    "RMSE"
  ),
  align = c(rep("l", 3), rep("r", 4))
) %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; pACC, peak acceleration; pAR, peak acceleration rate; pGRF, peak ground reaction force; pLR, peak loading rate; RMSE, root mean square error",
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = "scale_down") %>%
  pack_rows(
    "Peak ground reaction force prediction", 1, 6,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  pack_rows(
    "Peak loading rate prediction", 7, 12,
    latex_gap_space = "0.5em",
    bold = FALSE
  ) %>%
  landscape()


tab1_html <- tab1_df %>%
  kbl(caption = "Regession equations, $R^2$ and accuracy indices") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; pACC, peak acceleration; pAR, peak acceleration rate; pGRF, peak ground reaction force; pLR, peak loading rate; RMSE, root mean square error",
    general_title = ""
  )
