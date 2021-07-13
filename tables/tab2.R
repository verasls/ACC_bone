# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(kableExtra)
source(here("code", "funs.R"))

# Load and prepare data ---------------------------------------------------

load(here("output", "jump_type_accuracy.rda"))

# Build table -------------------------------------------------------------

# GRF table
res_GRF_tab2_df <- build_accuracy_table(
  drop_jumps_res_GRF_accuracy, "resultant", "drop"
) |>
  left_join(
    build_accuracy_table(box_jumps_res_GRF_accuracy, "resultant", "box")
  ) |>
  left_join(
    build_accuracy_table(
      continuous_jumps_res_GRF_accuracy, "resultant", "continuous"
    )
  )
ver_GRF_tab2_df <- build_accuracy_table(
  drop_jumps_ver_GRF_accuracy, "vertical", "drop"
) |>
  left_join(
    build_accuracy_table(box_jumps_ver_GRF_accuracy, "vertical", "box")
  ) |>
  left_join(
    build_accuracy_table(
      continuous_jumps_ver_GRF_accuracy, "vertical", "continuous"
    )
  )
GRF_tab2_df <- rbind(res_GRF_tab2_df, ver_GRF_tab2_df)
# LR table
res_LR_tab2_df <- build_accuracy_table(
  drop_jumps_res_LR_accuracy, "resultant", "drop"
) |>
  left_join(
    build_accuracy_table(box_jumps_res_LR_accuracy, "resultant", "box")
  ) |>
  left_join(
    build_accuracy_table(
      continuous_jumps_res_LR_accuracy, "resultant", "continuous"
    )
  )
ver_LR_tab2_df <- build_accuracy_table(
  drop_jumps_ver_LR_accuracy, "vertical", "drop"
) |>
  left_join(
    build_accuracy_table(box_jumps_ver_LR_accuracy, "vertical", "box")
  ) |>
  left_join(
    build_accuracy_table(
      continuous_jumps_ver_LR_accuracy, "vertical", "continuous"
    )
  )
LR_tab2_df <- rbind(res_LR_tab2_df, ver_LR_tab2_df)
# Join both tables
tab2_df <- rbind(GRF_tab2_df, LR_tab2_df)
# Remove vector name duplicates
tab2_df[c(2, 3, 5, 6, 8, 9, 11, 12), 1] <- ""

tab2_html <- tab2_df %>%
  kbl(caption = "$R^2$ and accuracy indices per jump type") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  footnote(
    general = "Abbreviations: MAE, mean absolute error; MAPE, mean absolute percent error; pACC, peak acceleration; pAR, peak acceleration rate; pGRF, peak ground reaction force; pLR, peak loading rate; RMSE, root mean square error",
    general_title = ""
  )
