# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
library(patchwork)
library(ggsci)
source(here("code/functions/plot_functions.R"))

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/processed/running_data.csv")) %>%
  mutate(
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I"
      )
    )
  ) %>%
  select(subj, BMI_cat)

jumping_df <- read_csv(here("data/processed/jumping_data.csv")) %>%
  mutate(
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I",
        "Obesity class II", "Obesity class III"
      )
    )
  ) %>%
  select(subj, BMI_cat)

# LOOCV data
GRF_loocv_running <- read_csv(here("data/processed/GRF_loocv_running.csv")) %>%
  full_join(running_df)
GRF_loocv_jumping <- read_csv(here("data/processed/GRF_loocv_jumping.csv")) %>%
  full_join(jumping_df)

# GRF plots ---------------------------------------------------------------

# Running
GRF_loocv_running %>%
  filter(acc_placement == "ankle" & vector == "resultant") %>%
  publication_bland_altman() +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1500, 600),
    expand = c(0, 0),
    breaks = seq(-1500, 600, 300)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = "Mean of Actual and Predicted pRGRF (N)",
    y = "Actual - Predicted pRGRF (N)"
  )
