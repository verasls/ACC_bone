# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)

# Read anthropometric data ------------------------------------------------

anthropometric <- read_csv(here("data", "anthropometric_data.csv")) |>
  select(id = ID, everything()) |>
  mutate(
    BMI = bmi(body_mass, height / 100),
    BMI_cat = bmi_cat(BMI)
  )

# Read jumping data -------------------------------------------------------

mechanical_load_data <- read_csv(here("data", "jumping_data.csv")) |>
  select(-c(body_mass, BMI, BMI_cat)) |>
  left_join(anthropometric, by = c("id", "trial")) |>
  mutate(
    across(c(acc_placement, vector, jump_type), as_factor)
  )

# Export data as rda ------------------------------------------------------

save(mechanical_load_data, file = here("data", "mechanical_load_data.rda"))
