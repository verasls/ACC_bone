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
    across(c(acc_placement, vector, jump_type), as_factor),
    jump = as_factor(
      str_to_sentence(paste0(jump_type, " ", jump_height, "cm"))
    ),
    jump = fct_relevel(jump, "Drop jumps 40cm", after = 7),
    jump = fct_relevel(
      jump, "Continuous jumps 5cm", "Continuous jumps 15cm", after = Inf
    ),
    .after = jump_height
  )

# Export data as rda ------------------------------------------------------

save(mechanical_load_data, file = here("data", "mechanical_load_data.rda"))
