# Load packages -----------------------------------------------------------

library(tidyverse)

# Obtain data -------------------------------------------------------------

data_dir <- "/Volumes/LV_HD_2/Accelerometry/GRF_ACC/"
data_dir <- list.files(
  paste0(data_dir, list.files(data_dir)),
  pattern = "*Plataforma*",
  full.names = TRUE
)

files <- list.files(
  data_dir,
  pattern = "*extracted*",
  full.names = TRUE
)

df <- files %>% 
  map_dfr(read_csv)
