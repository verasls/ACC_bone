# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/running_data.csv")) %>%
  mutate(
    run = as_factor(paste0("running ", speed, "km/h")),
  ) %>%
  select(
    subj, acc_placement, vector, run, pGRF_BW
  )
jumping_df <- read_csv(here("data/jumping_data.csv")) %>%
  mutate(
    jump = as_factor(paste0(jump_type, " ", jump_height, "cm")),
    jump = fct_relevel(jump, "drop jumps 40cm", after = 7),
  ) %>%
  select(
    subj, acc_placement, vector, jump, pGRF_BW
  )

data <- running_df %>%
  full_join(jumping_df) %>%
  mutate(
    acc_placement = as_factor(acc_placement),
    vector = recode(
      as_factor(vector),
      "vertical" = "Vertical",
      "resultant" = "Resultant"
    ),
    vector = fct_relevel(
      as_factor(vector),
      levels = c("Vertical", "Resultant")
    ),
    activity = ifelse(
      is.na(as.character(jump)), as.character(run), as.character(jump)
    ),
    activity = as_factor(activity),
    activity = fct_relevel(activity, "drop jumps 40cm", after = 15)
  )

# GRF boxplot -------------------------------------------------------------

boxplot <- data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pGRF_BW)) +
  geom_boxplot(aes(fill = vector), outlier.shape = NA) +
  geom_jitter(size = 0.4, alpha = 0.6) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  labs(x = "", y = "pGRF (BW)")

agg_tiff(
  here("figs/boxplot.tiff"),
  width = 40,
  height = 20,
  units = "cm",
  res = 300
)
plot(boxplot)
dev.off()
