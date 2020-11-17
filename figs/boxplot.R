# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
library(patchwork)

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/running_data.csv")) %>%
  mutate(
    run = as_factor(paste0("running ", speed, "km/h")),
  ) %>%
  select(
    subj, acc_placement, vector, run, pGRF_BW, pACC_g
  )
jumping_df <- read_csv(here("data/jumping_data.csv")) %>%
  mutate(
    jump = as_factor(paste0(jump_type, " ", jump_height, "cm")),
    jump = fct_relevel(jump, "drop jumps 40cm", after = 7),
  ) %>%
  select(
    subj, acc_placement, vector, jump, pGRF_BW, pACC_g
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

boxplot_GRF <- data %>%
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
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  labs(x = "", y = "pGRF (BW)")

agg_tiff(
  here("figs/boxplot_GRF.tiff"),
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  scaling = 1.5
)
plot(boxplot_GRF)
dev.off()

# ACC boxplots ------------------------------------------------------------

boxplot_ACC_ankle <- data %>%
  filter(acc_placement == "ankle") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.shape = NA) +
  geom_jitter(size = 0.4, alpha = 0.6) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 16),
    expand = c(0, 0),
    breaks = seq(0, 16, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pACC (g)")

boxplot_ACC_back <- data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.shape = NA) +
  geom_jitter(size = 0.4, alpha = 0.6) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 13),
    expand = c(0, 0),
    breaks = seq(0, 13, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pACC (g)")

boxplot_ACC_hip <- data %>%
  filter(acc_placement == "hip") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.shape = NA) +
  geom_jitter(size = 0.4, alpha = 0.6) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 13),
    expand = c(0, 0),
    breaks = seq(0, 13, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pACC (g)")

agg_tiff(
  here("figs/boxplot_ACC_ankle.tiff"),
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  scaling = 1.5
)
plot(boxplot_ACC_ankle)
dev.off()

agg_tiff(
  here("figs/boxplot_ACC_back.tiff"),
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  scaling = 1.5
)
plot(boxplot_ACC_back)
dev.off()

agg_tiff(
  here("figs/boxplot_ACC_hip.tiff"),
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  scaling = 1.5
)
plot(boxplot_ACC_hip)
dev.off()

# Combined GRF/ACC boxplot ------------------------------------------------

a <- boxplot_GRF + boxplot_ACC_ankle + boxplot_ACC_back + boxplot_ACC_hip +
  plot_layout(nrow = 1, byrow = FALSE, guides = "collect") &
  theme(legend.position = "top")

agg_tiff(
  here("figs/boxplot.tiff"),
  width = 120,
  height = 20,
  units = "cm",
  res = 300,
  scaling = 1.5
)
plot(a)
dev.off()
