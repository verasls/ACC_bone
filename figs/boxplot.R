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
    subj, acc_placement, vector, run, pGRF_BW, pACC_g, pLR_BWs, pATR_gs
  )
jumping_df <- read_csv(here("data/jumping_data.csv")) %>%
  mutate(
    jump = as_factor(paste0(jump_type, " ", jump_height, "cm")),
    jump = fct_relevel(jump, "drop jumps 40cm", after = 7),
  ) %>%
  select(
    subj, acc_placement, vector, jump, pGRF_BW, pACC_g, pLR_BWs, pATR_gs
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
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  labs(x = "", y = "pGRF (BW)")

# ACC boxplots ------------------------------------------------------------

boxplot_ACC_ankle <- data %>%
  filter(acc_placement == "ankle") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 16),
    expand = c(0, 0),
    breaks = seq(0, 16, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pACC (g)")

boxplot_ACC_back <- data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 13),
    expand = c(0, 0),
    breaks = seq(0, 13, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pACC (g)")

boxplot_ACC_hip <- data %>%
  filter(acc_placement == "hip") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
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

# LR boxplot --------------------------------------------------------------

boxplot_LR <- data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pLR_BWs)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  labs(x = "", y = "pLR (BW/s)")

# ATR boxplots ------------------------------------------------------------

boxplot_ATR_ankle <- data %>%
  filter(acc_placement == "ankle") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pATR (g/s)")

boxplot_ATR_back <- data %>%
  filter(acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 650),
    expand = c(0, 0),
    breaks = seq(0, 650, 50)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pATR (g/s)")

boxplot_ATR_hip <- data %>%
  filter(acc_placement == "hip") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = vector), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 650),
    expand = c(0, 0),
    breaks = seq(0, 650, 50)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(x = "", y = "pATR (g/s)")

# Combine boxplots --------------------------------------------------------

a <- boxplot_GRF + boxplot_ACC_back + boxplot_ACC_hip +
  boxplot_LR + boxplot_ATR_back + boxplot_ATR_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(nrow = 3, byrow = FALSE, guides = "collect") &
  theme(legend.position = "bottom")

agg_tiff(
  here("figs/boxplot.tiff"),
  width = 80,
  height = 80,
  units = "cm",
  res = 100,
  scaling = 3
)
plot(a)
dev.off()

# Try another combination -------------------------------------------------

boxplot_GRF_2 <- data %>%
  filter(vector == "Resultant" & acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pGRF_BW)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(axis.text.x = element_blank()) +
  labs(x = "", y = "pGRF (BW)")

boxplot_LR_2 <- data %>%
  filter(vector == "Resultant" & acc_placement == "lower_back") %>%
  ggplot(aes(x = activity, y = pLR_BWs)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "pLR (BW/s)")

boxplot_ACC <- data %>%
  filter(vector == "Resultant" & acc_placement != "ankle") %>%
  ggplot(aes(x = activity, y = pACC_g)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 13),
    expand = c(0, 0),
    breaks = seq(0, 13, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(x = "", y = "pACC (g)")

boxplot_ATR <- data %>%
  filter(vector == "Resultant" & acc_placement != "ankle") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 650),
    expand = c(0, 0),
    breaks = seq(0, 650, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(x = "", y = "pATR (g/s)")

b <- boxplot_GRF_2 + boxplot_ACC + boxplot_LR_2 + boxplot_ATR +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

agg_tiff(
  here("figs/boxplot_2.tiff"),
  width = 80,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(b)
dev.off()
