# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
library(patchwork)

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/processed/running_data.csv")) %>%
  mutate(
    run = as_factor(paste0("running ", speed, "km/h")),
  ) %>%
  select(
    subj, acc_placement, vector, run, pGRF_BW, pACC_g, pLR_BWs, pATR_gs
  )
jumping_df <- read_csv(here("data/processed/jumping_data.csv")) %>%
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
    acc_placement = recode(
      as_factor(acc_placement),
      "ankle" = "Ankle",
      "lower_back" = "Lower Back",
      "hip" = "Hip"
    ),
    activity = ifelse(
      is.na(as.character(jump)), as.character(run), as.character(jump)
    ),
    activity = str_to_sentence(activity),
    activity = as_factor(activity),
    activity = fct_relevel(activity, "drop jumps 40cm", after = 15)
  )

# GRF boxplot -------------------------------------------------------------

boxplot_GRF_res <- data %>%
  filter(acc_placement == "Lower Back" & vector == "resultant") %>%
  ggplot(aes(x = activity, y = pGRF_BW)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  labs(x = "", y = "pRGRF (BW)")

boxplot_GRF_ver <- data %>%
  filter(acc_placement == "Lower Back" & vector == "vertical") %>%
  ggplot(aes(x = activity, y = pGRF_BW)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  labs(x = "", y = "pVGRF (BW)")

# ACC boxplots ------------------------------------------------------------

boxplot_ACC_res <- data %>%
  filter(vector == "resultant" & acc_placement != "Ankle") %>%
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(x = "", y = "pRACC (g)")

boxplot_ACC_ver <- data %>%
  filter(vector == "vertical" & acc_placement != "Ankle") %>%
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(x = "", y = "pVACC (g)")

# LR boxplot --------------------------------------------------------------

boxplot_LR_res <- data %>%
  filter(acc_placement == "Lower Back" & vector == "resultant") %>%
  ggplot(aes(x = activity, y = pLR_BWs)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  labs(x = "", y = quote("pRLR" ~ (BW %.% s^-1)))

boxplot_LR_ver <- data %>%
  filter(acc_placement == "Lower Back" & vector == "vertical") %>%
  ggplot(aes(x = activity, y = pLR_BWs)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  labs(x = "", y = quote("pVLR" ~ (BW %.% s^-1)))

# ATR boxplots ------------------------------------------------------------

boxplot_ATR_res <- data %>%
  filter(vector == "resultant" & acc_placement != "Ankle") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
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
    legend.position = "right"
  ) +
  labs(x = "", y = quote("pRATR" ~ (g %.% s^-1)))

boxplot_ATR_ver <- data %>%
  filter(vector == "vertical" & acc_placement != "Ankle") %>%
  ggplot(aes(x = activity, y = pATR_gs)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
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
    legend.position = "right"
  ) +
  labs(x = "", y = quote("pVATR" ~ (g %.% s^-1)))

# Combine and save plots --------------------------------------------------

boxplot_GRF_fig <- boxplot_GRF_res + boxplot_ACC_res +
  plot_annotation(tag_levels = "A")
agg_tiff(
  here("figs/boxplot_GRF_fig.tiff"),
  width = 60,
  height = 25,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(boxplot_GRF_fig)
dev.off()

boxplot_GRF_suppl <- boxplot_GRF_res + theme(axis.text.x = element_blank()) +
  boxplot_ACC_res + theme(axis.text.x = element_blank()) +
  boxplot_GRF_ver + boxplot_ACC_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")
agg_tiff(
  here("figs/boxplot_GRF_suppl.tiff"),
  width = 60,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(boxplot_GRF_suppl)
dev.off()

boxplot_LR_suppl <- boxplot_LR_res + theme(axis.text.x = element_blank()) +
  boxplot_ATR_res + theme(axis.text.x = element_blank()) +
  boxplot_LR_ver + boxplot_ATR_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")
agg_tiff(
  here("figs/boxplot_LR_suppl.tiff"),
  width = 60,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(boxplot_LR_suppl)
dev.off()
