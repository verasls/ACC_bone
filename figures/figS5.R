# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("output", "loocv_data.rda"))

# Ankle resultant pLR Bland-Altman plot -----------------------------------

BA_LR_res_ankle <- cv_res_LR_models$ankle %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-120000, 120000),
    expand = c(0, 0),
    breaks = seq(-120000, 120000, 40000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 260000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Resultant vector - Ankle placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Lower back resultant pLR Bland-Altman plot ------------------------------

BA_LR_res_back <- cv_res_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-120000, 120000),
    expand = c(0, 0),
    breaks = seq(-120000, 120000, 40000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 270000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Resultant vector - Lower back placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Hip resultant pLR Bland-Altman plot -------------------------------------

BA_LR_res_hip <- cv_res_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-80000, 80000),
    expand = c(0, 0),
    breaks = seq(-80000, 80000, 40000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 270000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Ankle vertical pLR Bland-Altman plot ------------------------------------

BA_LR_ver_ankle <- cv_ver_LR_models$ankle %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-120000, 120000),
    expand = c(0, 0),
    breaks = seq(-120000, 120000, 40000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 270000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Vertical vector - Ankle placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Lower back vertical pLR Bland-Altman plot -------------------------------

BA_LR_ver_back <- cv_ver_LR_models$lower_back %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-120000, 120000),
    expand = c(0, 0),
    breaks = seq(-120000, 120000, 40000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 270000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Vertical vector - Lower back placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Hip vertical pLR Bland-Altman plot --------------------------------------

BA_LR_ver_hip <- cv_ver_LR_models$hip %>%
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(-90000, 90000),
    expand = c(0, 0),
    breaks = seq(-90000, 90000, 30000)
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    limits = c(0, 270000),
    expand = c(0, 0),
    breaks = seq(50000, 250000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13)
  ) +
  guides(alpha = "none") +
  labs(
    title = "Vertical vector - Hip placement",
    x = quote("Mean of Actual and Predicted pLR" ~ (N %.% s^-1)),
    y = quote("Actual - Predicted pLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

figS3 <- BA_LR_res_ankle +
  BA_LR_res_back +
  BA_LR_res_hip +
  BA_LR_ver_ankle +
  BA_LR_ver_back +
  BA_LR_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "figS3.png"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS3)
dev.off()
