# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("output", "loocv_data.rda"))

# Hip pGRF Bland-Altman plot ---------------------------------------------

BA_GRF_res_hip <- cv_res_GRF_models$hip |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1500, 1500),
    expand = c(0, 0),
    breaks = seq(-1500, 1500, 500)
  ) +
  scale_x_continuous(
    limits = c(1000, 6100),
    expand = c(0, 0),
    breaks = seq(1500, 6000, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Hip pLR Bland-Altman plot ----------------------------------------------

BA_LR_res_hip <- cv_res_LR_models$hip |>
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

# Combine and save plots --------------------------------------------------

fig2 <- BA_GRF_res_hip +
  BA_LR_res_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "fig2.png"),
  width = 80,
  height = 25,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig2)
dev.off()
