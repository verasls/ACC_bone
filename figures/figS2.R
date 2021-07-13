# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("output", "loocv_data.rda"))

# Ankle resultant pGRF Bland-Altman plot ----------------------------------

BA_GRF_res_ankle <- cv_res_GRF_models$ankle |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-2000, 2000),
    expand = c(0, 0),
    breaks = seq(-2000, 2000, 500)
  ) +
  scale_x_continuous(
    limits = c(1000, 5600),
    expand = c(0, 0),
    breaks = seq(1500, 5500, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Lower back resultant pGRF Bland-Altman plot -----------------------------

BA_GRF_res_back <- cv_res_GRF_models$lower_back |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1600, 1600),
    expand = c(0, 0),
    breaks = seq(-1600, 1600, 400)
  ) +
  scale_x_continuous(
    limits = c(1000, 5600),
    expand = c(0, 0),
    breaks = seq(1500, 5500, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Hip resultant pGRF Bland-Altman plot ------------------------------------

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

# Ankle vertical pGRF Bland-Altman plot -----------------------------------

BA_GRF_ver_ankle <- cv_ver_GRF_models$ankle |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-2000, 2000),
    expand = c(0, 0),
    breaks = seq(-2000, 2000, 500)
  ) +
  scale_x_continuous(
    limits = c(1000, 5600),
    expand = c(0, 0),
    breaks = seq(1500, 5500, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Lower back vertical pGRF Bland-Altman plot ------------------------------

BA_GRF_ver_back <- cv_ver_GRF_models$lower_back |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1600, 1600),
    expand = c(0, 0),
    breaks = seq(-1600, 1600, 400)
  ) +
  scale_x_continuous(
    limits = c(1000, 5600),
    expand = c(0, 0),
    breaks = seq(1500, 5500, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Hip vertical pGRF Bland-Altman plot -------------------------------------

BA_GRF_ver_hip <- cv_ver_GRF_models$hip |>
  plot_bland_altman(color = BMI_cat, alpha = 0.5) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(-1500, 1500),
    expand = c(0, 0),
    breaks = seq(-1500, 1500, 500)
  ) +
  scale_x_continuous(
    limits = c(1000, 5600),
    expand = c(0, 0),
    breaks = seq(1500, 5500, 500)
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
    x = "Mean of Actual and Predicted pGRF (N)",
    y = "Actual - Predicted pGRF (N)"
  )

# Combine and save plots --------------------------------------------------

figS2 <- BA_GRF_res_ankle +
  BA_GRF_res_back +
  BA_GRF_res_hip +
  BA_GRF_ver_ankle +
  BA_GRF_ver_back +
  BA_GRF_ver_hip +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 16)
  )

agg_png(
  here("figures", "figS2.png"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS2)
dev.off()
