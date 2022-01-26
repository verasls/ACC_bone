# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(ggsci)
library(patchwork)
library(ragg)

# Load and prepare data ---------------------------------------------------

load(here("output", "veras2020_predictions.rda"))
load(here("output", "loocv_data.rda"))

plot_data_GRF <- cv_res_GRF_models$hip |>
  left_join(
    select(
      hip_res_GRF_df,
      id, trial, filename, subj, acc_placement, vector,
      jump_type, jump_height, .predicted_Veras2020
    )
  ) |>
  select(pACC_g, .predicted, .predicted_Veras2020) |>
  pivot_longer(
    cols = starts_with("."),
    names_to = "equation",
    values_to = "pGRF_N"
  ) |>
  mutate(
    equation = recode_factor(
      as.factor(equation),
      .predicted = "Jumping equation",
      .predicted_Veras2020 = "Walking equation"
    )
  )

plot_data_LR <- cv_res_LR_models$hip |>
  left_join(
    select(
      hip_res_LR_df,
      id, trial, filename, subj, acc_placement, vector,
      jump_type, jump_height, .predicted_Veras2020
    )
  ) |>
  select(pAR_gs, .predicted, .predicted_Veras2020) |>
  pivot_longer(
    cols = starts_with("."),
    names_to = "equation",
    values_to = "pLR_Ns"
  ) |>
  mutate(
    equation = recode_factor(
      as.factor(equation),
      .predicted = "Jumping equation",
      .predicted_Veras2020 = "Walking equation"
    )
  )

# Plot GRF predictions ----------------------------------------------------

predicted_GRF_plot <- ggplot(plot_data_GRF) +
  geom_point(
    aes(x = pACC_g, y = pGRF_N, color = equation),
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#0072B5", "#BC3C29")) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-15000, 10000),
    breaks = seq(-15000, 10000, 5000)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 14),
    breaks = seq(0, 14, 2)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("pACC" ~ (italic(g))),
    y = "Predicted pGRF (N)"
  )

# Plot GRF predictions ----------------------------------------------------

predicted_LR_plot <- ggplot(plot_data_LR) +
  geom_point(
    aes(x = pAR_gs, y = pLR_Ns, color = equation),
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#0072B5", "#BC3C29")) +
  scale_y_continuous(
    labels = scales::label_number(),
    expand = c(0, 0),
    limits = c(-50000, 300000),
    breaks = seq(-50000, 300000, 50000)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 600),
    breaks = seq(0, 600, 100)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    plot.margin = margin(r = 1, unit = "cm")
  ) +
  labs(
    title = "Resultant vector - Hip placement",
    x = quote("pAR" ~ (italic(g) %.% s^-1)),
    y = quote("Predicted pLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

fig3 <- predicted_GRF_plot + predicted_LR_plot +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 17)
  )

agg_png(
  here("figures", "fig3.png"),
  width = 80,
  height = 25,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig3)
dev.off()
