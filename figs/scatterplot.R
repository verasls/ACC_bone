# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
library(patchwork)
library(ggsci)

# Load data ---------------------------------------------------------------

running_df <- read_csv(here("data/running_data.csv")) %>%
  mutate(
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I"
      )
    )
  )

jumping_df <- read_csv(here("data/jumping_data.csv")) %>%
  mutate(
    BMI_cat = fct_relevel(
      as_factor(BMI_cat),
      levels = c(
        "Normal weight", "Overweight", "Obesity class I",
        "Obesity class II", "Obesity class III"
      )
    )
  )

# GRF x ACC plots ---------------------------------------------------------

# Running
scatter_ACC_running_ankle_res <- running_df %>%
  filter(acc_placement == "ankle" & vector == "resultant") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = "pRACC (g)", y = "pRGRF (N)")

scatter_ACC_running_back_res <- running_df %>%
  filter(acc_placement == "lower_back" & vector == "resultant") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower back", x = "pRACC (g)", y = "pRGRF (N)")

scatter_ACC_running_hip_res <- running_df %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = "pRACC (g)", y = "pRGRF (N)")

scatter_ACC_running_ankle_ver <- running_df %>%
  filter(acc_placement == "ankle" & vector == "vertical") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = "pVACC (g)", y = "pVGRF (N)")

scatter_ACC_running_back_ver <- running_df %>%
  filter(acc_placement == "lower_back" & vector == "vertical") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower back", x = "pVACC (g)", y = "pVGRF (N)")

scatter_ACC_running_hip_ver <- running_df %>%
  filter(acc_placement == "hip" & vector == "vertical") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 3500),
    expand = c(0, 0),
    breaks = seq(0, 3500, 500)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = "pVACC (g)", y = "pVGRF (N)")


# Jumping
scatter_ACC_jumping_hip_res <- jumping_df %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = pACC_g, y = pGRF_N, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 6000),
    expand = c(0, 0),
    breaks = seq(0, 6000, 500)
  ) +
  theme_light() +
  theme(legend.title = element_blank()) +
  labs(x = "pACC (g)", y = "pGRF (N)")

# Combine GRF x ACC plots -------------------------------------------------

# Running
scatter_ACC_running <- scatter_ACC_running_ankle_res +
  scatter_ACC_running_back_res +
  scatter_ACC_running_hip_res +
  scatter_ACC_running_ankle_ver +
  scatter_ACC_running_back_ver +
  scatter_ACC_running_hip_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figs/scatterplot_ACC_running.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatter_ACC_running)
dev.off()

# LR x ATR plots ----------------------------------------------------------

scatter_ATR_running_hip_res <- running_df %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 60000),
    expand = c(0, 0),
    breaks = seq(0, 60000, 10000)
  ) +
  theme_light() +
  theme(legend.title = element_blank()) +
  labs(x = "pATR (g/s)", y = "pLR (N/s)")

scatter_ATR_jumping_hip_res <- jumping_df %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 280000),
    expand = c(0, 0),
    breaks = seq(0, 280000, 50000)
  ) +
  theme_light() +
  theme(legend.title = element_blank()) +
  labs(x = "pATR (g/s)", y = "pLR (N/s)")
