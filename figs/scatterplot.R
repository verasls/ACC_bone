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
  labs(title = "Lower Back", x = "pRACC (g)", y = "pRGRF (N)")

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
  labs(title = "Lower Back", x = "pVACC (g)", y = "pVGRF (N)")

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
scatter_ACC_jumping_ankle_res <- jumping_df %>%
  filter(acc_placement == "ankle" & vector == "resultant") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = "pRACC (g)", y = "pRGRF (N)")

scatter_ACC_jumping_back_res <- jumping_df %>%
  filter(acc_placement == "lower_back" & vector == "resultant") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower Back", x = "pRACC (g)", y = "pRGRF (N)")

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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = "pRACC (g)", y = "pRGRF (N)")

scatter_ACC_jumping_ankle_ver <- jumping_df %>%
  filter(acc_placement == "ankle" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Ankle", x = "pVACC (g)", y = "pVGRF (N)")

scatter_ACC_jumping_back_ver <- jumping_df %>%
  filter(acc_placement == "lower_back" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Lower Back", x = "pVACC (g)", y = "pVGRF (N)")

scatter_ACC_jumping_hip_ver <- jumping_df %>%
  filter(acc_placement == "hip" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(title = "Hip", x = "pVACC (g)", y = "pVGRF (N)")

# LR x ATR plots ----------------------------------------------------------

# Running
scatter_ATR_running_ankle_res <- running_df %>%
  filter(acc_placement == "ankle" & vector == "resultant") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

scatter_ATR_running_back_res <- running_df %>%
  filter(acc_placement == "lower_back" & vector == "resultant") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

scatter_ATR_running_ankle_ver <- running_df %>%
  filter(acc_placement == "ankle" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

scatter_ATR_running_back_ver <- running_df %>%
  filter(acc_placement == "lower_back" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

scatter_ATR_running_hip_ver <- running_df %>%
  filter(acc_placement == "hip" & vector == "vertical") %>%
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
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

# Jumping
scatter_ATR_jumping_ankle_res <- jumping_df %>%
  filter(acc_placement == "ankle" & vector == "resultant") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

scatter_ATR_jumping_back_res <- jumping_df %>%
  filter(acc_placement == "lower_back" & vector == "resultant") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

scatter_ATR_jumping_hip_res <- jumping_df %>%
  filter(acc_placement == "hip" & vector == "resultant") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pRATR" ~ (g %.% s^-1)),
    y = quote("pRLR" ~ (N %.% s^-1))
  )

scatter_ATR_jumping_ankle_ver <- jumping_df %>%
  filter(acc_placement == "ankle" & vector == "vertical") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Ankle",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

scatter_ATR_jumping_back_ver <- jumping_df %>%
  filter(acc_placement == "lower_back" & vector == "vertical") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Lower Back",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

scatter_ATR_jumping_hip_ver <- jumping_df %>%
  filter(acc_placement == "hip" & vector == "vertical") %>%
  ggplot(aes(x = pATR_gs, y = pLR_Ns, color = BMI_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_nejm() +
  scale_y_continuous(
    limits = c(0, 300000),
    expand = c(0, 0),
    breaks = seq(0, 300000, 50000)
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  labs(
    title = "Hip",
    x = quote("pVATR" ~ (g %.% s^-1)),
    y = quote("pVLR" ~ (N %.% s^-1))
  )

# Combine and save plots --------------------------------------------------

# Running GFR x ACC
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
  here("figs/scatterplot_GRF_ACC_running.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatter_ACC_running)
dev.off()

# Running LR x ATR
scatter_ATR_running <- scatter_ATR_running_ankle_res +
  scatter_ATR_running_back_res +
  scatter_ATR_running_hip_res +
  scatter_ATR_running_ankle_ver +
  scatter_ATR_running_back_ver +
  scatter_ATR_running_hip_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figs/scatterplot_LR_ATR_running.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatter_ATR_running)
dev.off()

# Jumping GFR x ACC
scatter_ACC_jumping <- scatter_ACC_jumping_ankle_res +
  scatter_ACC_jumping_back_res +
  scatter_ACC_jumping_hip_res +
  scatter_ACC_jumping_ankle_ver +
  scatter_ACC_jumping_back_ver +
  scatter_ACC_jumping_hip_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figs/scatterplot_GRF_ACC_jumping.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatter_ACC_jumping)
dev.off()

# Jumping LR x ATR
scatter_ATR_jumping <- scatter_ATR_jumping_ankle_res +
  scatter_ATR_jumping_back_res +
  scatter_ATR_jumping_hip_res +
  scatter_ATR_jumping_ankle_ver +
  scatter_ATR_jumping_back_ver +
  scatter_ATR_jumping_hip_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
agg_tiff(
  here("figs/scatterplot_LR_ATR_jumping.tiff"),
  width = 120,
  height = 50,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(scatter_ATR_jumping)
dev.off()
