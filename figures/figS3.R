# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
library(patchwork)

# Load data ---------------------------------------------------------------

load(here("data", "mechanical_load_data.rda"))
mechanical_load_data <- mechanical_load_data |>
  mutate(
    acc_placement = recode_factor(
      acc_placement,
      hip = "Hip",
      lower_back = "Lower back",
      ankle = "Ankle"
    )
  )

# Add new jump values to separate the boxplot by jump types
mechanical_load_data <- mechanical_load_data |>
  # filter(acc_placement == "Lower back" & vector == "resultant") |>
  add_row(
    acc_placement = rep(c("Ankle", "Lower back", "Hip"), 4),
    vector = c(rep("resultant", 6), rep("vertical", 6)),
    jump = rep(c(rep("1", 3), rep("2", 3)), 2),
    .before = 1
  ) |>
  mutate(
    jump = fct_relevel(
      as.factor(jump),
      levels = c(
        "Drop jumps 5cm", "Drop jumps 10cm", "Drop jumps 15cm",
        "Drop jumps 20cm", "Drop jumps 25cm", "Drop jumps 30cm",
        "Drop jumps 35cm", "Drop jumps 40cm", "1",
        "Box jumps 5cm", "Box jumps 15cm", "Box jumps 30cm", "2",
        "Continuous jumps 5cm", "Continuous jumps 15cm"
      )
    ),
    acc_placement = fct_relevel(
      as.factor(acc_placement),
      levels = c("Hip", "Lower back", "Ankle")
    )
  )

# GRF boxplot -------------------------------------------------------------

boxplot_GRF_ver <- mechanical_load_data |>
  filter(acc_placement == "Lower back" & vector == "vertical") |>
  ggplot(aes(x = jump, y = pGRF_BW)) +
  geom_boxplot() +
  scale_y_continuous(
    limits = c(0, 7),
    expand = c(0, 0),
    breaks = seq(0, 7, 1)
  ) +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "", y = "pGRF (BW)")

# ACC boxplots ------------------------------------------------------------

boxplot_ACC_ver <- mechanical_load_data |>
  filter(vector == "vertical") |>
  ggplot(aes(x = jump, y = pACC_g)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
  scale_fill_manual(values = c("gray90", "gray70", "gray50")) +
  scale_x_discrete(
    labels = c(
      "Drop jumps 5cm", "Drop jumps 10cm", "Drop jumps 15cm",
      "Drop jumps 20cm", "Drop jumps 25cm", "Drop jumps 30cm",
      "Drop jumps 35cm", "Drop jumps 40cm", " ",
      "Box jumps 5cm", "Box jumps 15cm", "Box jumps 30cm", " ",
      "Continuous jumps 5cm", "Continuous jumps 15cm"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 14),
    expand = c(0, 0),
    breaks = seq(0, 14, 2)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank()
  ) +
  labs(x = "", y = quote("pACC" ~ (italic(g))))

# LR boxplot --------------------------------------------------------------

boxplot_LR_ver <- mechanical_load_data |>
  filter(acc_placement == "Lower back" & vector == "vertical") |>
  ggplot(aes(x = jump, y = pLR_BWs)) +
  geom_boxplot() +
  scale_y_continuous(
    limits = c(0, 350),
    expand = c(0, 0),
    breaks = seq(0, 350, 50)
  ) +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "", y = quote("pLR" ~ (BW %.% s^-1)))

# AR boxplots -------------------------------------------------------------

boxplot_AR_ver <- mechanical_load_data |>
  filter(vector == "vertical") |>
  ggplot(aes(x = jump, y = pAR_gs)) +
  geom_boxplot(aes(fill = acc_placement), outlier.size = 0.8) +
  scale_fill_manual(values = c("gray90", "gray70", "gray50")) +
  scale_x_discrete(
    labels = c(
      "Drop jumps 5cm", "Drop jumps 10cm", "Drop jumps 15cm",
      "Drop jumps 20cm", "Drop jumps 25cm", "Drop jumps 30cm",
      "Drop jumps 35cm", "Drop jumps 40cm", " ",
      "Box jumps 5cm", "Box jumps 15cm", "Box jumps 30cm", " ",
      "Continuous jumps 5cm", "Continuous jumps 15cm"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 700),
    expand = c(0, 0),
    breaks = seq(0, 700, 100)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank(),
  ) +
  labs(x = "", y = quote("pAR" ~ (italic(g) %.% s^-1)))

# Combine and save plots --------------------------------------------------

figS3 <- boxplot_GRF_ver + theme(axis.text.x = element_blank()) +
  boxplot_LR_ver + theme(axis.text.x = element_blank()) +
  boxplot_ACC_ver +
  boxplot_AR_ver +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 17)
  )

agg_png(
  here("figures", "figS3.png"),
  width = 90,
  height = 80,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(figS3)
dev.off()
