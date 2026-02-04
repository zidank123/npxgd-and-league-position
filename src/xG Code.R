# xG and League Position in the Premier League
# Author: Kareem Zidan
# Data source: Understat

library(tidyverse)
library(haven)
library(janitor)
library(estimatr)
library(dplyr)
library(readxl)

data <- read_excel("2015-25_Prem_Data.xlsx")

n_seasons <- nrow(data) / 20

data <- data %>%
  mutate(
    season = rep(
      seq(2015, 2015 + n_seasons - 1),
      each = 20
    )
  )

head(data)
summary(data)

mean(data$NPxG)
mean(data$NPxGA)
mean(data$NPxGD)

baseline_model <- lm_robust(number ~ NPxGD, data=data)
summary(baseline_model)

FE_model <- lm_robust(number ~ NPxGD + factor(season), data=data)
summary(FE_model)

library(ggplot2)

ggplot(data, aes(x = NPxGD, y = number)) +
  geom_point(alpha = 0.6, position = position_jitter(height = 0.25)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_y_reverse(
    breaks = 1:20
  ) +
  labs(
    title = "Relationship Between Non-Penalty Expected Goal Difference and League Position",
    x = "Non-Pen xG Difference (NPxGD)",
    y = "Final League Position",
    caption = "Each point represents a team-season observation (Premier League, 2014–15 to 2024–25)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("npxgd_vs_rank.png", width = 8, height = 5, dpi = 300)