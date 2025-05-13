# Load required libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(lme4)

# Load data
load("throughput/processed-data.RData")

# ----------------------
# Functions
# ----------------------

plot_water_ci <- function(conf_df, title = "Water Ratings with 90% Confidence Intervals") {
  ggplot(conf_df, aes(x = estimate, y = name)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    labs(
      title = title,
      x = "Estimated Rating",
      y = "Water Type"
    ) +
    theme_minimal()
}

extract_conf_df <- function(model, ci_level = 0.90, term_filter = "^name") {
  critical_val <- qnorm(1 - (1 - ci_level) / 2)
  
  coef(summary(model)) |>
    as.data.frame() |>
    rownames_to_column("term") |>
    filter(grepl(term_filter, term)) |>
    transmute(
      name     = term,
      estimate  = Estimate,
      std_error = `Std. Error`,
      lower     = Estimate - critical_val * `Std. Error`,
      upper     = Estimate + critical_val * `Std. Error`
    )
}

# ----------------------
# Models and CI tables
# ----------------------

# Model 01: only water
model01 <- lm(rating ~ -1 + name, data = data_processed)
conf01  <- extract_conf_df(model01)

# Model 02: water + respondent
model02 <- lm(rating ~ -1 + name + respondent, data = data_processed)
conf02  <- extract_conf_df(model02)

# Model 03: center each participant
data_processed_03 <- data_processed |>
  group_by(respondent) |>
  mutate(rating_centered = rating - mean(rating, na.rm = TRUE)) |>
  ungroup()
model03 <- lm(rating_centered ~ -1 + name, data = data_processed_03)
conf03  <- extract_conf_df(model03)

# Model 04
# model04 <- lmer(rating ~ -1 + water + (water | respondent), data = data_processed)
# conf04  <- extract_conf_df(model04)

# ----------------------
# Example plots
# ----------------------

plot_water_ci(conf01, title = "Model 01: No Person Controls")
plot_water_ci(conf02, title = "Model 02: With Respondent Fixed Effects")
plot_water_ci(conf03, title = "Model 03: Centered by Respondent")
# plot_water_ci(conf04, title = "Model 04: Random Effects by Respondent")
