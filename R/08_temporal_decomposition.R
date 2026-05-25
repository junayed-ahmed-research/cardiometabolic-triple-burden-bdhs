# 08_temporal_decomposition.R
# Temporal decomposition of change in multiple burden
# Complete-case analysis

library(tidyverse)
library(survey)

# Create clean decomposition datasets

decomp_2017 <- analysis_sample_2017 %>%
  mutate(
    multiple_burden = case_when(
      burden_group %in% c("Double burden", "Triple burden") ~ 1,
      burden_group %in% c("No burden", "Single burden") ~ 0,
      TRUE ~ NA_real_
    ),
    survey_year = "2017-18"
  ) %>%
  filter(
    !is.na(multiple_burden),
    !is.na(age_group),
    !is.na(residence),
    !is.na(education),
    !is.na(wealth),
    !is.na(sample_weight),
    !is.na(v001),
    !is.na(v022)
  )

decomp_2022 <- analysis_sample %>%
  mutate(
    multiple_burden = case_when(
      burden_group %in% c("Double burden", "Triple burden") ~ 1,
      burden_group %in% c("No burden", "Single burden") ~ 0,
      TRUE ~ NA_real_
    ),
    survey_year = "2022"
  ) %>%
  filter(
    !is.na(multiple_burden),
    !is.na(age_group),
    !is.na(residence),
    !is.na(education),
    !is.na(wealth),
    !is.na(sample_weight),
    !is.na(v001),
    !is.na(v022)
  )

# Ensure same factor levels in both years

decomp_2017 <- decomp_2017 %>%
  mutate(
    age_group = factor(age_group, levels = c("18-24", "25-34", "35-49")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(
      education,
      levels = c("No education", "Primary", "Secondary", "Higher")
    ),
    wealth = factor(
      wealth,
      levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )

decomp_2022 <- decomp_2022 %>%
  mutate(
    age_group = factor(age_group, levels = c("18-24", "25-34", "35-49")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(
      education,
      levels = c("No education", "Primary", "Secondary", "Higher")
    ),
    wealth = factor(
      wealth,
      levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )

# Check rows
dim(decomp_2017)
dim(decomp_2022)

# Survey designs

design_2017_decomp <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = decomp_2017,
  nest = TRUE
)

design_2022_decomp <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = decomp_2022,
  nest = TRUE
)

# Observed weighted prevalence

prev_2017_multiple <- svymean(
  ~multiple_burden,
  design_2017_decomp,
  na.rm = TRUE
)

prev_2022_multiple <- svymean(
  ~multiple_burden,
  design_2022_decomp,
  na.rm = TRUE
)

p_2017 <- as.numeric(coef(prev_2017_multiple))
p_2022 <- as.numeric(coef(prev_2022_multiple))

total_change <- p_2022 - p_2017

# Survey-weighted logistic models

model_2017_decomp <- svyglm(
  multiple_burden ~ age_group + residence + education + wealth,
  design = design_2017_decomp,
  family = quasibinomial()
)

model_2022_decomp <- svyglm(
  multiple_burden ~ age_group + residence + education + wealth,
  design = design_2022_decomp,
  family = quasibinomial()
)

# Weighted mean function

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

# Predictions

pred_2017_on_2017 <- as.numeric(predict(
  model_2017_decomp,
  newdata = decomp_2017,
  type = "response"
))

pred_2022_on_2022 <- as.numeric(predict(
  model_2022_decomp,
  newdata = decomp_2022,
  type = "response"
))

pred_2017_on_2022 <- as.numeric(predict(
  model_2017_decomp,
  newdata = decomp_2022,
  type = "response"
))

pred_2022_on_2017 <- as.numeric(predict(
  model_2022_decomp,
  newdata = decomp_2017,
  type = "response"
))

# Check prediction lengths
length(pred_2017_on_2017)
nrow(decomp_2017)

length(pred_2022_on_2022)
nrow(decomp_2022)

length(pred_2017_on_2022)
nrow(decomp_2022)

length(pred_2022_on_2017)
nrow(decomp_2017)

# Weighted average predicted probabilities

mean_2017_beta2017 <- weighted_mean(
  pred_2017_on_2017,
  decomp_2017$sample_weight
)

mean_2022_beta2022 <- weighted_mean(
  pred_2022_on_2022,
  decomp_2022$sample_weight
)

mean_2022_beta2017 <- weighted_mean(
  pred_2017_on_2022,
  decomp_2022$sample_weight
)

mean_2017_beta2022 <- weighted_mean(
  pred_2022_on_2017,
  decomp_2017$sample_weight
)

# Twofold nonlinear decomposition

explained_2017_ref <- mean_2022_beta2017 - mean_2017_beta2017
unexplained_2017_ref <- mean_2022_beta2022 - mean_2022_beta2017

explained_2022_ref <- mean_2022_beta2022 - mean_2017_beta2022
unexplained_2022_ref <- mean_2017_beta2022 - mean_2017_beta2017

explained_average <- (explained_2017_ref + explained_2022_ref) / 2
unexplained_average <- total_change - explained_average

# Final decomposition table

temporal_decomposition_results <- tibble(
  component = c(
    "Observed prevalence in 2017-18",
    "Observed prevalence in 2022",
    "Total change",
    "Explained change: composition effect",
    "Unexplained change: coefficient/risk-structure effect"
  ),
  estimate = c(
    p_2017,
    p_2022,
    total_change,
    explained_average,
    unexplained_average
  ),
  percent_point = round(estimate * 100, 2),
  percent_of_total_change = c(
    NA,
    NA,
    100,
    round((explained_average / total_change) * 100, 2),
    round((unexplained_average / total_change) * 100, 2)
  )
)

write_csv(
  temporal_decomposition_results,
  "outputs/tables/temporal_decomposition_results.csv"
)

temporal_decomposition_results

message("Temporal decomposition completed.")
