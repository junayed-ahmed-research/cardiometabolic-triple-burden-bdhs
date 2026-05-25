# 10_sensitivity_analysis.R
# Sensitivity analysis for burden outcome specification

library(tidyverse)
library(survey)

# 1. Cell size check: 4-category burden by wealth and year

cell_count_2017_wealth <- analysis_sample_2017 %>%
  count(wealth, burden_group) %>%
  mutate(survey_year = "2017-18")

cell_count_2022_wealth <- analysis_sample %>%
  count(wealth, burden_group) %>%
  mutate(survey_year = "2022")

cell_count_wealth_burden <- bind_rows(
  cell_count_2017_wealth,
  cell_count_2022_wealth
) %>%
  select(survey_year, wealth, burden_group, n)

write_csv(
  cell_count_wealth_burden,
  "outputs/tables/sensitivity_cell_count_wealth_burden.csv"
)

cell_count_wealth_burden

# 2. Identify sparse cells
# Sparse cell threshold: n < 10

sparse_cells <- cell_count_wealth_burden %>%
  filter(n < 10)

write_csv(
  sparse_cells,
  "outputs/tables/sensitivity_sparse_cells.csv"
)

sparse_cells

# 3. Check unstable RRR from 4-category multinomial models

unstable_rrr_2017 <- rrr_2017 %>%
  filter(
    RRR > 100 |
      upper_95_CI > 1000 |
      lower_95_CI == 0
  ) %>%
  mutate(model_type = "4-category multinomial 2017-18")

unstable_rrr_2022 <- rrr_2022 %>%
  filter(
    RRR > 100 |
      upper_95_CI > 1000 |
      lower_95_CI == 0
  ) %>%
  mutate(model_type = "4-category multinomial 2022")

unstable_rrr_pooled <- rrr_pooled %>%
  filter(
    RRR > 100 |
      upper_95_CI > 1000 |
      lower_95_CI == 0
  ) %>%
  mutate(model_type = "4-category multinomial pooled")

unstable_rrr_all <- bind_rows(
  unstable_rrr_2017,
  unstable_rrr_2022,
  unstable_rrr_pooled
)

write_csv(
  unstable_rrr_all,
  "outputs/tables/sensitivity_unstable_4category_rrr.csv"
)

unstable_rrr_all

# 4. Binary logistic sensitivity model
# Outcome: multiple burden vs no/single burden

sensitivity_data <- analysis_sample_pooled %>%
  mutate(
    multiple_burden = case_when(
      burden_group %in% c("Double burden", "Triple burden") ~ 1,
      burden_group %in% c("No burden", "Single burden") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(multiple_burden),
    !is.na(survey_year),
    !is.na(age_group),
    !is.na(residence),
    !is.na(education),
    !is.na(wealth),
    !is.na(sample_weight),
    !is.na(v001),
    !is.na(v022)
  ) %>%
  mutate(
    survey_year = factor(survey_year, levels = c("2017-18", "2022")),
    age_group = factor(age_group, levels = c("18-24", "25-34", "35-49")),
    residence = factor(residence, levels = c("Urban", "Rural")),
    education = factor(
      education,
      levels = c("No education", "Primary", "Secondary", "Higher")
    ),
    wealth = factor(
      wealth,
      levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    ),
    psu_pooled = interaction(survey_year, v001, drop = TRUE),
    strata_pooled = interaction(survey_year, v022, drop = TRUE)
  )

sensitivity_design <- svydesign(
  ids = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sample_weight,
  data = sensitivity_data,
  nest = TRUE
)

model_binary_multiple <- svyglm(
  multiple_burden ~ survey_year + age_group + residence + education + wealth,
  design = sensitivity_design,
  family = quasibinomial()
)

summary(model_binary_multiple)

# 5. Extract OR from binary logistic sensitivity model

binary_sensitivity_results <- tibble(
  term = names(coef(model_binary_multiple)),
  estimate = as.numeric(coef(model_binary_multiple)),
  std_error = sqrt(diag(vcov(model_binary_multiple)))
) %>%
  mutate(
    z_value = estimate / std_error,
    p_value = 2 * (1 - pnorm(abs(z_value))),
    OR = exp(estimate),
    lower_95_CI = exp(estimate - 1.96 * std_error),
    upper_95_CI = exp(estimate + 1.96 * std_error)
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = round(OR, 3),
    lower_95_CI = round(lower_95_CI, 3),
    upper_95_CI = round(upper_95_CI, 3),
    p_value = round(p_value, 4),
    OR_CI = paste0(OR, " (", lower_95_CI, ", ", upper_95_CI, ")"),
    p_value_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 3))
    )
  )

write_csv(
  binary_sensitivity_results,
  "outputs/tables/sensitivity_binary_multiple_burden_model.csv"
)

binary_sensitivity_results

# 6. Final sensitivity conclusion table

sensitivity_summary <- tibble(
  issue = c(
    "Sparse triple burden cells",
    "Unstable 4-category multinomial estimates",
    "Collapsed multinomial model",
    "Binary multiple-burden model"
  ),
  finding = c(
    paste0(nrow(sparse_cells), " sparse cells found with n < 10"),
    paste0(nrow(unstable_rrr_all), " unstable estimates detected"),
    "Used as main adjusted model",
    "Used as robustness check"
  ),
  interpretation = c(
    "Triple burden was too sparse for stable fully adjusted 4-category regression",
    "Some 4-category RRRs were extremely large or had very wide confidence intervals",
    "No burden / Single burden / Multiple burden model was more stable and interpretable",
    "Binary model supported the direction of main findings"
  )
)

write_csv(
  sensitivity_summary,
  "outputs/tables/sensitivity_summary.csv"
)

sensitivity_summary

message("Sensitivity analysis completed.")
