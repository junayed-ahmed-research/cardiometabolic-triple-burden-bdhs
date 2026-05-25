# 05_descriptive_analysis.R
# Survey-weighted descriptive analysis

library(tidyverse)
library(survey)

# Survey design: 2022

bdhs_design <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample,
  nest = TRUE
)

# Survey design: 2017-18

bdhs_design_2017 <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample_2017,
  nest = TRUE
)

# Weighted prevalence: 2022

obesity_prev <- svymean(~obesity, bdhs_design, na.rm = TRUE)
hypertension_prev <- svymean(~hypertension, bdhs_design, na.rm = TRUE)
diabetes_prev <- svymean(~diabetes, bdhs_design, na.rm = TRUE)

burden_prev <- svymean(
  ~factor(burden_group),
  bdhs_design,
  na.rm = TRUE
)

descriptive_results <- tibble(
  survey_year = "2022",
  indicator = c("Obesity", "Hypertension", "Diabetes"),
  prevalence = c(
    coef(obesity_prev),
    coef(hypertension_prev),
    coef(diabetes_prev)
  ) * 100
) %>%
  mutate(prevalence = round(prevalence, 2))

burden_results <- tibble(
  survey_year = "2022",
  burden_group = c(
    "No burden",
    "Single burden",
    "Double burden",
    "Triple burden"
  ),
  prevalence = as.numeric(coef(burden_prev)) * 100
) %>%
  mutate(prevalence = round(prevalence, 2))

# Weighted prevalence: 2017-18

obesity_prev_2017 <- svymean(~obesity, bdhs_design_2017, na.rm = TRUE)
hypertension_prev_2017 <- svymean(~hypertension, bdhs_design_2017, na.rm = TRUE)
diabetes_prev_2017 <- svymean(~diabetes, bdhs_design_2017, na.rm = TRUE)

burden_prev_2017 <- svymean(
  ~factor(burden_group),
  bdhs_design_2017,
  na.rm = TRUE
)

descriptive_results_2017 <- tibble(
  survey_year = "2017-18",
  indicator = c("Obesity", "Hypertension", "Diabetes"),
  prevalence = c(
    coef(obesity_prev_2017),
    coef(hypertension_prev_2017),
    coef(diabetes_prev_2017)
  ) * 100
) %>%
  mutate(prevalence = round(prevalence, 2))

burden_results_2017 <- tibble(
  survey_year = "2017-18",
  burden_group = c(
    "No burden",
    "Single burden",
    "Double burden",
    "Triple burden"
  ),
  prevalence = as.numeric(coef(burden_prev_2017)) * 100
) %>%
  mutate(prevalence = round(prevalence, 2))

# 2017-18 vs 2022 comparison

prevalence_comparison <- descriptive_results_2017 %>%
  select(indicator, prevalence_2017 = prevalence) %>%
  left_join(
    descriptive_results %>%
      select(indicator, prevalence_2022 = prevalence),
    by = "indicator"
  ) %>%
  mutate(
    absolute_change = round(prevalence_2022 - prevalence_2017, 2),
    relative_change = round(
      (absolute_change / prevalence_2017) * 100,
      2
    )
  )

burden_comparison <- burden_results_2017 %>%
  select(burden_group, prevalence_2017 = prevalence) %>%
  left_join(
    burden_results %>%
      select(burden_group, prevalence_2022 = prevalence),
    by = "burden_group"
  ) %>%
  mutate(
    absolute_change = round(prevalence_2022 - prevalence_2017, 2),
    relative_change = round(
      (absolute_change / prevalence_2017) * 100,
      2
    )
  )

# Save outputs

write_csv(
  descriptive_results,
  "outputs/tables/descriptive_results_2022.csv"
)

write_csv(
  burden_results,
  "outputs/tables/burden_results_2022.csv"
)

write_csv(
  descriptive_results_2017,
  "outputs/tables/descriptive_results_2017.csv"
)

write_csv(
  burden_results_2017,
  "outputs/tables/burden_results_2017.csv"
)

write_csv(
  prevalence_comparison,
  "outputs/tables/prevalence_comparison.csv"
)

write_csv(
  burden_comparison,
  "outputs/tables/burden_comparison.csv"
)

# Print outputs

descriptive_results_2017
descriptive_results

prevalence_comparison
burden_comparison

message("Descriptive analysis completed.")
