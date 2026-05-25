# 12_concentration_index_curve.R
# Concentration index and concentration curve
# Prepare variables for inequality analysis

library(tidyverse)

# Recode wealth quintile

clean_wealth <- function(x) {
  x_chr <- as.character(x)
  
  case_when(
    x_chr %in% c("1", "Poorest") ~ "Poorest",
    x_chr %in% c("2", "Poorer") ~ "Poorer",
    x_chr %in% c("3", "Middle") ~ "Middle",
    x_chr %in% c("4", "Richer") ~ "Richer",
    x_chr %in% c("5", "Richest") ~ "Richest",
    TRUE ~ x_chr
  )
}

# Prepare 2017 data
# Only keep variables needed for concentration analysis

ci_data_2017 <- analysis_sample_2017 %>%
  transmute(
    survey_year = "2017-18",
    burden_group = as.character(burden_group),
    sample_weight = as.numeric(sample_weight),
    wealth = clean_wealth(wealth),
    multiple_burden = case_when(
      burden_group %in% c("Double burden", "Triple burden") ~ 1,
      burden_group %in% c("No burden", "Single burden") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Prepare 2022 data
# Only keep variables needed for concentration analysis

ci_data_2022 <- analysis_sample %>%
  transmute(
    survey_year = "2022",
    burden_group = as.character(burden_group),
    sample_weight = as.numeric(sample_weight),
    wealth = clean_wealth(wealth),
    multiple_burden = case_when(
      burden_group %in% c("Double burden", "Triple burden") ~ 1,
      burden_group %in% c("No burden", "Single burden") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Combine data

ci_data <- bind_rows(
  ci_data_2017,
  ci_data_2022
) %>%
  filter(
    !is.na(multiple_burden),
    !is.na(sample_weight),
    !is.na(wealth)
  ) %>%
  mutate(
    survey_year = factor(
      survey_year,
      levels = c("2017-18", "2022")
    ),
    
    wealth = factor(
      wealth,
      levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    ),
    
    wealth_order = as.numeric(wealth)
  ) %>%
  filter(!is.na(wealth_order))

# Check
dim(ci_data)
table(ci_data$survey_year, ci_data$wealth)

# Function: weighted concentration index

calculate_concentration_index <- function(data, outcome_var) {
  
  data_sorted <- data %>%
    arrange(wealth_order) %>%
    mutate(
      w = sample_weight,
      y = .data[[outcome_var]]
    ) %>%
    filter(
      !is.na(w),
      !is.na(y),
      !is.na(wealth_order)
    )
  
  total_weight <- sum(data_sorted$w, na.rm = TRUE)
  
  data_sorted <- data_sorted %>%
    mutate(
      cumulative_weight = cumsum(w),
      fractional_rank = (cumulative_weight - 0.5 * w) / total_weight
    )
  
  mu <- sum(data_sorted$w * data_sorted$y, na.rm = TRUE) / total_weight
  
  concentration_index <- (
    2 * sum(
      data_sorted$w * data_sorted$y * data_sorted$fractional_rank,
      na.rm = TRUE
    ) / (mu * total_weight)
  ) - 1
  
  # Erreygers corrected CI for binary outcome bounded between 0 and 1
  erreygers_ci <- 4 * mu * concentration_index
  
  tibble(
    mean_prevalence = mu,
    concentration_index = concentration_index,
    erreygers_corrected_ci = erreygers_ci
  )
}

# Concentration index by survey year

concentration_index_results <- ci_data %>%
  group_by(survey_year) %>%
  group_modify(
    ~ calculate_concentration_index(.x, "multiple_burden")
  ) %>%
  ungroup() %>%
  mutate(
    mean_prevalence_percent = round(mean_prevalence * 100, 2),
    concentration_index = round(concentration_index, 4),
    erreygers_corrected_ci = round(erreygers_corrected_ci, 4),
    interpretation = case_when(
      concentration_index > 0 ~ "Multiple burden is concentrated among wealthier women",
      concentration_index < 0 ~ "Multiple burden is concentrated among poorer women",
      concentration_index == 0 ~ "No socioeconomic inequality",
      TRUE ~ NA_character_
    )
  )

write_csv(
  concentration_index_results,
  "outputs/tables/concentration_index_results.csv"
)

concentration_index_results

# Concentration curve points

concentration_curve_points <- ci_data %>%
  group_by(survey_year, wealth_order, wealth) %>%
  summarise(
    population_weight = sum(sample_weight, na.rm = TRUE),
    burden_weight = sum(sample_weight * multiple_burden, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(survey_year) %>%
  arrange(wealth_order, .by_group = TRUE) %>%
  mutate(
    cumulative_population_share = cumsum(population_weight) /
      sum(population_weight, na.rm = TRUE),
    cumulative_burden_share = cumsum(burden_weight) /
      sum(burden_weight, na.rm = TRUE)
  ) %>%
  ungroup()

# Add origin point
origin_points <- concentration_curve_points %>%
  distinct(survey_year) %>%
  mutate(
    wealth_order = 0,
    wealth = "Origin",
    population_weight = 0,
    burden_weight = 0,
    cumulative_population_share = 0,
    cumulative_burden_share = 0
  )

concentration_curve_points <- concentration_curve_points %>%
  mutate(wealth = as.character(wealth)) %>%
  bind_rows(origin_points) %>%
  arrange(survey_year, wealth_order) %>%
  mutate(
    cumulative_population_percent = round(cumulative_population_share * 100, 2),
    cumulative_burden_percent = round(cumulative_burden_share * 100, 2)
  )

write_csv(
  concentration_curve_points,
  "outputs/tables/concentration_curve_points.csv"
)

concentration_curve_points

# Figure 1: Concentration curve

figure_concentration_curve <- concentration_curve_points %>%
  ggplot(
    aes(
      x = cumulative_population_share,
      y = cumulative_burden_share,
      group = survey_year,
      linetype = survey_year
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Cumulative share of population ranked by wealth",
    y = "Cumulative share of multiple burden",
    linetype = "Survey year",
    title = "Concentration curve for multiple burden"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/concentration_curve_multiple_burden.png",
  figure_concentration_curve,
  width = 7,
  height = 5,
  dpi = 300
)

# Figure 2: Concentration index comparison

figure_concentration_index <- concentration_index_results %>%
  ggplot(
    aes(
      x = survey_year,
      y = concentration_index
    )
  ) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Survey year",
    y = "Concentration index",
    title = "Socioeconomic inequality in multiple burden"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/concentration_index_multiple_burden.png",
  figure_concentration_index,
  width = 7,
  height = 5,
  dpi = 300
)

# Copy to final folders

dir.create("outputs/final_tables", showWarnings = FALSE)
dir.create("outputs/final_figures", showWarnings = FALSE)

write_csv(
  concentration_index_results,
  "outputs/final_tables/table_9_concentration_index.csv"
)

write_csv(
  concentration_curve_points,
  "outputs/final_tables/table_10_concentration_curve_points.csv"
)

file.copy(
  "outputs/figures/concentration_curve_multiple_burden.png",
  "outputs/final_figures/figure_7_concentration_curve_multiple_burden.png",
  overwrite = TRUE
)

file.copy(
  "outputs/figures/concentration_index_multiple_burden.png",
  "outputs/final_figures/figure_8_concentration_index_multiple_burden.png",
  overwrite = TRUE
)

message("Inequality analysis completed.")
