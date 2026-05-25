# 09_marginal_effects.R
# Adjusted predicted probabilities for multiple burden
# Final safe version using survey-weighted binary logistic model

library(tidyverse)
library(survey)

# Prepare marginal effects data

marginal_data <- analysis_sample_pooled %>%
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

dim(marginal_data)

# Survey design

design_marginal <- svydesign(
  ids = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sample_weight,
  data = marginal_data,
  nest = TRUE
)

# Binary logistic model for multiple burden

model_multiple_burden <- svyglm(
  multiple_burden ~ survey_year + age_group + residence + education + wealth,
  design = design_marginal,
  family = quasibinomial()
)

summary(model_multiple_burden)

# Weighted mean function

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

# Marginal prediction function

marginal_prediction_binary <- function(data, model, variable_name) {
  
  variable_levels <- levels(data[[variable_name]])
  
  results <- map_dfr(variable_levels, function(level_value) {
    
    newdata <- data
    
    newdata[[variable_name]] <- factor(
      level_value,
      levels = levels(data[[variable_name]])
    )
    
    pred <- predict(
      model,
      newdata = newdata,
      type = "response"
    )
    
    tibble(
      variable = variable_name,
      category = level_value,
      outcome = "Multiple burden",
      predicted_probability = weighted_mean(
        as.numeric(pred),
        newdata$sample_weight
      )
    )
  })
  
  return(results)
}

# Adjusted predicted probabilities

pred_by_survey_year <- marginal_prediction_binary(
  marginal_data,
  model_multiple_burden,
  "survey_year"
)

pred_by_age <- marginal_prediction_binary(
  marginal_data,
  model_multiple_burden,
  "age_group"
)

pred_by_residence <- marginal_prediction_binary(
  marginal_data,
  model_multiple_burden,
  "residence"
)

pred_by_education <- marginal_prediction_binary(
  marginal_data,
  model_multiple_burden,
  "education"
)

pred_by_wealth <- marginal_prediction_binary(
  marginal_data,
  model_multiple_burden,
  "wealth"
)

# Combine results

multiple_burden_predictions <- bind_rows(
  pred_by_survey_year,
  pred_by_age,
  pred_by_residence,
  pred_by_education,
  pred_by_wealth
) %>%
  mutate(
    predicted_percent = round(predicted_probability * 100, 2)
  )

write_csv(
  multiple_burden_predictions,
  "outputs/tables/marginal_effects_multiple_burden_only.csv"
)

multiple_burden_predictions

# Figures

figure_wealth_multiple <- multiple_burden_predictions %>%
  filter(variable == "wealth") %>%
  ggplot(aes(x = category, y = predicted_percent, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Wealth group",
    y = "Adjusted predicted probability (%)",
    title = "Adjusted predicted probability of multiple burden by wealth"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/predicted_probability_multiple_burden_by_wealth.png",
  figure_wealth_multiple,
  width = 7,
  height = 5,
  dpi = 300
)

figure_age_multiple <- multiple_burden_predictions %>%
  filter(variable == "age_group") %>%
  ggplot(aes(x = category, y = predicted_percent, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Age group",
    y = "Adjusted predicted probability (%)",
    title = "Adjusted predicted probability of multiple burden by age group"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/predicted_probability_multiple_burden_by_age.png",
  figure_age_multiple,
  width = 7,
  height = 5,
  dpi = 300
)

figure_year_multiple <- multiple_burden_predictions %>%
  filter(variable == "survey_year") %>%
  ggplot(aes(x = category, y = predicted_percent, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Survey year",
    y = "Adjusted predicted probability (%)",
    title = "Adjusted predicted probability of multiple burden by survey year"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/predicted_probability_multiple_burden_by_survey_year.png",
  figure_year_multiple,
  width = 7,
  height = 5,
  dpi = 300
)

figure_residence_multiple <- multiple_burden_predictions %>%
  filter(variable == "residence") %>%
  ggplot(aes(x = category, y = predicted_percent, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Residence",
    y = "Adjusted predicted probability (%)",
    title = "Adjusted predicted probability of multiple burden by residence"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/predicted_probability_multiple_burden_by_residence.png",
  figure_residence_multiple,
  width = 7,
  height = 5,
  dpi = 300
)

message("Predicted probabilities completed.")
