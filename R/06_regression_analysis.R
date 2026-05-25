
# 07_regression_analysis.R
# Survey-weighted regression analysis
# Prepare regression data and survey design

library(tidyverse)
library(survey)

# Recode categorical variables with clear labels

analysis_sample <- analysis_sample %>%
  mutate(
    survey_year = factor(survey_year, levels = c("2022")),
    
    burden_group = factor(
      burden_group,
      levels = c("No burden", "Single burden", "Double burden", "Triple burden")
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    ),
    
    residence = factor(
      as.numeric(v025),
      levels = c(1, 2),
      labels = c("Urban", "Rural")
    ),
    
    education = factor(
      as.numeric(v106),
      levels = c(0, 1, 2, 3),
      labels = c("No education", "Primary", "Secondary", "Higher")
    ),
    
    wealth = factor(
      as.numeric(v190),
      levels = c(1, 2, 3, 4, 5),
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )

analysis_sample_2017 <- analysis_sample_2017 %>%
  mutate(
    survey_year = factor(survey_year, levels = c("2017-18")),
    
    burden_group = factor(
      burden_group,
      levels = c("No burden", "Single burden", "Double burden", "Triple burden")
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    ),
    
    residence = factor(
      as.numeric(v025),
      levels = c(1, 2),
      labels = c("Urban", "Rural")
    ),
    
    education = factor(
      as.numeric(v106),
      levels = c(0, 1, 2, 3),
      labels = c("No education", "Primary", "Secondary", "Higher")
    ),
    
    wealth = factor(
      as.numeric(v190),
      levels = c(1, 2, 3, 4, 5),
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    )
  )

# Create pooled dataset for regression

analysis_sample_pooled <- bind_rows(
  analysis_sample_2017,
  analysis_sample
) %>%
  mutate(
    survey_year = factor(
      survey_year,
      levels = c("2017-18", "2022")
    ),
    
    burden_group = factor(
      burden_group,
      levels = c("No burden", "Single burden", "Double burden", "Triple burden")
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    ),
    
    residence = factor(
      residence,
      levels = c("Urban", "Rural")
    ),
    
    education = factor(
      education,
      levels = c("No education", "Primary", "Secondary", "Higher")
    ),
    
    wealth = factor(
      wealth,
      levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    ),
    
    # Important for pooled survey design
    psu_pooled = interaction(survey_year, v001, drop = TRUE),
    strata_pooled = interaction(survey_year, v022, drop = TRUE)
  )

# Recreate survey designs

bdhs_design_2022_reg <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample,
  nest = TRUE
)

bdhs_design_2017_reg <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample_2017,
  nest = TRUE
)

bdhs_design_pooled_reg <- svydesign(
  ids = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sample_weight,
  data = analysis_sample_pooled,
  nest = TRUE
)

# Final checks before regression

dim(analysis_sample)
dim(analysis_sample_2017)
dim(analysis_sample_pooled)

table(analysis_sample$burden_group, useNA = "ifany")
table(analysis_sample_2017$burden_group, useNA = "ifany")

table(
  analysis_sample_pooled$burden_group,
  analysis_sample_pooled$survey_year,
  useNA = "ifany"
)

summary(analysis_sample_pooled$age_group)
summary(analysis_sample_pooled$residence)
summary(analysis_sample_pooled$education)
summary(analysis_sample_pooled$wealth)

message("Regression data prepared.")

# Survey-weighted multinomial regression

# Install packages if not already installed
if (!requireNamespace("VGAM", quietly = TRUE)) {
  install.packages("VGAM")
}

if (!requireNamespace("svyVGAM", quietly = TRUE)) {
  install.packages("svyVGAM")
}

library(VGAM)
library(svyVGAM)

# Make sure outcome reference category is "No burden"

analysis_sample <- analysis_sample %>%
  mutate(
    burden_group = relevel(burden_group, ref = "No burden")
  )

analysis_sample_2017 <- analysis_sample_2017 %>%
  mutate(
    burden_group = relevel(burden_group, ref = "No burden")
  )

analysis_sample_pooled <- analysis_sample_pooled %>%
  mutate(
    burden_group = relevel(burden_group, ref = "No burden")
  )

# Update survey designs after releveling
bdhs_design_2022_reg <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample,
  nest = TRUE
)

bdhs_design_2017_reg <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample_2017,
  nest = TRUE
)

bdhs_design_pooled_reg <- svydesign(
  ids = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sample_weight,
  data = analysis_sample_pooled,
  nest = TRUE
)

# Model 1: 2017-18 survey-weighted multinomial regression

model_2017 <- svy_vglm(
  burden_group ~ age_group + residence + education + wealth,
  design = bdhs_design_2017_reg,
  family = multinomial(refLevel = 1)
)

summary(model_2017)

# Model 2: 2022 survey-weighted multinomial regression

model_2022 <- svy_vglm(
  burden_group ~ age_group + residence + education + wealth,
  design = bdhs_design_2022_reg,
  family = multinomial(refLevel = 1)
)

summary(model_2022)

# Model 3: Pooled model with survey year

model_pooled <- svy_vglm(
  burden_group ~ survey_year + age_group + residence + education + wealth,
  design = bdhs_design_pooled_reg,
  family = multinomial(refLevel = 1)
)

summary(model_pooled)

# Model 4: Pooled model with survey year × wealth interaction
# This tests whether wealth-related pattern changed between surveys

model_pooled_wealth_interaction <- svy_vglm(
  burden_group ~ survey_year * wealth + age_group + residence + education,
  design = bdhs_design_pooled_reg,
  family = multinomial(refLevel = 1)
)

summary(model_pooled_wealth_interaction)

message("Multinomial regression completed.")

# Extract relative risk ratios

extract_rrr <- function(model, model_name) {
  
  coef_values <- coef(model)
  se_values <- sqrt(diag(vcov(model)))
  
  results <- tibble(
    model = model_name,
    term = names(coef_values),
    estimate = as.numeric(coef_values),
    std_error = as.numeric(se_values),
    z_value = estimate / std_error,
    p_value = 2 * (1 - pnorm(abs(z_value))),
    RRR = exp(estimate),
    lower_95_CI = exp(estimate - 1.96 * std_error),
    upper_95_CI = exp(estimate + 1.96 * std_error)
  ) %>%
    mutate(
      RRR = round(RRR, 3),
      lower_95_CI = round(lower_95_CI, 3),
      upper_95_CI = round(upper_95_CI, 3),
      p_value = round(p_value, 4)
    )
  
  return(results)
}

rrr_2017 <- extract_rrr(model_2017, "2017-18")
rrr_2022 <- extract_rrr(model_2022, "2022")
rrr_pooled <- extract_rrr(model_pooled, "Pooled")
rrr_pooled_wealth_interaction <- extract_rrr(
  model_pooled_wealth_interaction,
  "Pooled wealth interaction"
)

write_csv(rrr_2017, "outputs/tables/rrr_model_2017.csv")
write_csv(rrr_2022, "outputs/tables/rrr_model_2022.csv")
write_csv(rrr_pooled, "outputs/tables/rrr_model_pooled.csv")
write_csv(
  rrr_pooled_wealth_interaction,
  "outputs/tables/rrr_model_pooled_wealth_interaction.csv"
)

rrr_2017
rrr_2022
rrr_pooled
rrr_pooled_wealth_interaction

message("Regression estimates exported.")

# Collapsed multinomial regression
# Outcome: No burden / Single burden / Multiple burden
# Multiple burden = Double burden + Triple burden

# Create collapsed outcome for regression
analysis_sample <- analysis_sample %>%
  mutate(
    burden_reg = case_when(
      burden_group == "No burden" ~ "No burden",
      burden_group == "Single burden" ~ "Single burden",
      burden_group %in% c("Double burden", "Triple burden") ~ "Multiple burden",
      TRUE ~ NA_character_
    ),
    burden_reg = factor(
      burden_reg,
      levels = c("No burden", "Single burden", "Multiple burden")
    )
  )

analysis_sample_2017 <- analysis_sample_2017 %>%
  mutate(
    burden_reg = case_when(
      burden_group == "No burden" ~ "No burden",
      burden_group == "Single burden" ~ "Single burden",
      burden_group %in% c("Double burden", "Triple burden") ~ "Multiple burden",
      TRUE ~ NA_character_
    ),
    burden_reg = factor(
      burden_reg,
      levels = c("No burden", "Single burden", "Multiple burden")
    )
  )

analysis_sample_pooled <- bind_rows(
  analysis_sample_2017,
  analysis_sample
) %>%
  mutate(
    survey_year = factor(
      survey_year,
      levels = c("2017-18", "2022")
    ),
    burden_reg = factor(
      burden_reg,
      levels = c("No burden", "Single burden", "Multiple burden")
    ),
    psu_pooled = interaction(survey_year, v001, drop = TRUE),
    strata_pooled = interaction(survey_year, v022, drop = TRUE)
  )

# Check cell counts
table(analysis_sample_2017$wealth, analysis_sample_2017$burden_reg)
table(analysis_sample$wealth, analysis_sample$burden_reg)
table(analysis_sample_pooled$wealth, analysis_sample_pooled$burden_reg)

# Survey designs for collapsed outcome
design_2017_collapsed <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample_2017,
  nest = TRUE
)

design_2022_collapsed <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~sample_weight,
  data = analysis_sample,
  nest = TRUE
)

design_pooled_collapsed <- svydesign(
  ids = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sample_weight,
  data = analysis_sample_pooled,
  nest = TRUE
)

# Collapsed multinomial models
model_2017_collapsed <- svy_vglm(
  burden_reg ~ age_group + residence + education + wealth,
  design = design_2017_collapsed,
  family = multinomial(refLevel = 1)
)

model_2022_collapsed <- svy_vglm(
  burden_reg ~ age_group + residence + education + wealth,
  design = design_2022_collapsed,
  family = multinomial(refLevel = 1)
)

model_pooled_collapsed <- svy_vglm(
  burden_reg ~ survey_year + age_group + residence + education + wealth,
  design = design_pooled_collapsed,
  family = multinomial(refLevel = 1)
)

# Extract RRR
rrr_2017_collapsed <- extract_rrr(model_2017_collapsed, "2017-18 collapsed")
rrr_2022_collapsed <- extract_rrr(model_2022_collapsed, "2022 collapsed")
rrr_pooled_collapsed <- extract_rrr(model_pooled_collapsed, "Pooled collapsed")

# Save outputs
write_csv(
  rrr_2017_collapsed,
  "outputs/tables/rrr_model_2017_collapsed.csv"
)

write_csv(
  rrr_2022_collapsed,
  "outputs/tables/rrr_model_2022_collapsed.csv"
)

write_csv(
  rrr_pooled_collapsed,
  "outputs/tables/rrr_model_pooled_collapsed.csv"
)

# Print results
rrr_2017_collapsed
rrr_2022_collapsed
rrr_pooled_collapsed

message("Collapsed regression completed.")

# Clean regression tables

clean_rrr_table <- function(rrr_data) {
  rrr_data %>%
    filter(!str_detect(term, "Intercept")) %>%
    mutate(
      outcome = case_when(
        str_detect(term, ":1$") ~ "Single burden vs No burden",
        str_detect(term, ":2$") ~ "Multiple burden vs No burden",
        TRUE ~ NA_character_
      ),
      variable = term %>%
        str_remove(":1$") %>%
        str_remove(":2$")
    ) %>%
    select(
      model,
      outcome,
      variable,
      RRR,
      lower_95_CI,
      upper_95_CI,
      p_value
    ) %>%
    mutate(
      RRR_CI = paste0(
        RRR,
        " (",
        lower_95_CI,
        ", ",
        upper_95_CI,
        ")"
      ),
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

clean_rrr_2017_collapsed <- clean_rrr_table(rrr_2017_collapsed)
clean_rrr_2022_collapsed <- clean_rrr_table(rrr_2022_collapsed)
clean_rrr_pooled_collapsed <- clean_rrr_table(rrr_pooled_collapsed)

# Save clean tables
write_csv(
  clean_rrr_2017_collapsed,
  "outputs/tables/clean_rrr_2017_collapsed.csv"
)

write_csv(
  clean_rrr_2022_collapsed,
  "outputs/tables/clean_rrr_2022_collapsed.csv"
)

write_csv(
  clean_rrr_pooled_collapsed,
  "outputs/tables/clean_rrr_pooled_collapsed.csv"
)

# Print clean tables
clean_rrr_2017_collapsed
clean_rrr_2022_collapsed
clean_rrr_pooled_collapsed

message("Regression tables cleaned.")

# Add readable regression labels

label_regression_table <- function(clean_table) {
  clean_table %>%
    mutate(
      variable_label = case_when(
        variable == "survey_year2022" ~ "Survey year: 2022 vs 2017-18",
        
        variable == "age_group25-34" ~ "Age group: 25-34 vs 18-24",
        variable == "age_group35-49" ~ "Age group: 35-49 vs 18-24",
        
        variable == "residenceRural" ~ "Residence: Rural vs Urban",
        
        variable == "educationPrimary" ~ "Education: Primary vs No education",
        variable == "educationSecondary" ~ "Education: Secondary vs No education",
        variable == "educationHigher" ~ "Education: Higher vs No education",
        
        variable == "wealthPoorer" ~ "Wealth: Poorer vs Poorest",
        variable == "wealthMiddle" ~ "Wealth: Middle vs Poorest",
        variable == "wealthRicher" ~ "Wealth: Richer vs Poorest",
        variable == "wealthRichest" ~ "Wealth: Richest vs Poorest",
        
        TRUE ~ variable
      ),
      
      final_result = paste0(RRR_CI, significance)
    ) %>%
    select(
      model,
      outcome,
      variable_label,
      final_result,
      p_value
    )
}

final_rrr_2017_collapsed <- label_regression_table(clean_rrr_2017_collapsed)
final_rrr_2022_collapsed <- label_regression_table(clean_rrr_2022_collapsed)
final_rrr_pooled_collapsed <- label_regression_table(clean_rrr_pooled_collapsed)

write_csv(
  final_rrr_2017_collapsed,
  "outputs/tables/final_rrr_2017_collapsed.csv"
)

write_csv(
  final_rrr_2022_collapsed,
  "outputs/tables/final_rrr_2022_collapsed.csv"
)

write_csv(
  final_rrr_pooled_collapsed,
  "outputs/tables/final_rrr_pooled_collapsed.csv"
)

final_rrr_2017_collapsed
final_rrr_2022_collapsed
final_rrr_pooled_collapsed

message("Regression labels applied.")

# Format regression p-values

format_p_value <- function(p) {
  case_when(
    p < 0.001 ~ "<0.001",
    p < 0.01 ~ as.character(round(p, 3)),
    p < 0.05 ~ as.character(round(p, 3)),
    TRUE ~ as.character(round(p, 3))
  )
}

final_rrr_2017_collapsed <- final_rrr_2017_collapsed %>%
  mutate(p_value_formatted = format_p_value(p_value))

final_rrr_2022_collapsed <- final_rrr_2022_collapsed %>%
  mutate(p_value_formatted = format_p_value(p_value))

final_rrr_pooled_collapsed <- final_rrr_pooled_collapsed %>%
  mutate(p_value_formatted = format_p_value(p_value))

write_csv(
  final_rrr_2017_collapsed,
  "outputs/tables/final_rrr_2017_collapsed_formatted.csv"
)

write_csv(
  final_rrr_2022_collapsed,
  "outputs/tables/final_rrr_2022_collapsed_formatted.csv"
)

write_csv(
  final_rrr_pooled_collapsed,
  "outputs/tables/final_rrr_pooled_collapsed_formatted.csv"
)

print(final_rrr_pooled_collapsed, n = Inf)

message("Regression tables formatted.")
