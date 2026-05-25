# 04_create_variables.R
# Create cardiometabolic indicators and burden outcome
# Final updated version

library(tidyverse)

# 2022 VARIABLE CREATION

analysis_data <- analysis_data %>%
  mutate(
    # BMI: DHS stored as BMI * 100
    bmi = if_else(!is.na(v445) & v445 < 9990, v445 / 100, NA_real_),
    
    # Clean final BP variables
    final_sbp_2022 = if_else(!is.na(wbp24) & wbp24 < 994, wbp24, NA_real_),
    final_dbp_2022 = if_else(!is.na(wbp25) & wbp25 < 994, wbp25, NA_real_),
    
    # Clean fasting plasma glucose
    glucose_2022 = if_else(!is.na(sb267g) & sb267g < 99, sb267g, NA_real_),
    
    # Obesity: BMI >= 27.5 kg/m2
    obesity = case_when(
      !is.na(bmi) & bmi >= 27.5 ~ 1,
      !is.na(bmi) & bmi < 27.5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Hypertension:
    # SBP >= 140 OR DBP >= 90 OR taking medication for BP
    hypertension = case_when(
      final_sbp_2022 >= 140 | final_dbp_2022 >= 90 | wbp19 == 1 ~ 1,
      !is.na(final_sbp_2022) & !is.na(final_dbp_2022) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Diabetes:
    # fasting plasma glucose >= 7.0 mmol/L OR taking diabetes medication
    diabetes = case_when(
      !is.na(glucose_2022) & glucose_2022 >= 7.0 ~ 1,
      sb240 == 1 ~ 1,
      !is.na(glucose_2022) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Burden count
    burden_count = obesity + hypertension + diabetes,
    
    # Burden group
    burden_group = case_when(
      burden_count == 0 ~ "No burden",
      burden_count == 1 ~ "Single burden",
      burden_count == 2 ~ "Double burden",
      burden_count == 3 ~ "Triple burden",
      TRUE ~ NA_character_
    )
  )

# Final 2022 analysis sample
analysis_sample <- analysis_data %>%
  filter(
    !is.na(obesity),
    !is.na(hypertension),
    !is.na(diabetes)
  ) %>%
  mutate(
    survey_year = "2022",
    sample_weight = v005 / 1000000,
    
    age_group = case_when(
      v012 >= 18 & v012 <= 24 ~ "18-24",
      v012 >= 25 & v012 <= 34 ~ "25-34",
      v012 >= 35 & v012 <= 49 ~ "35-49",
      TRUE ~ NA_character_
    ),
    
    burden_group = factor(
      burden_group,
      levels = c(
        "No burden",
        "Single burden",
        "Double burden",
        "Triple burden"
      )
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    ),
    
    residence = factor(v025),
    education = factor(v106),
    wealth = factor(v190)
  )

# Check 2022 sample
dim(analysis_sample)
table(analysis_sample$obesity, useNA = "ifany")
table(analysis_sample$hypertension, useNA = "ifany")
table(analysis_sample$diabetes, useNA = "ifany")
table(analysis_sample$burden_group, useNA = "ifany")


# 2017-18 VARIABLE CREATION

analysis_data_2017 <- analysis_data_2017 %>%
  mutate(
    # BMI: DHS stored as BMI * 100
    bmi = if_else(!is.na(ha40) & ha40 < 9990, ha40 / 100, NA_real_),
    
    # Clean BP readings
    sb315a_clean = if_else(!is.na(sb315a) & sb315a < 994, sb315a, NA_real_),
    sb315b_clean = if_else(!is.na(sb315b) & sb315b < 994, sb315b, NA_real_),
    
    sb323a_clean = if_else(!is.na(sb323a) & sb323a < 994, sb323a, NA_real_),
    sb323b_clean = if_else(!is.na(sb323b) & sb323b < 994, sb323b, NA_real_),
    
    sb332a_clean = if_else(!is.na(sb332a) & sb332a < 994, sb332a, NA_real_),
    sb332b_clean = if_else(!is.na(sb332b) & sb332b < 994, sb332b, NA_real_),
    
    # Final BP for 2017:
    # average of 2nd and 3rd readings
    final_sbp_2017 = rowMeans(
      cbind(sb323a_clean, sb332a_clean),
      na.rm = TRUE
    ),
    
    final_dbp_2017 = rowMeans(
      cbind(sb323b_clean, sb332b_clean),
      na.rm = TRUE
    ),
    
    final_sbp_2017 = if_else(is.nan(final_sbp_2017), NA_real_, final_sbp_2017),
    final_dbp_2017 = if_else(is.nan(final_dbp_2017), NA_real_, final_dbp_2017),
    
    # Glucose: 1 implied decimal
    glucose_2017 = if_else(!is.na(sb335b) & sb335b < 994, sb335b / 10, NA_real_),
    
    # Obesity: BMI >= 27.5 kg/m2
    obesity = case_when(
      !is.na(bmi) & bmi >= 27.5 ~ 1,
      !is.na(bmi) & bmi < 27.5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Hypertension:
    # SBP >= 140 OR DBP >= 90 OR taking medication for BP
    hypertension = case_when(
      final_sbp_2017 >= 140 | final_dbp_2017 >= 90 | sb318a == 1 ~ 1,
      !is.na(final_sbp_2017) & !is.na(final_dbp_2017) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Diabetes:
    # fasting plasma glucose >= 7.0 mmol/L OR taking diabetes medication
    diabetes = case_when(
      !is.na(glucose_2017) & glucose_2017 >= 7.0 ~ 1,
      sb327a == 1 ~ 1,
      !is.na(glucose_2017) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Burden count
    burden_count = obesity + hypertension + diabetes,
    
    # Burden group
    burden_group = case_when(
      burden_count == 0 ~ "No burden",
      burden_count == 1 ~ "Single burden",
      burden_count == 2 ~ "Double burden",
      burden_count == 3 ~ "Triple burden",
      TRUE ~ NA_character_
    )
  )

# Final 2017-18 analysis sample
analysis_sample_2017 <- analysis_data_2017 %>%
  filter(
    !is.na(obesity),
    !is.na(hypertension),
    !is.na(diabetes)
  ) %>%
  mutate(
    survey_year = "2017-18",
    sample_weight = v005 / 1000000,
    
    age_group = case_when(
      v012 >= 18 & v012 <= 24 ~ "18-24",
      v012 >= 25 & v012 <= 34 ~ "25-34",
      v012 >= 35 & v012 <= 49 ~ "35-49",
      TRUE ~ NA_character_
    ),
    
    burden_group = factor(
      burden_group,
      levels = c(
        "No burden",
        "Single burden",
        "Double burden",
        "Triple burden"
      )
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    ),
    
    residence = factor(v025),
    education = factor(v106),
    wealth = factor(v190)
  )

# Check 2017-18 sample
dim(analysis_sample_2017)
table(analysis_sample_2017$obesity, useNA = "ifany")
table(analysis_sample_2017$hypertension, useNA = "ifany")
table(analysis_sample_2017$diabetes, useNA = "ifany")
table(analysis_sample_2017$burden_group, useNA = "ifany")


# POOLED DATASET FOR REGRESSION

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
      levels = c(
        "No burden",
        "Single burden",
        "Double burden",
        "Triple burden"
      )
    ),
    
    age_group = factor(
      age_group,
      levels = c("18-24", "25-34", "35-49")
    )
  )

# Check pooled data
dim(analysis_sample_pooled)
table(analysis_sample_pooled$survey_year, useNA = "ifany")
table(
  analysis_sample_pooled$burden_group,
  analysis_sample_pooled$survey_year,
  useNA = "ifany"
)

message("Outcome variables created.")
