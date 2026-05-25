# 03_clean_data.R
# Clean and merge BDHS IR + PR files

library(tidyverse)

# 2022 DATA CLEANING

# Select required variables from women's individual recode file
women_ir_2022 <- ir_data %>%
  select(
    v001,   # cluster number
    v002,   # household number
    v003,   # respondent line number
    v005,   # sample weight
    v012,   # age
    v022,   # sample strata
    v025,   # residence
    v106,   # education
    v190,   # wealth index
    v445    # BMI * 100
  )

# Select required variables from household/member biomarker file
women_pr_2022 <- pr_data %>%
  select(
    hv001,   # cluster number
    hv002,   # household number
    hvidx,   # line number
    wbp24,   # final systolic blood pressure
    wbp25,   # final diastolic blood pressure
    wbp19,   # taking medication to control BP
    sb267g,  # fasting plasma glucose, mmol/L
    sb240    # taking medication for diabetes
  )

# Merge 2022 IR and PR files
analysis_data <- women_ir_2022 %>%
  left_join(
    women_pr_2022,
    by = c(
      "v001" = "hv001",
      "v002" = "hv002",
      "v003" = "hvidx"
    )
  )

# Check merged 2022 data
dim(analysis_data)


# 2017-18 DATA CLEANING

# Select required variables from women's individual recode file
women_ir_2017 <- ir_data_2017 %>%
  select(
    v001,   # cluster number
    v002,   # household number
    v003,   # respondent line number
    v005,   # sample weight
    v012,   # age
    v022,   # sample strata
    v025,   # residence
    v106,   # education
    v190    # wealth index
  )

# Select required variables from household/member biomarker file
women_pr_2017 <- pr_data_2017 %>%
  select(
    hv001,   # cluster number
    hv002,   # household number
    hvidx,   # line number
    ha40,    # BMI * 100
    
    sb315a,  # 1st systolic BP
    sb315b,  # 1st diastolic BP
    sb323a,  # 2nd systolic BP
    sb323b,  # 2nd diastolic BP
    sb332a,  # 3rd systolic BP
    sb332b,  # 3rd diastolic BP
    
    sb318a,  # taking medication for BP
    sb335b,  # plasma blood glucose, 1 implied decimal
    sb327a   # taking medication for diabetes
  )

# Merge 2017-18 IR and PR files
analysis_data_2017 <- women_ir_2017 %>%
  left_join(
    women_pr_2017,
    by = c(
      "v001" = "hv001",
      "v002" = "hv002",
      "v003" = "hvidx"
    )
  )

# Check merged 2017-18 data
dim(analysis_data_2017)


# Final check

names(analysis_data)
names(analysis_data_2017)

message("Data cleaning completed.")
