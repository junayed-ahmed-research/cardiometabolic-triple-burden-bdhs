# 06_bivariate_analysis.R
# Survey-weighted bivariate analysis

library(tidyverse)
library(survey)

# Function used in the analysis
# Only burden prevalence columns will be converted to percentage

clean_bivariate <- function(x) {
  x %>%
    as_tibble() %>%
    mutate(
      across(
        starts_with("factor(burden_group)"),
        ~ round(.x * 100, 2)
      )
    )
}

# 2022 bivariate analysis

bivariate_age_2022 <- svyby(
  ~factor(burden_group),
  ~age_group,
  bdhs_design,
  svymean,
  na.rm = TRUE
)

bivariate_residence_2022 <- svyby(
  ~factor(burden_group),
  ~residence,
  bdhs_design,
  svymean,
  na.rm = TRUE
)

bivariate_education_2022 <- svyby(
  ~factor(burden_group),
  ~education,
  bdhs_design,
  svymean,
  na.rm = TRUE
)

bivariate_wealth_2022 <- svyby(
  ~factor(burden_group),
  ~wealth,
  bdhs_design,
  svymean,
  na.rm = TRUE
)

bivariate_age_2022_clean <- clean_bivariate(bivariate_age_2022)
bivariate_residence_2022_clean <- clean_bivariate(bivariate_residence_2022)
bivariate_education_2022_clean <- clean_bivariate(bivariate_education_2022)
bivariate_wealth_2022_clean <- clean_bivariate(bivariate_wealth_2022)

# 2017-18 bivariate analysis

bivariate_age_2017 <- svyby(
  ~factor(burden_group),
  ~age_group,
  bdhs_design_2017,
  svymean,
  na.rm = TRUE
)

bivariate_residence_2017 <- svyby(
  ~factor(burden_group),
  ~residence,
  bdhs_design_2017,
  svymean,
  na.rm = TRUE
)

bivariate_education_2017 <- svyby(
  ~factor(burden_group),
  ~education,
  bdhs_design_2017,
  svymean,
  na.rm = TRUE
)

bivariate_wealth_2017 <- svyby(
  ~factor(burden_group),
  ~wealth,
  bdhs_design_2017,
  svymean,
  na.rm = TRUE
)

bivariate_age_2017_clean <- clean_bivariate(bivariate_age_2017)
bivariate_residence_2017_clean <- clean_bivariate(bivariate_residence_2017)
bivariate_education_2017_clean <- clean_bivariate(bivariate_education_2017)
bivariate_wealth_2017_clean <- clean_bivariate(bivariate_wealth_2017)

# Save outputs

write_csv(
  bivariate_age_2022_clean,
  "outputs/tables/bivariate_age_2022.csv"
)

write_csv(
  bivariate_residence_2022_clean,
  "outputs/tables/bivariate_residence_2022.csv"
)

write_csv(
  bivariate_education_2022_clean,
  "outputs/tables/bivariate_education_2022.csv"
)

write_csv(
  bivariate_wealth_2022_clean,
  "outputs/tables/bivariate_wealth_2022.csv"
)

write_csv(
  bivariate_age_2017_clean,
  "outputs/tables/bivariate_age_2017.csv"
)

write_csv(
  bivariate_residence_2017_clean,
  "outputs/tables/bivariate_residence_2017.csv"
)

write_csv(
  bivariate_education_2017_clean,
  "outputs/tables/bivariate_education_2017.csv"
)

write_csv(
  bivariate_wealth_2017_clean,
  "outputs/tables/bivariate_wealth_2017.csv"
)

# Print outputs

bivariate_age_2017_clean
bivariate_age_2022_clean

bivariate_residence_2017_clean
bivariate_residence_2022_clean

bivariate_education_2017_clean
bivariate_education_2022_clean

bivariate_wealth_2017_clean
bivariate_wealth_2022_clean

message("Bivariate analysis completed.")
