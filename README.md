# Cardiometabolic Triple Burden among Women in Bangladesh: Analysis of BDHS 2017–18 and 2022 Data

This repository contains R code for analyzing cardiometabolic triple burden among women in Bangladesh using Bangladesh Demographic and Health Survey data from 2017–18 and 2022.

## Analysis overview

The analysis includes data cleaning, harmonization across survey waves, construction of cardiometabolic burden outcomes, survey-weighted descriptive analysis, bivariate analysis, regression modelling, temporal decomposition, adjusted predicted probabilities, sensitivity analysis, and socioeconomic inequality analysis.

## Outcome definitions

Obesity was defined as BMI ≥27.5 kg/m². Hypertension was defined as systolic blood pressure ≥140 mmHg, diastolic blood pressure ≥90 mmHg, or current use of antihypertensive medication. Diabetes was defined as fasting plasma glucose ≥7.0 mmol/L or use of glucose-lowering medication.

The burden outcome was categorized as no burden, single burden, double burden, and triple burden. For adjusted regression analysis, double and triple burden were combined as multiple burden due to sparse triple-burden cells.

## Data source

The analysis used BDHS 2017–18 and BDHS 2022 data. Raw DHS datasets are not included in this repository because of DHS data-use restrictions. Data can be requested from the DHS Program.

## Main analyses

- Survey-weighted prevalence estimation
- Burden category comparison between survey years
- Survey-weighted multinomial regression
- Temporal decomposition of change in multiple burden
- Adjusted predicted probabilities
- Sensitivity analysis
- Concentration index and concentration curve

## Software

The analysis was conducted in R using tidyverse, haven, survey, svyVGAM, ggplot2, officer, flextable, and related packages.
