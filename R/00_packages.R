# 01_setup.R
# Project setup for BDHS cardiometabolic triple burden analysis

# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(haven)
library(survey)
library(labelled)

# Survey design option
# This prevents error when a stratum has only one PSU
options(survey.lonely.psu = "adjust")

# Create output folders if they do not exist
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)

# Check working directory
getwd()

# Confirm setup is complete
message("Package setup completed.")
