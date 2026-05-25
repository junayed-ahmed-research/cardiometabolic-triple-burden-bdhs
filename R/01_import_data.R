# 02_import_data.R
# Import BDHS 2017-18 and 2022 data

library(tidyverse)
library(haven)

# Import 2022 BDHS data

ir_data <- read_dta("data/raw/BDIR81FL.DTA")
pr_data <- read_dta("data/raw/BDPR81FL.DTA")

# Import 2017-18 BDHS data

ir_data_2017 <- read_dta("data/raw/BDIR7RFL.DTA")
pr_data_2017 <- read_dta("data/raw/BDPR7RFL.DTA")

# Quick checks

dim(ir_data)
dim(pr_data)

dim(ir_data_2017)
dim(pr_data_2017)

names(ir_data)[1:10]
names(pr_data)[1:10]

names(ir_data_2017)[1:10]
names(pr_data_2017)[1:10]

message("Data import completed.")
