# Purpose: Clean the survey data downloaded from European Social Survey
# Author: Ashas Memon, Basil Wong & Tamsen Yau
# Data: 6th March 2022
# Contact: basilw99@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ESS data and saved it to inputs/data
# - Don't forget to gitignore it!


library(haven)
library(tidyverse)
library(dplyr)
# Read in the raw data. 
raw_data <- readr::read_csv("../inputs/data/ESS9e03_1.csv"
)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

# Removing columns where all values are NA
reduced_data <- 
  raw_data[ , colSums(is.na(raw_data)) < nrow(raw_data)]

# Get all the data related to Subjective well-being, social exclusion, religion,
# national and ethnic identity
europe_data <- select(raw_data, c(1:6, 115:200))
names(europe_data)

rm(raw_data)


#### What's next? ####
