# Purpose: Clean the survey data downloaded from European Social Survey
# Author: Ashas Memon, Basil Wong & Tamsen Yau
# Data: 6th March 2022
# Contact: basilw99@gmail.com
# License: MIT

# Pre-requisites: 
# - Need to have downloaded the ESS data and saved it to inputs/data
# - Don't forget to gitignore it!
# Please follow the instructions in README before running the code

library(haven)
library(tidyverse)
library(dplyr)
library(janitor) # Helps clean datasets
library(tidyr) # Helps make tidy datasets
library(ggplot2)
library(lubridate)
library(gridExtra)
library(scales)
library(knitr)


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

# Write the Europe data to our raw data csv
write_csv(
  x=europe_data,
  file = "../inputs/data/raw_data.csv"
)
  
# Clean names
survey_data <-
  read.csv("../inputs/data/raw_data.csv")

# Clear the memory of the raw and reduced data
rm(raw_data)
rm(reduced_data)


# Recode the country name so that people can easily understand the meaning of the code
# Info of the country code is obtained from https://www.iban.com/country-codes
survey_data <-
  survey_data %>%
  mutate(
    cntry = 
      recode(
        cntry,
        'AT' = 'Austria',
        'BE' = 'Belgium',
        'BG' = 'Bulgaria',
        'HR' = 'Croatia',
        'CY' = 'Cyprus',
        'CZ' = 'Czech Republic',
        'DK' = 'Denmark',
        'EE' = 'Estonia',
        'FI' = 'Finland',
        'FR' = 'France',
        'DE' = 'Germany',
        'GR' = 'Greece',
        'HU' = 'Hungary',
        'IE' = 'Ireland, Republic of (EIRE)',
        'IT' = 'Italy',
        'IS' = 'Iceland',
        'LV' = 'Latvia',
        'LT' = 'Lithuania',
        'SK' = 'Slovakia',
        'SI' = 'Slovenia',
        'ES' = 'Spain',
        'GB' = 'United Kingdom',
        'PT' = 'Portugal',
        'PL' = 'Poland',
        'NL' = 'Netherlands',
        'SE' = 'Sweden',
        'RS' = 'Serbia',
        'NO' = 'Norway',
        'ME' = 'Montenegro',
        'CH' = 'Switzerland'
      )
  )


# Graphs of how happy people are in different country
happiness <- 
  survey_data %>%
  drop_na(happy) %>%
  group_by(cntry) %>%
  summarise_at(vars(happy), list(score = mean)) %>%
  ggplot(mapping = aes(x=cntry, y=score)) +
  geom_bar(stat = "identity", width = 0.6, color="darkgoldenrod", fill="darkgoldenrod1") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10)) +
  coord_flip() +
  labs(x="Countries", y="Level of Happiness")
  
happiness


# How hampered in daily life
daily_activities <-
  table(survey_data['cntry', 'hlthhmp'])
  
temp <-  
  summarise(group_by(survey_data, cntry, hlthhmp), count=n())
 
  


daily_activities  






