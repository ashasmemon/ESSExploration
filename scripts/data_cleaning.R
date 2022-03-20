# Purpose: Clean the survey data downloaded from European Social Survey
# Author: Ashas Memon, Basil Wong & Tamsen Yau
# Data: 6th March 2022
# Contact: basilw99@gmail.com
# License: MIT

# Pre-requisites: 
# - Need to have downloaded the ESS data and saved it to inputs/data
# - Don't forget to gitignore it!
# Please follow the instructions in README before running the code
install.packages("haven")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("scales")
install.packages("knitr")
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
        'IE' = 'Ireland',
        'IT' = 'Italy',
        'IS' = 'Iceland',
        'LV' = 'Latvia',
        'LT' = 'Lithuania',
        'SK' = 'Slovakia',
        'SI' = 'Slovenia',
        'ES' = 'Spain',
        'GB' = 'UK',
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


# Graphs of how happy people are in different countries
# get the mean of the score of different countries
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


# get the data of the cntry happiness and summarise them with their mean
# rename cntry to region
cntry_happiness <- subset(survey_data, select = c(cntry, happy))
mapping <- 
  cntry_happiness %>%
  drop_na(happy) %>%
  group_by(cntry) %>%
  summarise_at(vars(happy), list(score = mean)) %>%
  rename(region = cntry)

# generate the world and left join it with our mapping data frame
mapdata <- map_data("world")
view(mapdata)
mapdata <- left_join(mapdata, mapping, by='region')

# filter those data that has no score
mapdata1 <- mapdata %>% filter(!is.na(mapdata$score))

# plot the map out and colour them
happiness_map <- ggplot(mapdata1, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=score), color = "black") 

happiness_map <- happiness_map +scale_fill_gradient(name = "score", low = "cyan", high = "purple", na.value = "grey50") +
  theme_void()
happiness_map


# How often socially meet with friends, relatives or colleagues
social <- subset(survey_data, select = c(cntry, sclmeet)) 
unique(social$sclmeet)

social <- social %>%
  group_by(sclmeet)

social_table <- table(social$sclmeet)

# get all the labels of the chart
lbls <- c("Never", "Less than once a month", "Once a month", 
            "Several times a month", "Once a week", "Several times a week",
            "Everyday", "Refusal", "Don't know", "No answer")

s_data <- data.frame(lbls, social_table)
s_data <- subset(s_data, select = c(lbls, Freq))

ggplot(s_data, aes(x="", y=Freq, fill=lbls)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_minimal() +
  labs(x="", y="")

# get the percentage of each section and plot the graph using pie function
# not by country, only whole europe
# pct <- round(social_table/sum(social_table)*100, 1)
# lbls <- paste(lbls, pct) #add percents to labels
# lbls <- paste(lbls, "%", sep="")
# social_chart <- pie(social_table, labels = lbls, col=rainbow(length(lbls)), 
#                     main="Pie Chart of Countries", radius = 1, cex=1)


# plot the graph of how emotionally attached to their countries and Europe
emotional_attach <- subset(survey_data, select = c(cntry, atchctr, atcherp))
# convert them from int to char
emotional_attach$atchctr <- as.character(emotional_attach$atchctr)
emotional_attach$atcherp <- as.character(emotional_attach$atcherp)

# plot the graphs
country_emo <- emotional_attach %>%
  drop_na(atchctr) %>%
  ggplot(mapping = aes(x=atchctr)) +
  geom_histogram(stat= 'count', color="darkblue", fill="lightblue") +
  theme_minimal() +
  labs(x= "How emotionally attached to their own countries")


eu_emo <- emotional_attach %>%
  drop_na(atcherp) %>%
  ggplot(mapping = aes(x=atcherp)) +
  geom_histogram(stat= 'count', color="darkred", fill="red") +
  theme_minimal() +
  labs(x= "How emotionally attached to Europe")

grid.arrange(country_emo, eu_emo, nrow=1)

# get the data related to religious
religious <- subset(survey_data, select = c(cntry, rlgdnm, rlgdnme))
religious$rlgdnm <- as.character(religious$rlgdnm)
religious$rlgdnme <- as.character(religious$rlgdnme)

religious <- 
  religious %>%
  mutate(
    rlgdnm = 
           recode(
             rlgdnm,
             '1' = 'Roman Catholic',
             '2' = 'Protestant',
             '3' = 'Eastern Orthodox',
             '4' = 'Other Christian denomination',
             '5' = 'Jewish',
             '6' = 'Islam',
             '7' = 'Eastern religions',
             '8' = 'Other Non-Christian religions',
             '66' =	'Not applicable',
             '77' =	'Refusal',
             '99' =	'No answer'
             
           ))

religious <- 
  religious %>%
  mutate(
    rlgdnme = 
      recode(
        rlgdnme,
        '1' = 'Roman Catholic',
        '2' = 'Protestant',
        '3' = 'Eastern Orthodox',
        '4' = 'Other Christian denomination',
        '5' = 'Jewish',
        '6' = 'Islam',
        '7' = 'Eastern religions',
        '8' = 'Other Non-Christian religions',
        '66' =	'Not applicable',
        '77' =	'Refusal',
        '99' =	'No answer'
        
      ))

current_reli <- religious %>%
  drop_na(rlgdnm) %>%
  ggplot(mapping = aes(x=rlgdnm)) +
  geom_histogram(stat= 'count', color="darkgoldenrod", fill="darkgoldenrod1") +
  theme_minimal() +
  labs(x= "Religion or denomination \n belonging to at present") +
  coord_flip()

past_reli <- religious %>%
  drop_na(rlgdnme) %>%
  ggplot(mapping = aes(x=rlgdnme)) +
  geom_histogram(stat= 'count', color="green", fill="lightgreen") +
  theme_minimal() +
  labs(x= "Religion or denomination \n belonging to in the past") +
  coord_flip()

grid.arrange(current_reli, past_reli, nrow=2)







