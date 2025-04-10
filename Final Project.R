Hello

## Load library and packages ## ----------
library(tidyverse)
library(readr)
library(dplyr)

## Set working directory & Load dataset ## ----------
lungcancer <- read_csv('lung_cancer_mortality_data_large_v2.csv')
# read_csv is faster than read.csv at reading large dataset

glimpse(lungcancer)


## -------- Data Cleaning -------- ##

## Check for missing data
sum(is.na(lungcancer)) # there is no missing data

# Check what values are in this variable
unique(lungcancer$family_history)

## Convert into binary data -------
lungcancer$family_history = ifelse(lungcancer$family_history == 'Yes', 1, 0)


## Convert chr data into factors -------
lungcancer <- lungcancer |>
  mutate(across(where(is.character), as.factor))

str(lungcancer) # check if it works


## -------- EDA -------- ##

# Distribution plots
lungcancer |>
  select(where(is.numeric)) |>
  pivot_longer(cols = -c(survived, id)) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~name, scales = "free")
 
ggplot(lungcancer, aes(x = treatment_type, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Survived")

table(lungcancer$treatment_type)

## -------- Modeling -------- ##

## Research question 1: -------
# Can we predict whether a patient will survive based on pre-treatment 
#health profile and cancer stage?
Q1 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer,
          data=lungcancer, family='binomial')
summary(Q1)

## Research question 2: -------
#
Q2 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer + treatment_type,
          data=lungcancer, family='binomial')

# Add in necessary variable:

# Assumption check:





