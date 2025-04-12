## Load library and packages ## ----------
install.packages('caret')
library(caret)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)


## Set working directory & Load dataset ## ----------
lungcancer <- read_csv('lung_cancer_mortality_data_test_v2_small.csv')
# read_csv is faster than read.csv at reading large dataset

glimpse(lungcancer)


## -------- Data Cleaning & Preprocessing -------- ##

## Check for missing data
sum(is.na(lungcancer)) # there is no missing data


## Convert chr data into factors -------
lungcancer <- lungcancer |>
  mutate(across(where(is.character), as.factor))

# Convert binary data into factors
lungcancer <- lungcancer |>
  mutate(across(c("hypertension", "asthma", "cirrhosis", "other_cancer", "survived")))

str(lungcancer) # check if it works



## -------- EDA -------- ##

# General distribution plots on all numeric variables
lungcancer |>
  select(where(is.numeric)) |>
  pivot_longer(cols = -c(survived, id)) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~name, scales = "free")

# Age range
range(lungcancer$age)
# Breaking age groups
lungcancer$age_group <- cut(lungcancer$age,
                    breaks = c(seq(0, 90, by = 15), 100),
                    labels = c("0-15", "16-30", "31-45", "46-60", "61-75", "76-90", "91-100"),
                    right = FALSE)

# Distribution of Age Groups visualization
ggplot(lungcancer, aes(x = age_group)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Age Groups Distribution",
       x = "Age Group",
       y = "Count") +
  theme_minimal()


# Distribution of Survival rates
ggplot(lungcancer, aes(x = survived)) + 
  geom_bar(fill = "lightgreen") + labs(title = "Survival Distribution")

# Distribution of Age
ggplot(lungcancer, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "lightgreen", color='black') + labs(title = "Age Distribution")


# Survival Proportion by Treatment type
ggplot(lungcancer, aes(x = treatment_type, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Treatment Type",
       x = "Treatment Type",
       y = "Proportion", fill = "Survived")

# Survival Rate by Treatment type
lungcancer |>
  group_by(treatment_type) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = treatment_type, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Treatment Type", y = "Survival Rate", x = "Treatment") +
  scale_y_continuous(labels = scales::percent_format())


# Survival Proportion by Age Groups
ggplot(lungcancer, aes(x = age_group, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Age Group",
       x = "Age Group",
       y = "Proportion", fill = "Survived")

# Survival Rate by Age Groups
lungcancer |>
  group_by(age_group) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = age_group, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Age Groups", y = "Survival Rate", x = "Age Group") +
  scale_y_continuous(labels = scales::percent_format())


# Survival Proportion by Smoking Status
ggplot(lungcancer, aes(x = smoking_status, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Smoking Status",
       x = "Smoking Status",
       y = "Proportion", fill = "Survived")

# Survival Rate by Smoking Status
lungcancer |>
  group_by(smoking_status) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = smoking_status, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Smoking Status", y = "Survival Rate", x = "Smoking Status") +
  scale_y_continuous(labels = scales::percent_format())


# Survival Rate by Cancer Stage
lungcancer |>
  group_by(cancer_stage) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = cancer_stage, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Cancer Stage", y = "Survival Rate", x = "Cancer Stage") +
  scale_y_continuous(labels = scales::percent_format())


# Normality check for continuous variable (age) - QQ plot
qqnorm(lungcancer$age)
qqline(lungcancer$age, col = "red")


## -------- Assumption Check -------- ##



## -------- Modeling -------- ##

## Research question 1: -------
# Which pre-treatment factors (age, smoking status, cancer stage, etc.) 
# are significantly associated with higher survival rates?

Q1 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer,
          data=lungcancer, family='binomial')
summary(Q1)



## Research question 2: -------
# Which treatment types (surgery, chemotherapy, radiation, or combined) 
# are associated with higher mortality rates?
Q2 <- glm(survived ~ treatment_type, 
          data=lungcancer, family = 'binomial')

summary(Q2)


## Research question 3: -------
# Can we predict the likelihood of survival based on 
# a combination of pre-treatment factors and treatment types?
Q3 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer,
          data=lungcancer, family = 'binomial')

# View the model summary
summary(Q3)
