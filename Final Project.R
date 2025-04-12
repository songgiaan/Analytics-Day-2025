## Load library and packages ## ----------
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

## Set working directory & Load dataset ## ----------
lungcancer <- read_csv('lung_cancer_mortality_data_test_v2_small.csv')
# read_csv is faster than read.csv at reading large dataset

glimpse(lungcancer)


## -------- Data Cleaning / EDA -------- ##

## Check for missing data
sum(is.na(lungcancer)) # there is no missing data

# Check what values are in this variable
#unique(lungcancer$family_history)

## Convert into binary data -------
#lungcancer$family_history = ifelse(lungcancer$family_history == 'Yes', 1, 0)


## Convert chr data into factors -------
lungcancer <- lungcancer |>
  mutate(across(where(is.character), as.factor))

str(lungcancer) # check if it works

# Convert binary data into factors
lungcancer <- lungcancer |>
  mutate(across(c("hypertension", "asthma", "cirrhosis", "other_cancer", "survived")))

## -------- EDA -------- ##
table(lungcancer$survived)

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

lungcancer %>%
  count(treatment_type, survived) %>%
  ggplot(aes(x = treatment_type, y = n, fill = factor(survived))) +
  geom_col(position = "fill") +
  labs(title = "Survival Rate by Treatment Type", 
       x = "Treatment Type", 
       y = "Proportion", fill = "Survived")


ggplot(lungcancer, aes(x = smoking_status, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Survived")

## -------- Modeling -------- ##

## Research question 1: -------
# Can we predict whether a patient will survive based on pre-treatment 
#health profile and cancer stage?
Q1 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer,
          data=lungcancer, family='binomial')
summary(Q1)
exp(coef(Q1))     # Find odds ratio

# Predict response variable
Q1_predicted <- predict(Q1, type = "response")
predicted_class <- ifelse(Q1_predicted > 0.5, 1, 0)

# check prediction accuracy
mean(predicted_class == lungcancer$survived)

table(Predicted = predicted_class, Actual = lungcancer$survived)

## Research question 2: -------
#
Q2 <- glm(survived ~ age + gender + cancer_stage + family_history
          + smoking_status + bmi + cholesterol_level + hypertension
          + asthma + cirrhosis + other_cancer + treatment_type,
          data=lungcancer, family='binomial')
summary(Q2)

# Predict response variable
Q2_predicted <- predict(Q2, type = "response")
predicted_class_2 <- ifelse(Q2_predicted > 0.5, 1, 0)

# check prediction accuracy
mean(predicted_class_2 == lungcancer$survived)

table(Predicted = predicted_class_2, Actual = lungcancer$survived)

# Add in necessary variable:

# Assumption check:


lungcancer %>%
  count(hypertension, survived) %>%
  ggplot(aes(x = factor(hypertension), y = n, fill = factor(survived))) +
  geom_col(position = "fill") +
  labs(title = "Survival Rate by Hypertension Status", 
       x = "Hypertension (0 = No, 1 = Yes)", 
       y = "Proportion", fill = "Survived")


# show survival rate
lungcancer %>%
  group_by(treatment_type) %>%
  summarise(
    total = n(),
    survival_rate = mean(survived)
  ) %>%
  ggplot(aes(x = treatment_type, y = survival_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(survival_rate)), vjust = -0.5) +
  labs(title = "Survival Rate by Treatment Type", 
       x = "Treatment Type", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format())

# OR 
lungcancer %>%
  group_by(treatment_type) %>%
  summarise(survival_rate = mean(survived)) %>%
  ggplot(aes(x = treatment_type, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Treatment Type", y = "Survival Rate", x = "Treatment") +
  scale_y_continuous(labels = scales::percent_format())

## Assumption check:
# Linearity check
lungcancer_test <- lungcancer %>% mutate(log_age = log(age + 1))
model_test <- glm(survived ~ age + log_age + ..., data = lungcancer_test, family = "binomial")
summary(model_test)


# Multicollinearity
library(car)
vif(Q1)

plot(Q1, which = 4)  # Cook’s Distance


