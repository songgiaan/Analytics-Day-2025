## Load library and packages ## ----------
install.packages('caret')
library(caret)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)


## ---------- Set working directory & Load dataset ---------- ##
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


# Distribution of Survival rates
ggplot(lungcancer, aes(x = survived)) + 
  geom_bar(fill = "lightgreen") + labs(title = "Survival Distribution")


## TREATMENT TYPE ## --------

# Survival Proportion by Treatment type
ggplot(lungcancer, aes(x = treatment_type, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Treatment Type",
       x = "Treatment Type",
       y = "Proportion", fill = "Survived")

# Survival Rate by Treatment type (**PREFERRED** )
lungcancer |>
  group_by(treatment_type) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = treatment_type, y = survival_rate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Survival Rate by Treatment Type", y = "Survival Rate", x = "Treatment") +
  scale_y_continuous(labels = scales::percent_format())


## SMOKING STATUS ## --------

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


## CANCER STAGE ## --------

# Survival Proportion by Cancer Stage
ggplot(lungcancer, aes(x = cancer_stage, fill = factor(survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Cancer Stage",
       x = "Cancer Stage",
       y = "Proportion", fill = "Survived")

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



## -------- Modeling -------- ##

# Model 1: How is smoking status associated with the likelihood of survival among lung cancer patients?
Model1 <- glm(survived ~ smoking_status, data=lungcancer, family='binomial')
summary(Model1)


# Model 2: How do cancer stage and treatment type jointly affect survival outcomes in lung cancer patients?
Model2 <- glm(survived ~ cancer_stage + treatment_type,
              data=lungcancer, family='binomial')
summary(Model2)


## -------- Assumption Check -------- ##

# Normality check for continuous variable (age) - QQ plot
qqnorm(lungcancer$age)
qqline(lungcancer$age, col = "red")

# Multicollinearity check
library(car)

vif(Model1)
vif(Model2)






