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

## -----------------------------Plot Pie Charts -----------------------------------------##
# Set up 2 rows and 3 columns layout for the pie charts

par(mfrow = c(2,3),
    mar = c(1,1,1,1),
    oma = c(0,0,0,0))

# Frequency 
treatment_type_counts <- table(lungcancer$treatment_type)
gender_counts <- table(lungcancer$gender)
cancer_stage_counts <- table(lungcancer$cancer_stage)
age_group_counts <- table(lungcancer$age_group)
smoking_status_counts <- table(lungcancer$smoking_status)
survived_counts <- table(lungcancer$survived)

# Calcuate the percentage of each variable
treatment_type_pct <- round(100*treatment_type_counts/sum(treatment_type_counts), 1)
gender_pct <- round(100*gender_counts/sum(gender_counts), 1)
cancer_stage_pct <- round(100*cancer_stage_counts/sum(cancer_stage_counts), 1)
age_group_pct <- round(100*age_group_counts/sum(age_group_counts), 1)
smoking_status_pct <- round(100*smoking_status_counts/sum(smoking_status_counts), 1)
survived_pct <- round(100*survived_counts/sum(survived_counts), 1)

# Create a vector labels
treatment_label <- names(treatment_type_counts)
gender_label <- names(gender_counts)
cancer_stage_label <- names(cancer_stage_counts)
age_group_label <- names(age_group_counts)
smoking_status_label <- names(smoking_status_counts)
survived_label <- names(survived_counts)


# Create vector of colors
combined <- c("cancer_stage", "gender", "treatment_type", "survived", "age_group", "smoking_status")

colors <- rainbow(length(combined))

# Create legend label
treatment_legend_labels <- paste(treatment_label, ": ", treatment_type_pct, "%")
gender_legend_labels <- paste(gender_label, ": ", gender_pct, "%")
stage_legend_labels <- paste(cancer_stage_label, ": ", cancer_stage_pct, "%")
age_legend_labels <- paste(age_group_label, ": ", age_group_pct, "%")
smoking_legend_labels <- paste(smoking_status_label, ": ", smoking_status_pct, "%")
survived_legend_labels <- paste(survived_label, ": ", survived_pct, "%")

# Plot pie charts

# Pie 1: Treatment Type

pie(treatment_type_counts, 
    labels = NA,
    col = colors)

title(main=" Treatment Types", line = -9, cex.main = 1.2)
# Add the legend
legend("bottom", legend = treatment_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)

# Pie 2: Gender
pie(gender_counts,
    labels = NA,
    col = colors)

title(main=" Gender", line = -9, cex.main = 1.2)
legend ("bottom", legend = gender_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)

# Pie 3: Cancer stages
pie(cancer_stage_counts,
    labels = NA,
    col = colors)

title(main="Cancer Stages", line = -9, cex.main = 1.2)
legend("bottom", legend = stage_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)

# Pie 4: Age Group
pie(age_group_counts,
    labels = NA,
    col = colors)

title(main="Age Group", line = -9, cex.main = 1.2)
legend("bottom", legend = age_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)

# Pie 5: Smoking Status
pie(smoking_status_counts,
    labels = NA,
    col = colors)

title(main="Smoking Status", line = -9, cex.main = 1.2)

legend("bottom", legend = smoking_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)

# Pie 6: Survived 
pie(survived_counts,
    labels = NA,
    col = colors)
    
title(main="Survived", line = -9, cex.main = 1.2)

legend("bottom", legend = survived_legend_labels, fill = colors, bty="n", inset = 0.1, cex = 0.8)
##----------------- End Plot Pie Chart -----------------------------##

