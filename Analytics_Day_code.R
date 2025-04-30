## Load library and packages ## ----------
install.packages('caret')
library(caret)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)


## ---------- Set working directory & Load dataset ---------- ##
lungcancer <- read_csv('lung_cancer_mortality_data_test_v2.csv')
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


## SMOKING STATUS ## --------

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

# Survival Rate by Treatment type and cancer stage
lungcancer |>
  group_by(treatment_type, cancer_stage) |>
  summarise(survival_rate = mean(survived)) |>
  ggplot(aes(x = cancer_stage, y = survival_rate, fill = treatment_type)) +
  geom_col(position = "dodge") + # To make sure the bars are displayed side by side
  geom_text(aes(label = scales::percent(survival_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.9), # tells ggplot to place bars side by side, control how much space is between them. 
            vjust = -0.5) + # Set the space between the text and the bar
  labs(title = "Survival Rate by Treatment Type and cancer Stage", 
       y = "Survival Rate", 
       x = "Treatment and Stage") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "lightyellow"))


## -------- Logistic Regression Modeling -------- ##

# Model 1: How is smoking status associated with the likelihood of survival among lung cancer patients?
Model1 <- glm(survived ~ smoking_status, data=lungcancer, family='binomial')
summary(Model1)


# Model 2: How do cancer stage and treatment type jointly affect survival outcomes in lung cancer patients?
Model2 <- glm(survived ~ cancer_stage + treatment_type,
              data=lungcancer, family='binomial')
summary(Model2)


## -------- Assumption Check -------- ##

## Check for any unusual strong outliers -------

## Model 1
which(abs(rstandard(Model1)) > 2)
## Model 2
which(abs(rstandard(Model2)) > 2)

## Linearity & Independence - Residuals vs Fitted Values --------

# Model 1
plot(Model1, which = 1, main = "Residuals vs Fitted")
# Model 2
plot(Model2, which = 1, main = "Residuals vs Fitted")

## Multicollinearity check - VIF -------
library(car)

# Model 1 only has 1 predictor so no need for multicollinearity check
# Model 2
vif(Model2)


## ASSUMPTION CHECKS THAT ARE NOT APPLICABLE TO LOGISTIC REGRESSION ------

## Homoscedasticity - Spread-Location plot --------

# Model 1
plot(Model1, which = 3, main = "Scale-Location")
# Model 2
plot(Model2, which = 3, main = "Scale-Location")

## Normality -------


##----------------------------Pie Chart------------------------------##
# Set up 2 rows and 3 columns layout for the pie charts

par(mfrow = c(2,2),
    mar = c(1,1,1,1),
    oma = c(0,0,0,0))

# List all the variables to plot in the pie chart 

vars <- list(
  "Treatment Types" = lungcancer$treatment_type,
  "Cancer Stage" = lungcancer$cancer_stage,
  "Smoking Status" = lungcancer$smoking_status,
  "Survived" = lungcancer$survived
)

# Define colors for each variable
colors_list <- list(
  "Treatment Types" = c("lightblue", "lightgreen", "lightcoral", "lightyellow" ),
  "Cancer Stage" = c("Red", "orange", "yellow", "green"),
  "Smoking Status" = c("purple", "pink", "blue", "cyan"),
  "Survived" = c("lightgreen", "lightpink")
  
)

# Plot the pie chart
#Set the variables that needed for plotting
for (title in names(vars)) {
  variable_counts <- table(vars[title]) # Get the variabel name 
  percentage <- round(100 * variable_counts / sum(variable_counts), 1) # Calculate the percentage for each variable
  labels <- paste(names(variable_counts), ": ", percentage, "%")  # Create the labels
  colors <- colors_list[[title]] # Set the color for the pie chart
  
  # Plot the pie chart with title and legend
  pie(variable_counts,
      labels = NA,
      col= colors,
      radius = 0.2)
  
  title(main = title, line = -10, cex.main = 1.6) # Set the features for the text 
  legend("bottom", legend = labels, fill=colors, bty="n", inset=0.2, cex = 1.2) # Set the features for the legends 
  
}


