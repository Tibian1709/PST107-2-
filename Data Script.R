# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)  # For regression diagnostics

# Import the data set
bank <- read.csv("bank.csv", sep = ";", stringsAsFactors = TRUE)

# Data processing - cleaning and preparing
# Remove rows with missing values
bank <- na.omit(bank)

# Data exploration
# Summary of the dataset
glimpse(bank)
summarize(bank, across(everything(), ~ sum(is.na(.))))  # Check for missing values

# Exploratory Data Analysis (EDA)
# Visualize the distribution of 'balance'
ggplot(bank, aes(x = balance)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Account Balance",
       x = "Account Balance",
       y = "Frequency") +
  theme_minimal()

# Relationship between age and balance
ggplot(bank, aes(x = age, y = balance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Age vs. Account Balance",
       x = "Age",
       y = "Account Balance") +
  theme_minimal()

# Relationship between job and balance
ggplot(bank, aes(x = job, y = balance)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Job vs. Account Balance",
       x = "Job Type",
       y = "Account Balance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Relationship between education and balance
ggplot(bank, aes(x = education, y = balance)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Education Level vs. Account Balance",
       x = "Education Level",
       y = "Account Balance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Creating the linear model
# Predicting 'balance' based on 'age', 'job', and 'education'
balance_model <- lm(balance ~ age + job + education, data = bank)

# Model Summary and Insights
# Check if 'balance_model' was created and view the summary
if (exists("balance_model")) {
  print(summary(balance_model))
  print(anova(balance_model))  # Analysis of variance for further insights
  
  # Check for multicollinearity using VIF (Variance Inflation Factor)
  vif_values <- vif(balance_model)
  print("Variance Inflation Factors (VIF) to check multicollinearity:")
  print(vif_values)
  
  # Check residuals to validate the assumptions of linear regression
  # Plot residuals one by one to avoid issues with multi-plot layout
  plot(balance_model, which = 1)  # Residuals vs Fitted
  plot(balance_model, which = 2)  # Normal Q-Q
  plot(balance_model, which = 3)  # Scale-Location
  plot(balance_model, which = 4)  # Cook's Distance
  
  # Additional residual analysis
  ggplot(data.frame(residuals = resid(balance_model)), aes(x = residuals)) +
    geom_histogram(binwidth = 100, fill = "orange", color = "black") +
    labs(title = "Residuals Distribution",
         x = "Residuals",
         y = "Frequency") +
    theme_minimal()
  
} else {
  print("The linear model 'balance_model' was not created.")
}

# Advanced Analysis - Interaction Effects
# Adding interaction between age and education
balance_model_interaction <- lm(balance ~ age * education + job, data = bank)
summary(balance_model_interaction)

# Visualizing interaction effects
interaction.plot(bank$age, bank$education, bank$balance,
                 col = c("red", "blue", "green"),
                 xlab = "Age", ylab = "Mean Balance",
                 legend = TRUE, main = "Interaction Plot: Age and Education")

# This code performs a complete end-to-end analysis:
# - Loading data and pre-processing to handle missing values.
# - Performing exploratory data analysis (EDA) to understand data distribution and key relationships.
# - Building a linear model, checking for multicollinearity, and assessing residuals for model validation.
# - Advanced modeling by adding interaction effects to improve insights into the relationships within the data set.
# - The results are visualized with clear labels, providing transparency into the model's predictive performance.