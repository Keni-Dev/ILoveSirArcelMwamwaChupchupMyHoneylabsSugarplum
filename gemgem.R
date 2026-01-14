# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================
# Load necessary libraries for data manipulation, visualization, and modeling
library(tidyverse)  # Includes ggplot2, dplyr, tidyr, readr
library(corrplot)   # For correlation heatmaps
library(broom)      # For tidying statistical model outputs
library(GGally)     # For advanced pair plots (optional, but great for EDA)

# Load the data
# Ensure 'Restaurant Satisfaction.csv' is in your working directory

df <- read_csv("01 - Flat files/Restaurant Satisfaction.csv")
# ==============================================================================
# 2. DATA PREPROCESSING & CLEANING
# ==============================================================================
# Check for missing values
sum(is.na(df)) 

# Convert categorical variables to Factors
# We assume 'sex' is 0/1 (e.g., Male/Female) and 'store' is categorical
df <- df %>%
  mutate(
    sex = as.factor(sex),
    store = as.factor(store)
  )

# Inspect the structure after conversion
glimpse(df)

# ==============================================================================
# 3. DESCRIPTIVE STATISTICS
# ==============================================================================
# Summary of all variables
summary(df)

# Detailed breakdown: Average ratings by Store
store_summary <- df %>%
  group_by(store) %>%
  summarise(
    Count = n(),
    Avg_Overall = mean(overall),
    Avg_Food = mean(food_bev),
    Avg_Service = mean(service),
    SD_Overall = sd(overall) # Standard Deviation to check consistency
  ) %>%
  arrange(desc(Avg_Overall))

print("--- Summary by Store ---")
print(store_summary)

# ==============================================================================
# 4. EXPLORATORY DATA ANALYSIS (EDA) & VISUALIZATION
# ==============================================================================

# A. Distribution of Overall Satisfaction
# Is the data normally distributed or skewed?
ggplot(df, aes(x = overall)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean(overall)), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Overall Satisfaction Scores",
       subtitle = "Red dashed line indicates the mean score",
       x = "Overall Score", y = "Frequency") +
  theme_minimal()

# B. Correlation Matrix
# How do the numeric variables relate to each other?
# We select only numeric columns for correlation
numeric_vars <- df %>% select(food_bev, service, money, interior, overall)
cor_matrix <- cor(numeric_vars)

# Plotting the heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", # Add coefficient numbers
         tl.col = "black", tl.srt = 45, # Text label rotation
         title = "Correlation Heatmap: What drives Overall Satisfaction?",
         mar = c(0,0,1,0))

# C. Boxplot: Satisfaction by Store
# Are some stores significantly better than others?
ggplot(df, aes(x = store, y = overall, fill = store)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Overall Satisfaction by Store",
       x = "Store Location", y = "Satisfaction Score") +
  theme_minimal() +
  theme(legend.position = "none")

# ==============================================================================
# 5. STATISTICAL HYPOTHESIS TESTING
# ==============================================================================

# A. T-Test: Does Gender (Sex) affect Satisfaction?
# H0: There is no difference in overall satisfaction between sex groups.
t_test_result <- t.test(overall ~ sex, data = df)
print("--- T-Test Results (Sex vs Overall) ---")
print(t_test_result)

# B. ANOVA: Is there a significant difference between Stores?
# H0: All stores have the same average satisfaction.
anova_model <- aov(overall ~ store, data = df)
print("--- ANOVA Results (Store Differences) ---")
summary(anova_model)

# If ANOVA is significant (p < 0.05), which stores are different?
# Tukey's HSD (Honest Significant Difference) test
tukey_result <- TukeyHSD(anova_model)
print("--- Tukey HSD (Pairwise Store Comparison) ---")
# viewing first few significant differences
head(tukey_result$store)

# ==============================================================================
# 6. REGRESSION MODELING (Driver Analysis)
# ==============================================================================
# Goal: Predict 'overall' satisfaction based on specific attributes.
# Which factor is the strongest driver? (Food, Service, Money, or Interior?)

model <- lm(overall ~ food_bev + service + money + interior, data = df)

# View the Model Summary
print("--- Multiple Linear Regression Results ---")
summary(model)

# Interpretation Visualization: Coefficient Plot
# Visualizing which factors have the biggest impact (Estimate value)
tidy_model <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Remove intercept for cleaner plot

ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() + # Make it horizontal
  labs(title = "Key Drivers of Customer Satisfaction",
       subtitle = "Higher estimate = Stronger impact on Overall Score",
       x = "Variable", y = "Coefficient Estimate") +
  theme_minimal()